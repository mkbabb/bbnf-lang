# GN-0 — THE GREAT ARCHIVAL & THE x86 PURGE

*The opening act of the greenfield novum: the legacy totality is committed to the historical
record behind tags, the working tree is emptied of everything that is not the greenfield's
inheritance, and the x86 abrogation is made structural — a build failure, not a policy.*

---

## 1. Charter

### 1.1 Mission

GN-0 disposes of the repository itself. Three prior campaigns froze plans without a line
landing; this tranche executes the one act every one of them deferred. Two indivisible works:

1. **The archival.** Master's entire git-tracked state is preserved behind one umbrella tag
   plus nine navigational tags plus one branch; then the legacy tree — the root `crates/`
   workspace, `restart/`, `audit/`, `playground/`, `wasm/`, `server/`, `extension/`, the
   sibling shelf, the stale worktrees, the regenerable build churn — is removed from the
   working tree. Roughly 185G of pure build churn and ~17G of source-shaped legacy reclaim,
   all recoverable by `git show <tag>:path`. The greenfield inheritance — `skinny/` and the
   general-infra crates it path-depends on — is **retained**, not re-founded; the re-founding
   is GN-1's work.
2. **The x86 purge.** The abrogation, normative both repos, is completed and made
   enforceable. The legacy x86 kernel bodies (`crates/simd-scan/{avx2,avx512,wasm}.rs`, 952
   lines the PB-2 probe proved deletable but never landed) die with the archival. The
   **survivor tree** carries its own x86 residue — a dead `avx512bw` dispatch lever, stale
   competitor-comparison plumbing — which this tranche excises, then installs four enforcement
   layers so x86 cannot re-enter the greenfield.

The freeze note stands: GN-0's substance is not speculation. The archive mechanics are the
SK-V22 sequence, measured and specified (`restart/skinny/tranches/sk-v22/SPEC.md` §2). The x86
purge is the PB-2 probe (`restart/skinny/tranches/sk-v23/prototypes/PROTO-PB-2.md`), PROVEN on
the live surface with a zero-insertion regen diff. This document is the landing sequence.

### 1.2 Precept bindings

Bound by number from the SK-V23 precept codex
(`restart/skinny/tranches/sk-v23/research/07-precept-codex.md`):

- **42** — *tags-before-removals, always, order normative.* W1 (the tag set) precedes every
  removal; no `git rm` executes before the full tag set exists and every tag resolves.
- **4** — *abrogate before patch.* The legacy tree is not migrated, refactored, or
  feature-flagged; it is deleted wholesale behind tags. The archival IS abrogation.
- **6** — *no legacy code: delete, never rename-and-hide or feature-flag-and-keep.* The empty
  `skinny/crates/simd-scan/src/` stub, the `avx512bw` dead lever, the `asmjson_avx512`
  competitor columns — deleted, not gated.
- **2** — *no workarounds, stubs, disabled gates.* No transitional-allowlist limbo survives
  GN-0: the x86 residue named by PB-2 as "the DATED TB1.W3 transitional-allowlist entry" is
  **emptied here**, not carried forward.
- **7** — *no silent deferrals.* `skinny/`'s own archival is deferred to a **named**
  destination — GN-1's close, once port-in completes (§2). The evidence-corpus rescue (W2)
  exists precisely to make the `restart/` removal non-lossy.
- **14 / 15** — *no god modules; directory modules over flat siblings.* The general-infra
  re-home lands `egraph`/`egraph-derive`/`csp-solver` under a `crates-infra/` tier (per the
  general-infra-crates precept), not stuffed into a domain crate; the token gate is a
  `scripts/`-owned instrument, not an inline shell fragment.
- **53** — *generated files are fresh-regen output, idempotent on disk.* The x86 purge's
  layer-4 proof is a `regen` that produces a byte-identical tree minus x86 stamps — zero
  insertions.
- **19** — *a hard gate is valid only with an artefact.* Every gate below cites a build,
  a deletion-proof, a planted-phantom fire, or a resolvable tag — never a grep alone.
- **8 / 37 / 40** — *Y.13: substrate lands with its consumer in the same wave.* The token
  gate's consumer is the CI job that runs it (W4); the workspace lint's consumer is the
  workspace build; neither the gate nor the lint lands dormant.

### 1.3 Refutation guards (SPEC §0 laws constraining this tranche)

GN-0 does **no** performance work; the §0 performance laws bind it by prohibition, not by
pursuit:

- **No shim perf claims / warm benches forbidden.** GN-0 quotes **no** benchmark number. The
  x86 purge is a **non-perf** change: on aarch64 every deleted arm was `#[cfg(x86_64)]` — dead
  on the target — so the regenerated tree is byte-identical to the pre-purge tree on the NEON +
  scalar arms (PB-2's zero-insertion regen diff is the proof, and it is a *stronger* guarantee
  than a paired bench, which would measure only host load). Any claim that the purge *improved*
  a lane is refuted by construction and forbidden here.
- **Digit-scan SIMD / block-scheduling / fold-over-cursor / tape-navigation-as-get** — none
  are touched; these are GN-2/GN-4 concerns. GN-0 that emitted a single SIMD kernel or value
  lane would be out of charter.
- The one perf obligation GN-0 *hands forward*, not discharges: the survivor tree's §0 numbers
  become GN-1's non-regression floor. GN-0 must not perturb a hot path — hence the byte-identity
  requirement on the purge, and the retention (not re-founding) of `skinny/`.

### 1.4 Supersessions

- Supersedes **SK-V22 GW0** in one respect only: skinny does **not** graduate to the root at
  archival time. GW0 re-founded skinny's `[workspace]` as the root manifest in the same wave
  as the removal; GN-0 **retains** `skinny/` in place and defers the re-founding to GN-1.
  Rationale: the port-in applies fourteen patch.diffs during the loom; re-founding before they
  land would force the archival and the port to share one wave's blast radius.
- Supersedes **SK-V23 TB1's** archival-first campaign ordering: the codex formalized first
  (this freeze), the archival executes first (this tranche) — the owner's "once tranches are
  formalized" is satisfied.
- The umbrella tag renames `pre-graduation-2026-07-09` → **`pre-greenfield-2026-07`**. The nine
  navigational tags and the `archive/legacy-crates` branch carry verbatim (§4, W1).

---

## 2. Interlocks

| Direction | Counterpart | Binding |
|---|---|---|
| depends-on | *(none)* | GN-0 is the first tranche; it opens on the frozen SPEC alone. |
| **feeds** | **GN-1.W1** (the loom) | GN-0.W2 re-homes `egraph`/`egraph-derive`/`csp-solver` to `crates-infra/`; GN-1 port-in adopts that tier as the greenfield's infra members. `skinny/`'s retained state is GN-1's port source; skinny archives at GN-1 close. |
| **feeds** | **every downstream GN wave** | GN-0.W4's four-layer x86 enforcement is the anti-regrowth guard under which GN-2's kernel plane and every emitter change operate. A GN-2 wave that stamps an x86 arm fails GN-0.W4's token gate in CI. |
| **feeds** | **GN-1 non-regression** | The retained (un-perturbed) survivor tree carries the §0 numbers GN-1 re-measures against. |

**The hard-ordering hazard (SEAM-EGRAPH, carried verbatim from SK-V22 §3.1).**
`skinny/Cargo.toml:36` path-depends on `../crates/egraph` and `:37` on `../crates/csp-solver`;
parse-that's `.cargo/config.toml` patch table resolves `egraph` through this same
`bbnf-lang/crates/` on-disk home. **Removing `crates/` before the re-home bricks two trees'
builds at once.** The normative sub-order inside GN-0:

> **W2 (egraph/csp-solver re-home + skinny repoint) → owner-placed BBNF-PT-G repoint
> acknowledgment → W3 (`crates/` removal).**

**Owner gates (SPEC §3 checkboxes; defaults proceed absent veto):**

1. **BBNF-PT-G letter placement** — the additive coordination letter carrying parse-that's
   `egraph` patch-table repoint (PT-N/PT-R.5 lineage). *Placement law unchanged: only the owner
   or a parse-that-side session places it.* GN-0.W3's `crates/` removal is gated on the
   acknowledgment that parse-that has repointed (or that parse-that's build no longer resolves
   `egraph` through the archiving path). A bbnf agent never touches parse-that.
2. **`release.yml` aarch64-only (bbnf-lang)** — a GN-0.W4 x86-purge deliverable (SPEC §2 GN-0
   "all-x86 CI"), consumed in W4; NOT owner checkbox §3.2, which is parse-that-specific and homed
   solely at PT-2.W5.
3. **GN-0 timing** (SPEC §3.3) — archival executes immediately post-freeze; default yes.

---

## 3. Wave manifest

| id | name | scope (one line) | LOC (+new / −del) | gate | hard cap |
|---|---|---|---|---|---|
| GN-0.W1 | **The Tag Set** | Cut umbrella `pre-greenfield-2026-07` + 9 navigational tags + `archive/legacy-crates` branch; nothing removed | +0 / −0 (refs only) | **born-verified** (every tag resolves) | 20 min |
| GN-0.W2 | **The Rescue & The Re-Home** | 6 rescue checks incl. sk-v24 evidence corpus → `docs/tranches/sk-v24/evidence/`; `egraph`/`egraph-derive`/`csp-solver` → `crates-infra/`; skinny repoint | +~35 / −~4 | **born-RED** (evidence complete + skinny builds on re-homed infra) | 30 min |
| GN-0.W3 | **The Removals & The New Shape** | `git rm` the archived trees (crates/ gated on W2+letter); worktree purge; sibling shelf; `data/` trim; skinny **retained** | +~10 / −(tree removal) | **born-RED** (top-level is the new shape; reclaim measured) | 30 min |
| GN-0.W4 | **The x86 Purge & The Four Layers** | Survivor-tree x86 excision + structural token gate + workspace lints + aarch64 CI assertion + `release.yml` aarch64-only + emitter-stamp proof | +~95 / −~50 | **born-RED** (token gate FIRES on planted stamp, then green; regen zero x86 insertions) | 40 min |
| GN-0.W5 | **The Reckoning** *(challenge)* | Adversarial re-run of W1–W4 gates from a cold checkout; the research/09 70-row audit; the counterexample battery | +~10 / −0 | **born-RED** (a planted defect in each of 4 classes is caught) | 30 min |

LOC is dominated by removal; the archival's true magnitude is measured in reclaim (GB), not
lines (§6). Net-new is confined to enforcement instruments and re-home plumbing.

---

## 4. Per-wave specification

### GN-0.W1 — The Tag Set

**Objective.** Make the entire legacy totality permanently recoverable before a single byte is
removed. Precept 42 is absolute: no removal in any later wave may begin until this wave's gate
reads green.

**File-level plan.** No source files change; this wave writes git refs only.

- Umbrella tag over master's entire tracked state: `pre-greenfield-2026-07`.
- The nine navigational tags (verbatim from SK-V22 §2.1 step 1, `restart/skinny/tranches/sk-v22/SPEC.md:183-188`):
  branch `archive/legacy-crates` **+** tag `legacy-totality-final`; `restart-doclayer-final`;
  `pre-restart-corpus-final`; `restart-audit-2026-05-03`; `legacy-wasm-final`;
  `vscode-ext-final`; `playground-spa-final`; `restart-archive-gen1`; and
  `sk-v16-w6-lazylock-experiment` on the one worktree commit not reachable from master,
  `f2fe49bbc`.
- **Amendment (sk-v24):** the umbrella already captures `skinny/` (it covers the whole tracked
  state), so no separate skinny tag is cut here. `skinny/` is retained live; its archival is
  GN-1's close (a named destination — precept 7).

**Seam citations.** SK-V22 §2.1 (the executable sequence); §2.2 (`restart/skinny/tranches/sk-v22/SPEC.md:214-221`,
the reclaim figures); recon `git tag` at open returns only `v1.0.5` — the SK-V21/22
dispositions were never executed, so this wave actually creates the refs.

**Gate (born-verified).** Command and thresholds:

```
git tag | grep -cE '^(pre-greenfield-2026-07|legacy-totality-final|restart-doclayer-final|pre-restart-corpus-final|restart-audit-2026-05-03|legacy-wasm-final|vscode-ext-final|playground-spa-final|restart-archive-gen1|sk-v16-w6-lazylock-experiment)$'
# MUST equal 10
git rev-parse --verify archive/legacy-crates          # exit 0
for t in <the 10 tags>; do git show "$t:Cargo.toml" >/dev/null 2>&1 || exit 1; done   # exit 0
git show sk-v16-w6-lazylock-experiment | head -1 | grep -q f2fe49bbc   # the off-master commit tagged
```
**Failure semantics:** a count ≠ 10, an unresolvable tag, or a branch that does not point at
master's tip halts the entire tranche — no later wave may open. (A gate that cannot fail is a
defect: this one fires on any missing or mis-pointed ref, verified by planting a deliberately
absent tag name in the check list during W5.)

**Consumer in this wave.** The tags' consumer is W2's evidence-authoring reads and W3's
removal proof, both of which resolve archived paths via `git show <tag>:path`; W1 is not a
document-only wave — its artefact is the resolvable ref set every downstream removal depends on.

---

### GN-0.W2 — The Rescue & The Re-Home

**Objective.** Nothing irreplaceable is lost when the working tree empties, and the cross-repo
`egraph` coupling is dissolved before `crates/` can be removed.

**File-level plan.**

*The six rescue checks (five carried from SK-V22 §2.1 step 2, one new — the load-bearing
sk-v24 amendment):*

1. **[NEW] The evidence-corpus re-home.** Copy `restart/skinny/tranches/sk-v24/` in full —
   `SPEC.md`, `HANDOFF.md`, `CAMPAIGN-STATE.md`, `ROUND-ZERO.md`, `research/R1-R7`, and
   `prototypes/PROVE-01..14/{REPORT.md, patch.diff, VERIFY.md}` — into
   **`docs/tranches/sk-v24/evidence/`**, BEFORE `restart/` is removed in W3. The fourteen
   `patch.diff` files are the code of record; the campaign's whole certified swath is in them
   and the twelve `VERIFY.md` adversarial certifications. Losing the patches is losing the
   campaign. `docs/tranches/sk-v24/` (this codex + its evidence) is the durable home that
   survives the archival.
2. Skim the `bbnf-lang-json-threaded-fork` dirty +102-line diff once, then discard — it patches
   a file already slated for removal (`restart/skinny/tranches/sk-v22/SPEC.md:189-191`).
3. **`data/` provenance trim.** Enumerate the corpora the retained benches/oracles consume —
   `data/{json,css,sheets,bbnf}` canonical (twitter/canada/citm JSON; bootstrap/tailwind/
   material/animate CSS; the `test-fixtures` loader): **KEEP**; generated stress corpora:
   delete in W3. (`data/` is *not* a wholesale removal — the trim keeps what the survivor tree
   reads; SK-V22 §2.1 note (iii).)
4. **`grammar/` retention.** Root `grammar/` is the canonical fleet; skinny carries only
   `json.bbnf`. Root `grammar/` is **retained** for GN-1's port-in (do not archive it here) —
   this reverses SK-V22's "merge into one" only in timing: the merge is GN-1's, the retention
   is GN-0's.
5. **No live cross-reference into the archiving tree.** Verify no retained oracle/parity/test
   in `skinny/` reads a path under the archiving legacy tree (the SOTA references —
   serde_json, sonic-rs, lightningcss — survive as external deps, not legacy `crates/`).
6. **Docs disposition.** `docs/tranches/sk-v24/` and `docs/precepts` (the live submodule) are
   retained; the legacy tranche-letter corpus (`docs/tranches/{AA..BD, W-Z, archive,
   meta-audit, PLAN-INPUT-*}`) is tag-captured (W1) then removed in W3.

*The re-home (SEAM-EGRAPH, the hard-order precondition to W3's `crates/` removal):*

- `git mv crates/egraph → crates-infra/egraph`, `crates/egraph-derive → crates-infra/egraph-derive`,
  and the `crates/csp-solver` vendor mirror → `crates-infra/csp-solver` (its canonical home
  stays csc411; the `sync-csp-solver-vendor.sh` mechanism carries — the re-vendor commits at
  HEAD, `fe3e615d3` and kin, are that sync and are preserved). Add `scripts/sync-egraph-vendor.sh`
  only if egraph acquires an out-of-tree canonical home; else it is a plain workspace member.
- Repoint `skinny/Cargo.toml:36-37`: `egraph = { path = "../crates-infra/egraph" }`,
  `csp-solver = { path = "../crates-infra/csp-solver" }`, and add the `egraph-derive` entry the
  derive macro needs.
- The parse-that-side repoint (`egraph` patch table from `…/crates/egraph` to
  `…/crates-infra/egraph`) rides the **BBNF-PT-G letter** — owner/parse-that-placed only; W2
  authors the letter's repoint note but does not touch parse-that.

**Module structure (colocation edict).** `crates-infra/` is the general-infra tier — a
directory-module sibling to domain `crates/`, per the general-infra-crates precept; egraph does
not re-enter domain space. Vendor sync scripts live in `scripts/`, one file per mirror, not a
`common.sh` kitchen sink.

**Gate (born-RED — cannot read green until the corpus is complete AND skinny builds on the
re-homed infra):**

```
test -f docs/tranches/sk-v24/evidence/SPEC.md
[ "$(find docs/tranches/sk-v24/evidence/prototypes -name patch.diff | wc -l)" -eq 14 ]
[ "$(find docs/tranches/sk-v24/evidence/prototypes -name VERIFY.md  | wc -l)" -eq 12 ]
# byte-identity of every rescued patch vs origin (zero drift):
for p in $(cd restart/skinny/tranches/sk-v24/prototypes && find . -name patch.diff); do \
  diff "restart/skinny/tranches/sk-v24/prototypes/$p" \
       "docs/tranches/sk-v24/evidence/prototypes/$p" || exit 1; done   # exit 0
# skinny resolves infra from crates-infra, not legacy crates/:
cd skinny && cargo metadata --format-version 1 2>/dev/null | grep -c '/crates/egraph"'   # MUST be 0
cargo check -p passes --profile ax-iter    # exit 0, egraph resolved via crates-infra
```
**Failure semantics:** a patch count ≠ 14, a VERIFY count ≠ 12, any diff drift, or a skinny
build still resolving legacy `crates/egraph` blocks W3. The born-RED colour is real: the
metadata grep is nonzero and the corpus incomplete at wave open.

**Consumer in this wave.** The re-homed `crates-infra/egraph` is consumed *in this wave* by
`cargo check -p passes` (its live dependant); the evidence corpus is consumed by W5's
byte-completeness audit and by every future session resurrecting the campaign from
`docs/tranches/sk-v24/`.

---

### GN-0.W3 — The Removals & The New Shape

**Objective.** The working tree becomes the greenfield's antecedent: `skinny/` + `crates-infra/`
+ `grammar/` + trimmed `data/` + `docs/` + root config, and nothing else. Everything removed
is recoverable behind W1's tags.

**File-level plan (removals — all `git rm`, never `rm`; PB-2 finding 5: `git ls-files` reads a
plain-`rm` file live until staged, so the deletion and the archival commit land atomically):**

- **`crates/`** — the legacy domain workspace (core, ir, analysis, lsp, ser, gorgeous,
  bootstrap, bbnf-path, bbnf-path-ts, simd-scan with its x86 bodies). **Gated on W2's egraph
  re-home AND the owner-placed parse-that repoint acknowledgment.** This is the single
  highest-consequence removal; its close verifies both repoints resolved before it runs.
- **`restart/`** — gated on W2 rescue check 1 (evidence corpus present under
  `docs/tranches/sk-v24/evidence/`).
- `restart-archive-2026-05-04/`, `audit/`, `playground/` (archives outright — the dirty
  `M playground/vite.config.ts` and `?? playground/test-results/` are dispositioned by
  removal), `wasm/`, `server/`, `extension/`, legacy `docs/tranches/{AA..BD, W-Z, archive,
  meta-audit, PLAN-INPUT-*}`, root `Makefile`, `package.json` + `package-lock.json`,
  `node_modules/`, `.profiles/`, `.profile-target/`, `.playwright-mcp/`.
- `scripts/` — **trimmed**, not removed: surviving profiling/worktree/hooks tooling + the
  vendor-sync scripts stay; legacy generators go.
- `data/` — **trimmed** per W2 check 3 (generated stress deleted; canonical corpora kept).
- Worktree purge: `git worktree remove --force` every entry under `.claude/worktrees/`
  (the `wf_386900ed-462-N` prover trees, `wf_55cb3d1f-cb8-*`, the `agent-*` trees) +
  `git worktree prune`; delete the sibling shelf `/Users/mkbabb/Programming/bbnf-lang-{json-threaded-fork,
  skv15-profile-*, skv5-index-validate, skv6-candidate7..10-base}` via `rm -rf` (untracked
  siblings, not worktrees).
- **Retained:** `skinny/`, `crates-infra/`, `grammar/`, trimmed `data/`, `docs/` (sk-v24 +
  precepts), `.github/`, `.cargo/`, `.gitmodules`, `LICENSE`, `README.md`, `rust-toolchain.toml`,
  `.rustfmt.toml`, `Cargo.lock`. The root `Cargo.toml` (legacy workspace manifest) is
  **retained as a stub pointing only at `crates-infra/` + `skinny` members** so the tree builds
  through the archival; GN-1 re-founds it. (This is the minimum manifest, not a workaround —
  it names only surviving members.)

**Seam citations.** SK-V22 §2.1 step 4 (removals) + step 5 (re-found root) + §2.3 (the clean
top-level shape); §2.2 (`:214-221`) for the reclaim arithmetic.

**Gate (born-RED):**

```
[ "$(git ls-files crates/ | wc -l)" -eq 0 ]          # legacy crates gone
[ "$(git ls-files restart/ | wc -l)" -eq 0 ]         # restart gone (post evidence-rescue)
test -d skinny && test -d crates-infra && test -d grammar   # inheritance retained
[ "$(git worktree list | wc -l)" -eq 1 ]             # only the main tree remains
grep -rl 'restart/skinny\|\.\./crates/' skinny/ crates-infra/ | wc -l    # MUST be 0 live refs into removed trees
cargo build --workspace --profile ax-iter            # exit 0 — the trimmed tree builds green
```
Plus a **reclaim measurement** (directional, not a pass/fail perf number): `du -sh` of the repo
before and after; expected ≥ ~180G reclaimed (worktrees ~90G + skinny/root `target/` ~90G +
sibling shelf + `.profile*`), recorded in the wave close, per SK-V22 §2.2.

**Failure semantics:** any live reference from the retained tree into a removed path, a
non-empty `crates/`/`restart/` ls-files, or a broken workspace build fails the wave and is
reverted (the tags make revert lossless). The `crates/` removal specifically refuses to run if
W2's metadata grep is nonzero — the ordering hazard is a hard interlock, not a warning.

**Consumer in this wave.** The re-founded top-level is consumed *in this wave* by
`cargo build --workspace` — the trimmed tree must build, which is the liveness proof that the
removal severed no live edge.

---

### GN-0.W4 — The x86 Purge & The Four Layers

**Objective.** Complete the abrogation on the survivor tree and make x86 regrowth a mechanical
build failure across four enforcement layers.

**File-level plan — the survivor-tree excision (the residue PB-2 named but left as a "dated
transitional-allowlist entry"; GN-0 empties it, precept 2):**

- **`skinny/crates/passes/src/lib.rs`** — the dead `avx512bw` dispatch lever.
  - `:370` delete the `pub avx512bw: bool` field from `TargetFeatures`.
  - `:380` delete the `avx512bw: cfg!(all(target_arch = "x86_64", target_feature = "avx512bw"))`
    host-detection line.
  - `:960` `admits_collapsed_stage` currently reads `target.avx512bw && matches!(rule_ir.expr,
    BackendExpr::Entry(_))`. On aarch64 `avx512bw` is *always false* — the collapsed-stage emit
    path is dead substrate. **Transposition, not deletion** (precept 3): rekey the predicate to
    the backend-agnostic author signal already present in the struct,
    `target.collapsed_stage_author_declared`, so the emit capability survives behind a grammar
    signal instead of an x86 cfg. (Whether collapsed-stage *earns* its place is deferred to
    **GN-5.W5**, the challenge wave that adjudicates `CollapsedStage` as consumer-or-retire;
    GN-0's mandate is only to sever it from x86. W5 flags it as candidate dead substrate for that
    adjudication.)
  - `:1944` delete the `avx512bw: true` test literal.
- **`skinny/crates/codegen/src/lib.rs:1185`** — delete the `avx512bw: false` fixture literal.
- **`skinny/crates/bbnf-bench/src/{report.rs, bin/gate.rs}`** — the `asmjson_avx512_mbps`
  competitor-comparison column (`report.rs:133,5478,6345,9434`; `gate.rs:2789,3853`) and the
  `"x86_64 AVX side evidence"` admission string (`report.rs:1678,10578`) are stale x86-baseline
  plumbing; the campaign benches against aarch64 sonic-rs/simdjson. Purge the column and its
  string. (This is the one judgment call: the alternative — scope the token gate to exclude
  competitor identifiers — is rejected as a workaround; the column is dead and deleted.)
- **`skinny/crates/simd-scan/src/`** — the empty stub directory (recon: `ls` returns nothing).
  Delete the crate husk and its workspace member entry.

**File-level plan — the four enforcement layers (the SPEC §4 / HANDOFF §5 abrogation, both
repos):**

1. **Layer 1 — structural token gate.** `scripts/x86-token-gate.sh` (new, directory-owned
   instrument): greps `skinny/crates/**/src` + generated files for
   `avx2|avx512|__m256|_mm256|_mm_|pclmul|bmi2|pext|target_arch = "x86`. Scoped to **compiled
   sources only** and path-excluding `**/archive/**` and `docs/**` (PB-2 findings 1-2: the
   `crates/gorgeous cfg(not(target_arch="wasm32"))` mimalloc allocator gate and the archived
   `json-prototype` AVX bodies must not trip it — but gorgeous archives in W3, so the sole
   surviving allowlist entry is any retained `cfg(not(wasm32))` allocator selection, narrowed to
   non-SIMD). Zero hits on the abrogated surface.
2. **Layer 2 — workspace lints.** Root `Cargo.toml` gains `[workspace.lints.rust]` with
   `unexpected_cfgs` set to `deny`, declaring the allowed cfg universe via `check-cfg` as
   `{aarch64, neon, scalar, the wasm32 allocator gate}` only — so any `target_arch = "x86_64"`
   cfg an emitter or a hand edit introduces becomes an `unexpected_cfgs` **deny** at compile,
   caught before CI. Each surviving member sets `[lints] workspace = true`.
3. **Layer 3 — aarch64 CI assertion + `release.yml` aarch64-only.** `.github/workflows/ci.yml`
   gains a first step `test "$(uname -m)" = arm64 -o "$(uname -m)" = aarch64` (and moves off
   `runs-on: ubuntu-latest` to a macOS-arm64 / aarch64 runner), then runs
   `scripts/x86-token-gate.sh` as a required step. `.github/workflows/release.yml:16-32` drops
   the x86 targets (`x86_64-unknown-linux-gnu`, `x86_64-apple-darwin`, `x86_64-pc-windows-msvc`)
   — aarch64 matrix only (part of the SPEC §2 GN-0 "all-x86 CI" purge; NOT owner checkbox 2, which
   governs parse-that's `release.yml` and is homed at PT-2.W5). `bench-iai.yml` likewise moves off
   the x86 runner or is archived if its consumer (legacy iai benches) died in W3.
4. **Layer 4 — the emitter stops stamping.** Prove the codegen decision plane can no longer
   emit an x86 arm: `cargo xtask regen` produces a tree whose `git diff --stat` shows **zero
   x86-token insertions** (PB-2's zero-insertion signature — the NEON+scalar arms round-trip
   byte-identical; only x86 arms are absent because the `avx512bw` lever is gone). The existing
   `regen --check` drift gate then makes any re-introduction a build failure.

**Module structure (colocation edict).** The token gate is a standalone `scripts/` instrument,
not an inline CI heredoc; the lint config is workspace-level; the CI assertion is a named step,
not buried in a run block.

**Gate (born-RED — the planted-phantom fire is the artefact, precept 19):**

```
# Layer 1 FIRES on a plant, then reads green:
printf '\n// avx2 canary\nfn _x(){}\n' >> skinny/crates/passes/src/lib.rs
scripts/x86-token-gate.sh && echo GATE_FAILED_TO_FIRE && exit 1   # MUST exit nonzero
git checkout skinny/crates/passes/src/lib.rs
scripts/x86-token-gate.sh                                          # now exit 0
# Layer 2: an injected x86 cfg is a deny:
cargo check -p passes --profile ax-iter                           # exit 0 clean tree
# Layer 4: regen introduces zero x86 stamps:
cargo xtask regen && [ "$(git diff --stat | grep -cE 'avx2|avx512|x86_64')" -eq 0 ]
# survivor grep floor (allowlist = the one wasm32 allocator gate, if retained):
[ "$(grep -rcE 'target_arch = .x86' skinny/crates/*/src | awk -F: '{s+=$2} END{print s}')" -le 0 ]
```
**Failure semantics:** if `scripts/x86-token-gate.sh` returns 0 on the planted canary, the gate
is a defect (cannot fail) and the wave does not close; if regen introduces any x86 insertion,
layer 4 is unproven and the wave fails.

**Consumer in this wave.** The token gate's consumer is the `ci.yml` step that invokes it *in
this wave* (layer 3 wires it); the workspace lint's consumer is the `cargo check` build. No
enforcement lands dormant — the Y.13 four-hop closes: emitter fact (no x86 lever) → lint/gate
selection → CI reads the const → build fails on regrowth.

---

### GN-0.W5 — The Reckoning *(the challenge wave)*

**Objective.** Adversarially re-measure GN-0's own claims. A non-author auditor re-runs every
W1–W4 gate from a cold checkout, walks the research/09 pre-map, and attempts to plant a defect
in each of four classes; the wave closes only when all four plants are caught.

**File-level plan.** Read-only audit + one output ledger appended to
`docs/tranches/sk-v24/evidence/GN-0-audit.md` (the audit is the artefact; no source changes
except reverting any residue the audit surfaces).

**The four counterexample classes (each a NAMED plant that must be caught):**

1. **A corrupted tag.** Delete `restart-audit-2026-05-03` locally and re-run W1's gate — it must
   report count 9 ≠ 10 and halt. Counterexample: *"an archival that proceeds with an
   unresolvable tag has lost a surface irrecoverably."*
2. **A lossy evidence rescue.** Truncate one rescued `patch.diff` to zero bytes and re-run W2's
   `diff` loop — it must fire. Counterexample: *"PROVE-07's fused-sink patch is the direct-to-
   struct code of record; a zero-byte rescue silently loses the win the whole campaign
   certified."*
3. **A build escape.** Plant `use egraph;` resolving through a path into a removed tree and
   re-run W3's `grep -rl '\.\./crates/'` — it must be nonzero. Counterexample: *"skinny's
   `passes` crate path-depended on `../crates/egraph`; a removal that missed the repoint bricks
   the build and, transitively, parse-that's."*
4. **An x86 regrowth.** Plant an `avx2` stamp in a generated file and run `regen --check` +
   `scripts/x86-token-gate.sh` — both must fail. Counterexample: *"the emitter that re-learns to
   stamp `nospace_bitmap_64_avx2` (PB-2's named ancestor body) re-imports the whole abrogated
   surface one arm at a time."*

**The research/09 audit (the carried "W-final" residual).** Walk all 70 rows of
`restart/skinny/tranches/sk-v23/research/09-ledger-rehome.md`; for each, assert its GN-0-relevant
disposition holds and record the owning GN wave. Specifically re-affirm the three flagged
orphans by name (precept 7 — no silent standing-deferral):

- **N5** (dead/lying oracle surfaces + W3C rekey) — folded to GN-1's oracle hygiene, named here
  as re-homed, not lost.
- **Lock-5 (TS/WASM backends) + TS path runtime** — authority unchanged, re-verified, owned by
  GN-4/GN-6 (the value fabric's TS seed + the shopwindow), named explicitly.
- **N20's revival door** (structural-bitmap pre-scan, "returns only via a measured win through
  the cost table") — bound to GN-2's kernel plane as the general-purpose door, distinct from the
  CSS-specific tokenize-once door, so a future session does not rediscover the twice-buried
  architecture a third time.

Plus: flag the `admits_collapsed_stage` rekey (W4) as candidate dead substrate for **GN-5.W5**'s
residue hunt, which adjudicates `CollapsedStage` as consumer-or-retire — a dormant emit path is
exactly the prototype residue the totality framing names as a defect.

**Gate (born-RED).** All four plants caught (each command returns the failing status); the
70-row walk produces a disposition for every row (zero rows without an owning GN wave or a
"closed-historical" mark); the reclaim figure from W3 is independently re-measured within 10%.
**Failure semantics:** any plant that slips through means the corresponding W1–W4 gate is a
defect and that wave re-opens; an orphaned research/09 row means a silent deferral and blocks
GN-0 close.

**Consumer in this wave.** The audit ledger is consumed by the GN-1 dispatch (it inherits the
re-homed infra, the retained corpus, and the confirmed enforcement); the four caught plants are
the standing proof that GN-0's gates are falsifiable.

---

## 5. LOC ledger

| Wave | +new | −deleted | Net | Note |
|---|---:|---:|---:|---|
| W1 | 0 | 0 | 0 | git refs only |
| W2 | ~35 | ~4 | +31 | evidence copy (files, not counted) + skinny repoint (~4 edited) + sync script (~15) + letter note (~16) |
| W3 | ~10 | *(tree)* | — | minimal root `Cargo.toml` stub (~10); removals are tree-level, tag-preserved, not LoC-counted |
| W4 | ~95 | ~50 | +45 | token gate (~45) + lints (~18) + CI edits (~20) + release.yml (net −); survivor x86 excision (~50 across passes/codegen/bench) |
| W5 | ~10 | 0 | +10 | audit ledger |
| **Total** | **~150** | **~54** *(survivor src)* | **+~96** | **plus** the archival removal: ~185G build-churn reclaim + tag-preserved legacy source (measured in GB, not LoC) |

The tranche is, in code terms, *additive* (enforcement + re-home plumbing); its dominant effect
is subtractive at the file-tree level and recorded as reclaim.

---

## 6. Exit criteria

GN-0 closes when:

1. W1's ten tags + `archive/legacy-crates` branch all resolve (`git show` sampled per surface).
2. `docs/tranches/sk-v24/evidence/` holds the complete corpus — 14 `patch.diff`, 12 `VERIFY.md`,
   all byte-identical to origin.
3. `crates/egraph`/`egraph-derive`/`csp-solver` live at `crates-infra/`; `skinny/` builds
   against them with zero path escape into the removed `crates/`; the BBNF-PT-G repoint
   acknowledgment is placed (owner gate).
4. The working tree is the new shape: `skinny/` + `crates-infra/` + `grammar/` + trimmed
   `data/` + `docs/` + root config; `git ls-files crates/ restart/` returns empty; the workspace
   builds green.
5. The four x86 enforcement layers are live and each falsifiable: the token gate FIRES on a
   plant then reads green; the lint denies an injected x86 cfg; the aarch64 CI assertion is
   wired; `regen` introduces zero x86 insertions.
6. W5's four plants are all caught; the research/09 70-row walk is complete with no orphan.
7. `skinny/`'s numbers are un-perturbed — GN-0 quotes no benchmark; the byte-identity of the
   regen diff is the only performance-adjacent artefact, and it is a hygiene proof, not a claim.

Owner gates satisfied: BBNF-PT-G letter placed (checkbox 1); timing immediate-post-freeze
(checkbox 3, default). The bbnf-lang `release.yml` aarch64-only cut landed in W4 as part of the
SPEC §2 GN-0 x86 purge (not an owner gate); owner checkbox 2 governs parse-that's `release.yml`,
homed at PT-2.W5.

---

## 7. Risks with named counterexamples

- **The two-tree brick.** *Counterexample:* `crates/` is removed before parse-that repoints;
  parse-that's next `cargo build` cannot resolve `egraph` and its active value.js/keyframes
  sessions break. *Guard:* W3's `crates/` removal is a hard interlock on W2's metadata grep
  (== 0) AND the owner-placed repoint acknowledgment; the removal command refuses to run
  otherwise.
- **The lost patch.** *Counterexample:* `restart/` is removed and later a session needs
  PROVE-03's fused NEON string-scan `patch.diff` (the twitter parse_tape 0.898× win) — it is
  gone, and the win must be re-proven from scratch. *Guard:* W2 rescue check 1 + W5 plant 2
  (zero-byte truncation must fire).
- **The un-killable canary.** *Counterexample:* the archived `docs/tranches/AZ-II/.../json-
  prototype/src/simd.rs` (PB-2 finding 2) with live `nospace_bitmap_64_avx2` bodies trips the
  token gate forever after archival. *Guard:* layer 1 path-excludes `docs/**` and `**/archive/**`
  and scopes to compiled sources; W5 confirms the gate reads green on the retained tree.
- **The dead-substrate carry.** *Counterexample:* the `avx512bw` lever is deleted but the
  `admits_collapsed_stage` emit path it gated becomes silently unreachable — dead substrate
  smuggled past the abrogation as a rekey. *Guard:* W4 rekeys to `collapsed_stage_author_declared`
  (author-drivable, not orphaned) and W5 flags it explicitly for GN-5.W5's residue hunt
  (consumer-or-retire) — named, not hidden.
- **The re-founding temptation.** *Counterexample:* an executor re-founds skinny as root inside
  GN-0 (SK-V22's GW0 shape), coupling the archival's blast radius to the fourteen-patch port and
  risking a hot-path perturbation before GN-1's non-regression floor is captured. *Guard:* §1.4
  supersession — skinny is retained in place; re-founding is GN-1's charter.
- **The silent perf claim.** *Counterexample:* a close report states the purge "sped up CSS
  tokenization" from a warm one-arm bench. *Guard:* §1.3 refutation guard — GN-0 quotes no
  bench; the regen byte-identity is the sole artefact and it proves *no* runtime delta.
