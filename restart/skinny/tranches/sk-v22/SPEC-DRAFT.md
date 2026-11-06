# SK-V22 SPEC-DRAFT — The Graduation and the Great Archival

Date: 2026-07-09. Charter: `PASS-ALPHA.md` (five binding corrections consumed herein). Evidence
base: research/01-07 (this pass) + the load-accounted graduation baseline (research/04) + the
frozen SK-V21 corpus (`sk-v21/SPEC.md`, `ADDENDUM-GREENFIELD-PRUNE.md`, `DEFERRED-LEDGER.md`,
research/01-41, prototypes, critiques). This document AMENDS the frozen SK-V21 SPEC's execution
substrate (§6 names exactly what) and dispositions the repository itself. It reopens no frozen
architecture decision; every SK-V21 performance thesis stands as written. Rev 2 (2026-07-09):
the three-lens critique folded in place — CRIT-architecture A1/A2 + R1-R6, CRIT-honesty H1-H6 + F,
CRIT-charter Findings A-E; the CONVERGENCE-LEDGER records what moved.

The pivot, restated once: **skinny graduates to the tree of record** (PASS-ALPHA:9-14). The
SK-V21 TW1a legacy split-in-place is superseded — its value (six-seam decomposition) is already
realized in skinny's 11-crate topology (research/02:193). The graduation is a cutover: master's
entire git-tracked state is preserved behind one umbrella tag, the working tree is re-founded on
skinny, the ~40K legacy hand seam ports in selectively (in truth ~10K of vendored infra plus a
`.bbnf` corpus — research/02:167-171), and everything else archives.

---

## §1 — THE GRADUATION: the wave re-map, the port-in list, the maturation order

### §1.1 The re-lettered manifest

The graduated campaign runs under new letters — **GW** (graduation waves) for the bbnf tree,
**PT-R / PT-T** (parse-that Rust / TypeScript) for the constellation uplift track (§3), ratifying
research/07's `TW-X.R`/`TW-X.T` placeholders (07:116-117). Order:

**GW0 (cutover + archival) → GW1 (edition-2024 + hygiene) → GW2a (walls + the one regen host,
in-tree) → GW2b (one-regex-home, cross-repo-paired with PT-R.4) → GW3 (=TW4.1) →
GW4 (=TW3 ∥ TW4.2) → GW5 (=TW5) → GW6 (=TW6)**, with PT-R.1-2 and PT-T.1-5 running independently
except at the two named cross-repo edges (§3.4). The GW2a/GW2b split is deliberate: a single
conjunctive close gate must never couple an in-tree wall dissolution to a cross-repo fold's
schedule — GW2a closes on in-tree evidence alone; GW2b closes paired.

The Y.13 consumer-invariant close-gate law (SK-V21 SPEC §1, DEFERRED-LEDGER N21) carries
unamended: every substrate wave closes on a runtime consumer-invariant gate, liveness AND
measured win; hygiene/stack waves close on build+bench parity; every close cites the template.
Every no-regression clause binds to the research/04 load-accounted baseline via paired
interleaved capture (§5) — the "TW0 quiet baseline" role is filled, by amendment, by 04.
The binding mechanism, precisely: 04 is the standing campaign ANCHOR; each wave's regression
PROOF is its own paired same-session before/after capture against the prior wave's end-state;
GW1's measured channel ratio re-anchors the floor for GW2+ (04-adjusted, the adjustment
ledgered) — the anchor never moves, the proof always pairs.

### §1.2 The SK-V21 manifest, wave by wave — SURVIVES / RE-HOMES / SUPERSEDED

Per research/02 §3, ratified with the following calls (SPEC §11 coordinates cited):

| SK-V21 wave | Call | Graduated owner | Notes |
|---|---|---|---|
| TW0 W0.0 (D-G reconcile, `sk-v19-frozen`, supersession pointer) | SURVIVES, RE-TARGETED | GW0.a | Tag semantics invert: skinny UN-freezes as worksite; the four REDRESS ids (51/53/246/247, DEFERRED-LEDGER §0) carry verbatim into the graduated ledger; the archival branch cut + graduation declaration IS commit 0 |
| TW0 W0.1 (24-crate supersession doc) | SUPERSEDED | GW0.e | The six-seam split target no longer exists; replaced by this archival manifest + the durable doc home (§2, §4) |
| TW0 W0.2 (Y.13 law) | SURVIVES UNCHANGED | GW0 (adoption), all waves | §1.1 above |
| TW0 W0.3 (five-lock falsifier audit; widened Lock-14) | RE-HOMES | GW0.d | Lock-14 keys on skinny's ACTUAL crate dirs — `lock14_baseline.rs:1287` already references `crates/codegen/src/general_descent/`; re-point, don't invent (02:187) |
| TW0 W0.4 (Lock-15 + target-cpu) | RE-HOMES | GW1 | Profile config on the graduated workspace; precedes any decision-grade bench of GW2+ |
| TW0 W0.5 (Lock-4 + csc411 re-vendor checkpoint) | SURVIVES | GW0.b | The checkpoint IS the vendoring mechanism for egraph/csp-solver port-in (02:189) |
| TW0 W0.6 (deletions: x86/wasm, bootstrap, ser) | SUPERSEDED | GW0.c | The whole legacy tree archives; residual: skinny's empty `crates/simd-scan` stub deletes (02:84-86) in GW1 |
| TW0 W0.7 (inline-test relocation) | SURVIVES, RE-SCOPED | GW1 | Census corrected to **75 blocks / 34 files** (07:78, superseding both "21+" and 02's 32-file count); the "7 core" half is moot |
| TW0 W0.8 (MASTER-PLAN reconcile) | SUPERSEDED | GW0.e | The plan corpus archives; residue = graduated docs contradict nothing |
| TW0 W0.9 (oracle hygiene, W3C rekey) | SURVIVES | GW1 | Substrate-neutral |
| TW0 W0.10 (NO-GO letter; junk ignore) | SURVIVES | GW0.f (seam records) | Letter dated 2026-07-05 (research/03:100-104); playground junk moot — the tree dies |
| TW0 W0.11 (quiet truth-floor bench) | SUPERSEDED-AS-AMENDED | DONE (research/04) + §5 standing law | loadavg<2 is environmentally unachievable; the load-accounted protocol replaces it. 04 IS the graduation baseline; the A3 channel-ratio capture (DEFERRED-LEDGER:122) is delivered by GW1's paired old-pin/new-pin capture |
| TW-S (edition 2024 + stable pin) | SURVIVES, RE-HOMES | GW1 | = G6/S-2/N26; PROTO-A3's five edits applied to the LIVE workspace and committed (07:79); TW-S.2 criterion re-path rides |
| TW1a (247K core topology split) | **SUPERSEDED** — the known supersession | — | Skinny is already decomposed; nothing to split; the wave dies (02:193). Its Lock-14 re-key survives as GW0.d |
| TW1b (one-tape AoS + eager-builder retirement) | COLLAPSES TO A GATE | GW1 | Skinny's `runtime/src/tape/` is already the single AoS tape; no `TapeStructBuilder` exists (02:194). Residue: Lock-1 falsified two ways (grep-zero + runtime single-owner `frame_depth` assert) lands in GW1's gate battery |
| TW1c (rename; N24 reclaim; presize delete) | MOSTLY SUPERSEDED | GW1 | 88.5% of the blast radius was legacy-core regen (02:195); residue: grep-zero hygiene gate on retired names + `simd-scan` stub delete |
| TW2 (the one generator) | SURVIVES as the CENTRAL wave, GROWN, SPLIT | GW2a + GW2b | GW2a absorbs G1 (walls in-tree), G5 (xtask un-fork), grammar-source wiring (G4); the loc-budget gate re-homes; the css_l4 target shrinks 100× (skinny generated css_l4 is 974 LOC — 02:196). GW2b carries one-regex-home (G8), cross-repo-paired with PT-R.4 |
| TW4.1 (Eisel-Lemire + KernelCostTable) | SURVIVES, RE-TARGETED | GW3 | Binds to `bbnf-simd`'s live dispatch, not the phantom `simd-scan::KernelShape::select` (02:197). The 04 baseline sharpens the gate: the canada float tax is +1.48 cyc/B, localized to `materialize_f64` (04:149-159). Ownership across the GW2b fold is NAMED: GW3 owns the float-tax fix AND its gate wherever the `eisel_lemire` module lives at execution time — post-GW2b that is the canonical engine, so the change lands upstream as a paired PT companion commit and the vendor re-syncs in the same wave; the gate is always measured bbnf-side |
| TW3 (value plane) | SURVIVES UNCHANGED in scope | GW4 | = G2, the largest genuine forward workload; the bbnf-path cursor alphabet REBUILDS here rather than porting the legacy crate (02:157) |
| TW4.2 (decision engine + kernel plane) | SURVIVES, RE-HOMES | GW4 | Over skinny's `passes`; `decision_csp.rs` retires; C4/C5 shrink (kernels already unified aarch64-side). Four ledger sub-rows carry NAMED gates here, not blanket inheritance: N22 (`node_budget` guard verified before any escalation), N23 (Y.5 connected-components CSP), the precept-21 `perf_cost` production fix (`passes/src/lib.rs:590`), and the §5 profile-guided-specialization re-examination trigger |
| TW5 (type resolution + @host) | SURVIVES UNCHANGED | GW5 | B3 width patch re-land + @host on skinny's grammar/ir |
| TW6 (fleet + TOML + CSV flip) | SURVIVES, GATED on GW2a + GW0.b | GW6 | Fleet re-baselines from 3 toward ten; the grammar-source port-in is the precondition the frozen SPEC never anticipated (02:201) |

Ledger continuity: every DEFERRED-LEDGER row's wave coordinate re-maps by this table; no row is
orphaned; the empty-chronic-ledger assertion (DEFERRED-LEDGER §7) stands under the new letters.

### §1.3 The legacy port-in list (port-or-rewrite verdicts, ratified from research/02 §2)

| Legacy surface | Verdict | Wave |
|---|---|---|
| `grammar/` `.bbnf` fleet (~30 files: google-sheets, css/l4 ×15, bbnf, bnf, ebnf, csv, toml, math) | **PORT-IN (move)** — pure data; blocks all regen post-archival (G4) | GW0.b |
| `egraph` + `egraph-derive` (2,228 LOC) | **PORT-IN (vendor)** — general-infra crates, workspace members at **`crates-infra/egraph` + `crates-infra/egraph-derive`** (the DECIDED home — 07:112-114's open question closed: the general-infra precept keeps them out of domain `crates/`; not own-repo, not registry); also parse-that's build seam (07:80,49) | GW0.b |
| `csp-solver` (7,393 LOC) | **PORT-IN (vendor), then PRUNE** — canonical home stays csc411 (memory: csp-solver-crate); the vendor mirror lands at `crates-infra/csp-solver` + sync script survives; `decision_csp` retires at GW4 | GW0.b / GW4 |
| core hand seam (~31K frontend/lower/backend) | **REWRITE-SUPERSEDED** — skinny's un-forked codegen IS the adjudicated replacement; porting it back reintroduces the pathology (02:156) | dies with the archive |
| `bbnf-path` (+`-ts`) | **REWRITE-DEFERRED** — the value plane subsumes the cursor alphabet natively at GW4 | GW4 |
| `lsp` + `ir/vm` + @debug constellation; `gorgeous` | **KEEP-DORMANT-IN-ARCHIVE** — ride `archive/legacy-crates`; the host wave is no longer TW-P (superseded, §6.5) but the future greenfield shopwindow wave, which must NAME the re-bind at its opening (ADDENDUM §1:30-31) | future shopwindow wave |
| `analysis`; legacy `ir`; `backend/ts` arm | **REWRITE-SUPERSEDED / KEEP-DORMANT** — skinny's passes+ir carry the live analysis; the TS arm rides the archive as the Emitter-seam proof | dies / archive |
| `bootstrap`, `ser`, legacy `simd-scan` | **DELETE** (husk / Lock-12 / x86-wasm bodies) — via whole-tree archival | GW0.c |

### §1.4 The maturation order — DECIDED: archival first, edition-2024 second, walls third

The rationale: the frozen-hot-path finding (research/04) plus the coupling map (02/07) and the
recorded A3 channel confound (SPEC.md:31-32), read together as an ordering instrument — 04
grounds point 1; points 2-4 rest on their own cited evidence:

1. **The hot path is frozen and certified.** `generated.rs` untouched since 2026-06-02; the
   baseline reproduces A1/report-33 — product lanes within ~3%, third-party baselines within
   ~6% (04:26-33, 42). GW0 (a pure move) and GW1 (a
   toolchain change) are the only two waves that can bind cheap parity gates to it — "a pure move
   must not move a number" (SK-V21 TW1a's gate philosophy, re-homed). Run them while the floor
   is freshest.
2. **Walls-in-tree regenerates the hot path.** GW2a's WsPolicy threading, leaf-regex interpreter,
   and WART-C JSON fold change EMITTED code — the first wave that can legitimately move a number.
   Running it on the final toolchain means its paired regression capture has ONE variable (the
   wall fix), not two (wall fix + toolchain). The A3 nightly-vs-stable channel confound
   (SPEC.md:31-32) is the recorded cost of mixing those variables; we do not pay it twice.
3. **Edition-2024 is priced and cheap** — five edits, divan EXIT 0 both channels (PROTO-A3;
   DEFERRED-LEDGER N26). Cheapest first per the build-infra-first precept; and GW1's paired
   old-pin/new-pin capture delivers the un-confounded channel ratio the ledger demands
   (DEFERRED-LEDGER:122) as a by-product, closing that row without a separate session.
4. **The archival cannot wait** — skinny's regen and `passes` build reach INTO the legacy tree
   (`skinny/Cargo.toml:36-37`; `xtask/src/main.rs:191`; `regen_css.rs:5-20` — 02:93-101), and
   parse-that's regex engine resolves egraph through it (07:49,80). GW0.b resolves that coupling
   FIRST; everything downstream builds on a self-contained workspace.

### §1.5 GW close gates (each consumer-invariant, per the Y.13 law)

- **GW0**: graduated tree builds green with zero path-deps escaping the workspace; 3-grammar
  regen byte-IDENTICAL post-move; JSON+CSS oracles green — AND, verified BEFORE GW0.c's
  removal, no oracle/parity/test cross-references the archiving legacy tree (the oracle
  reference side is external SOTA — serde_json, sonic-rs, lightningcss — which survives; legacy
  `core` was never the parity reference); paired parity capture vs the 04 baseline shows no
  movement (pure-move proof); the full tag set exists and each named tag resolves
  (`git show <tag>:path` on a sampled file per surface); Lock-14 re-keyed and FIRES on a
  planted phantom before reading green; **GW0.e delivered** — the six durable docs +
  EVIDENCE-INDEX exist, every §4-table row resolves, every index coordinate is a full
  tag-relative path that `git show` resolves, and the PERFORMANCE-THESIS precondition (the H1
  twitter-sonic reconciliation capture, §4) has run; **GW0.f delivered** — the four dated seam
  records committed in-repo. The wave does not close on build/tag evidence alone; its marquee
  deliverables are inside its own gate.
- **GW1**: whole-workspace pinned-stable edition-2024 build; 75/34 inline-test blocks relocated,
  zero `#[cfg(test)]` under src/; Lock-1 falsified two ways on the graduated tape; grep-zero on
  retired names + `simd-scan` gone; paired old-pin/new-pin cold capture ledgered (the channel
  ratio); no regression vs 04.
- **GW2a**: ONE emit path serves every in-tree grammar (the WART-C fold close — `json_templates/`
  + `json_sink_direct/` retired, `runtime_generator.rs:100-140` fork gone); `regen --check`
  byte-equality + `generated_loc_budget` + N25 idempotency enforced; metadata-driven regen host
  ([workspace.metadata.bbnf.grammars] lists every fleet member); paired capture: no cold
  regression on any corpus, per-corpus reported. Closes on in-tree evidence alone.
- **GW2b** (paired with PT-R.4): grep-zero `parse-that-regex` fork + skinny `bbnf-regex`
  recognizer crate deleted post-fold; the vendor mirror `crates-infra/bbnf-regex` is
  byte-identical to the canonical engine at the pinned upstream commit (`diff -r` clean, sync
  script enforced); the canonical engine builds on the graduated stable toolchain against
  `crates-infra/egraph` (§3.4 cross-repo gate); zero build-time cross-repo edges in the
  graduated tree (`cargo metadata` shows no path escaping the workspace); paired capture: no
  cold regression, per-corpus reported.
- **GW3-GW6**: the SK-V21 close gates carry verbatim with re-homed paths (SPEC §11 TW4.1, TW3,
  TW4.2, TW5, TW6), each binding to the 04 baseline; GW3's headline gate: canada eager cold
  beats sonic (the +1.48 cyc/B float tax closed — 04:222-226).

---

## §2 — THE ARCHIVE MANIFEST (normative, executable)

Ratified from research/01 wholesale, with its three open questions decided here. The load-bearing
finding stands: **the SK-V21 dispositions were never executed** — `git tag` returns only `v1.0.5`
(01:12-17); SK-V22 actually creates the refs. No history rewrite; the 1.7G `.git` is the archive
and is retained deliberately (01:30-32).

**Decisions on 01's open questions:** (i) `archive/legacy-crates` is a BRANCH, not a pure tag —
the selective port-in workflow (checkout, cherry-pick a crate) wants a named branch (01:225-227).
(ii) Root `grammar/` is the canonical fleet copy — skinny carries only `json.bbnf` in-tree and its
xtask consts point at root `grammar/` (02:93-96); GW0.b merges both into ONE graduated root
`grammar/`. (iii) The `data/` trim is gated on a provenance enumeration: the corpora the oracles
and benches consume (twitter/canada/citm JSON; bootstrap/tailwindcss/material/animate CSS —
04:79-113, 190-214; the `test-fixtures` loader) are KEPT; generated stress corpora delete.

### §2.1 The executable sequence (tags first, then removals — order is normative)

1. **Tag set** (one commit sequence, before ANY removal — 01 §9):
   `pre-graduation-2026-07-09` (umbrella, entire master state); branch `archive/legacy-crates` +
   tag `legacy-totality-final`; `restart-doclayer-final`; `pre-restart-corpus-final`;
   `restart-audit-2026-05-03`; `legacy-wasm-final`; `vscode-ext-final`; `playground-spa-final`;
   `restart-archive-gen1`; `sk-v16-w6-lazylock-experiment` on `f2fe49bbc` (the ONE worktree
   commit not reachable from master — 01:125-129).
2. **Rescue checks** (before removal, each a named step): skim the `bbnf-lang-json-threaded-fork`
   dirty +102-line diff once, then discard (it patches a file already marked DELETE — 01:97);
   enumerate `data/` corpus provenance (above); reconcile `grammar/` to one copy (above);
   verify no oracle/parity/test cross-references the archiving legacy tree (§1.5 GW0 gate —
   run it here, pre-removal, not post-hoc); copy sk-v22's own tranche directory to the
   graduated `docs/tranches/sk-v22` (it lives UNDER the `restart/` removed at step 4).
3. **Port-in** (GW0.b — BLOCKS step 4 for `crates/`): `.bbnf` fleet → graduated `grammar/`;
   `egraph`/`egraph-derive` → `crates-infra/egraph[-derive]` workspace members; `csp-solver`
   vendor mirror → `crates-infra/csp-solver` + `sync-csp-solver-vendor.sh` port; xtask source
   consts repointed; the csc411 diff-against-upstream checkpoint recorded first (W0.5 mechanism).
4. **Removals**: `git rm` the archived source trees (`crates/` — only after step 3 AND the
   parse-that repoint, §3.4 — `restart/`, `restart-archive-2026-05-04/`, `docs/` [precepts
   submodule re-homes live], `audit/`, `wasm/`, `extension/` source, `playground/`, legacy
   `Makefile`, root `package.json`+lock); `git worktree remove --force` all 17 + `prune` +
   delete the 16 `worktree-*` branches; `rm -rf` the sibling shelf (01 §4 table); delete all
   build artifacts.
5. **Re-found root**: skinny's `[workspace]` becomes the root manifest; `docs/precepts` submodule
   wired at the new path; fresh minimal `Makefile`/`justfile` (build/test/bench/regen/profile —
   no `ay-*`/`az-*` sprawl); README/LICENSE/rust-toolchain/.rustfmt/.github rebased on skinny's
   equivalents (01 §2).
6. **Durable docs land** (GW0.e — §4), authored via `git show <tag>:path` reads over the
   archived corpus — the working tree is already emptied of it at this step; the tag-read is
   the authoring mechanism, not a fallback. Gated in GW0's close (§1.5), preconditioned on the
   H1 reconciliation capture (§4).

### §2.2 Disk reclaim (01 §8, authoritative figures)

Total working-tree reclaim **≈ 202G**, of which **~186G is pure regenerable build churn**: the 17
stale `.claude/worktrees/` (90G — the single largest target), skinny `target/` 57G, root `target/`
33G, orphan sibling `CARGO_TARGET_DIR`s 10.2G, `.profile-target/` 5.2G, the threaded fork 3.4G
(post-skim), registered sibling worktrees 1.4G, `data/` trim ~1.0G, root `node_modules` 283M, the
orphan copy 130M, profiling/extension drops ~215M. Live siblings untouched: `bbnf-buddy`,
`parse-that`, `value.js`, `pprint` (01:110-112).

### §2.3 The clean top-level shape and the durable doc home

The graduated tree (01 §6): `Cargo.toml`/`Cargo.lock` (skinny's workspace, edition 2024 after
GW1), `crates/` (graduated implementation — domain crates only), **`crates-infra/`** (the
general-infra tier, per the general-infra-crates precept: `egraph`, `egraph-derive`, the
`csp-solver` and `bbnf-regex` vendor mirrors + their sync scripts), `grammar/` (the canonical
fleet),
`data/` (canonical corpora only), **`docs/`** — the named durable doc home — carrying
`ARCHITECTURE.md`, `PERFORMANCE-THESIS.md`, `NEGATIVE-RESULTS.md`, `BENCH-PROTOCOL.md`,
`ORACLES.md`, `EVIDENCE-INDEX.md`, `precepts/` (the live submodule, re-homed), and
`docs/tranches/` (tranche docs going forward — sk-v22's own directory graduates there as the
first resident; sk-v3.5..sk-v21 are tag-retained, not copied), plus `xtask/`, `scripts/`
(surviving profiling/worktree/hooks tooling only), a minimal `Makefile`/`justfile`, and root
config. Gone from root: everything in §2.1 step 4.

---

## §3 — THE CONSTELLATION: dated seam records + the parse-that SOTA uplift track

### §3.1 Dated seam records (normative, from research/03)

- **SEAM-EGRAPH (2026-07-09)**: parse-that's regex engine build-depends on `egraph`/
  `egraph-derive` (only on-disk home: the archiving `bbnf-lang/crates/`) and `csp-solver`
  (canonical home csc411) via a gitignored patch table (03:31-46). The graduation re-homes
  egraph/egraph-derive into the graduated tree at **`crates-infra/`** (the named target); both
  repos repoint — parse-that's patch entries move from `…/bbnf-lang/crates/egraph` to
  `…/bbnf-lang/crates-infra/egraph`, a REAL repoint, not a no-op (the legacy-identical-path
  shortcut is rejected: general-infra does not re-enter domain `crates/`). **Hard ordering:
  GW0.b → PT-R.5 repoint → GW0.c `crates/` removal** (07:95-99) — the single
  highest-consequence ordering constraint in GW0: a mis-order bricks two trees' builds at once,
  so GW0.b's close explicitly verifies both repoints before any removal.
- **SEAM-NOGO (dated 2026-07-05)**: bbnf's LR/packrat verdict is NO-GO; parse-that is released
  from the bbnf keep (research/41:116-121 via 03:100-104). Recorded in-repo at GW0.f; the WDM/LR
  tier retirement executes in PT-R.1 (charter correction 4).
- **SEAM-PT-E1 (2026-07-09)**: upgraded from "mirror-when" to EXECUTED — PT-T.2 ships the scoped
  diagnostics; bbnf copies the scoped-return `Diagnostic` shape when its Value API grows an error
  surface at GW4+ (06:230-233).
- **SEAM-XTASK**: legacy `xtask/` archives with legacy `crates/`; `skinny/xtask` matures into
  the one regen host at GW2a — collapse `regen_css.rs` into the one generator, split the 2,780-LOC
  `main.rs` into a directory module, fold/archive `skv15_w0.rs` + `real_typed_schema.rs`, add the
  three missing gates (N1 byte-equality, N6 loc-budget, N25 idempotency) (03:125-145).
- **SEAM-BUDDY (2026-07-09)**: `~/Programming/bbnf-buddy` is a live sibling repo with ZERO bbnf
  dependency (its `bbnf-*` strings are its own file-format brands); archival touches nothing;
  transitive edge bbnf-buddy → value.js → parse-that noted for the PT semver discipline only
  (03:147-156).
- **SEAM-LATEX-PAPER**: `@mkbabb/bbnf-lang ^0.1.1` is a real published-package edge (03:94-97);
  the graduation does not unpublish; any future TS surface re-versions rather than mutating 0.1.x.

### §3.2 The PT Rust track (supersedes R3's "obligations: nothing else" per charter correction 4)

The consumer map first, because it scopes everything: **no Rust-combinator consumer survives the
graduation** (every one — core, ir, gorgeous, wasm, xtask — archives; skinny uses codegen; the
backbones consume the TS package — 05:108-126). Adjudication of 05's open question (A): **the
graduated bbnf tree does NOT grow a combinator surface**; the Rust uplift is parse-that's own
maturation in its own repo, gated on its own test parity — bbnf gates only on the regex-engine
seam. The other half of (A) is RECORDED, not routed past: post-graduation the Rust combinator
crate is consumer-less (every surviving consumer takes the TS package; its demo parsers die in
PT-R.1; its bbnf consumers archive). The uplift is charter-mandated (correction 4) and proceeds
deletion-first so the invested surface is minimal — PT-R.1 before PT-R.2; whether the combinator
crate ultimately retires is parse-that's own future adjudication and gates no GW wave.

Adjudication of (B) — the one-regex-home, MECHANIZED: the canonical `bbnf-regex` engine keeps
the name and the home (`parse-that/rust/regex`); the graduated tree consumes it as a **vendor
mirror at `crates-infra/bbnf-regex`** with `sync-bbnf-regex-vendor.sh` — the exact W0.5 /
csp-solver mechanism, already ratified for a canonical-home-elsewhere dependency. No published
pin is required (the engine is not established as published — 03 §1A) and no path-dep escapes:
the GW0 "zero path-deps escaping the workspace" gate and §1.4's self-contained workspace hold
verbatim through GW2b and beyond. The apparent graduated-egraph ← parse-that-regex ←
graduated-runtime cycle dissolves into two acyclic build graphs plus a sync protocol: parse-that
resolves egraph via its repointed patch table (its ONE cross-repo edge, the status quo made
durable); the graduated tree resolves the vendored engine and egraph as workspace members.
Divergence is a gate failure, not a possibility: GW2b holds the mirror byte-identical to the
pinned upstream commit. skinny's 322-LOC **family recognizer** (`RegexKind`/`ByteSet256`/
`FirstSet` — a functional tier, not a stub; 05:188-191) folds UPSTREAM as the engine's
recognizer front; the local crate, redundant post-fold, deletes at GW2b.

Adjudication of (C): `bbnf-simd` is the SIMD façade and **stays resident in the graduated
tree**; parse-that gains NO dependency on it — the {ws, quoted, number} family scanners
transpose upstream as engine-owned code, `bbnf-simd` retains the tape/value kernels, and the
`eisel_lemire` module's one home is the canonical engine, reaching the graduated tree via the
vendor (no duplicated module, no reverse edge). The portable fallback is the existing
per-kernel scalar path — aarch64/Apple-M-only stands for bbnf's consumption path (SK-V21 SPEC
§0); no x86 SIMD work. That posture binds bbnf; parse-that's own platform/CI envelope is its
own call, with the scalar tier as its portable default (05:315-317).

Waves, re-ordered from 05 §7 under the abrogate-before-patch precept (delete before de-nightly —
do not transpose code a later wave deletes):

- **PT-R.1 — the pure-primitives cut.** Delete `span_parser/` (the falsified SpanParser
  experiment, ~1,150 LOC — 05:227-233), the demo parsers (`parsers/css/*`, `csv.rs`, `json.rs`),
  the `bootstrap` re-export crate (07 R-5), the hand-timed CLI (07 R-6, a warm-timing artifact
  contradicting the cold-bench precept), and the WDM/LR-keep machinery per SEAM-NOGO. This
  SATISFIES 07's R-4 stack unification by deletion: one combinator stack remains. *Gate*:
  `combinator_test.rs` + regex tests green; workspace members = `parse_that` + `regex` only;
  competitor benches archive.
- **PT-R.2 — de-nightly as redesign.** The headline correction stands: nightly is FOUR gates
  deep, not one — `portable_simd`, `fmt_helpers_for_derive`, `stmt_expr_attributes`
  (`rust/parse_that/src/lib.rs:1-3`) plus `smallbox[nightly]` (05:14-33; 07:43). Transpositions:
  `std::simd` → stable `core::arch::aarch64` intrinsics on the bbnf-simd pattern (the surface
  first shrunk by PT-R.1); smallbox → **BumpSlab arena allocation of the parser tree**
  (transposition A — the S32 inline budget is defeated on combinator nodes anyway, 05:56-63);
  drop the two cosmetic gates; delete or truthfully rewrite `rust-toolchain.toml` (07 R-3 — it is
  a bbnf-lang copy-paste citing another repo's ICE). *Gate*: `cargo +stable build` EXIT 0;
  `cargo tree -e features` zero nightly; scanner bench parity within the load-accounted ratio
  band; `grep -c 'bbnf-lang' rust-toolchain.toml == 0` or file absent.
- **PT-R.3 — regex-engine SOTA absorptions.** Fold `is_accept` into the transition table (tagged
  StateId / is_match bitset) and read `padded_bytes()` in the `find_at` hot loop — two clean
  transpositions on the hottest loop (05:161-167); degrade the 4096-state bail to an NFA walk
  instead of `panic!` (05:153-160). The engine's SOTA parity is otherwise confirmed (byte-class
  compression, Hopcroft, accel, one-pass, egraph HIR simplification BEYOND regex-automata —
  05:143-150); the lazy-DFA and capture tiers stay rightly absent. *Gate*: match-position parity
  across `regex/tests`; paired cyc/byte delta profiled under load accounting.
- **PT-R.4 — the fold-back (bidirectional).** Skinny's recognizer tier moves upstream as the
  engine's fast front (`RegexKind` families, allocation-free); `StringMode`/`StringFlags`, the
  Höhrmann UTF-8 decoder, and the {ws, quoted, number} family scanners absorb as engine-owned
  code (no parse-that dependency on `bbnf-simd` — §3.2 C); the byte-identical 660-LOC
  `eisel_lemire/table.rs` duplication collapses to one module, homed in the engine (05:199-215).
  The bbnf half — skinny deletes the `parse-that-regex` fork (3,022 LOC) + its local recognizer
  crate and adopts the `crates-infra/bbnf-regex` vendor mirror of the canonical engine —
  executes as GW2b, paired with this wave. *Gate*: number/decode/quote-parity suites green in
  both repos; the DFA path produces identical spans to the family scanners on {ws, quoted,
  number}; grep-zero forked modules in the graduated tree; vendor byte-identical at the pin.
- **PT-R.5 — the resolution-graph relocation.** The gitignored 8-coordinate cross-repo
  `.cargo/config.toml` patch web → workspace-relative patch entries against the graduated
  tree's **`crates-infra/egraph` + `crates-infra/egraph-derive`** (the named target, §3.1
  SEAM-EGRAPH — a real repoint, the path changes); csp-solver keeps its csc411 coordinate; the
  dead `bbnf = crates/core` line deletes (05:236-244; 07 R-7). *Gate*: clean `cargo metadata`,
  zero path escapes into an archived directory. **Blocks GW0.c's `crates/` removal.**
- **PT-R.6 — hazard closure.** `Box::leak(format!(…))` diagnostic labels → interner (unbounded
  leak); `parse_padded(&PaddedView)` entry amortizing the per-parse 64B-aligned input copy
  (05:88-95, 302-305). *Gate*: diagnostics-feature tests; load-accounted profile of the copy cost.

### §3.3 The PT TypeScript track (target 1.1.0; ratified from research/06 §5)

The consumed surface is exactly known — 9 named imports + the listed instance methods; keyframes
is parse-that-FREE directly, shielded behind value.js (06:26-51; 03:80-91). Both open asks land
as MINOR. Waves PT-T.1-5 adopt 06's TW-PT-TS.1-5 verbatim:

- **PT-T.1 — PT-E2 inference tightening**: `Parser<any>` → `readonly Parser<unknown>[]`
  constraints; `const` type params; `NoInfer`; internal erasure stays. *Gate*: `.d.ts` inference
  assertion (`any(string("a"),string("b")): Parser<string>`); **cross-repo typecheck of value.js
  + latex-paper against the candidate `.d.ts` green** (the semver tripwire).
- **PT-T.2 — PT-E1 scoped diagnostics**: `parseWithDiagnostics()` returning the existing
  `Diagnostic`; the scope replaces the process-global toggle via the PACKRAT_ARMED latch pattern
  (one pattern, two tiers); `suggestions`/`secondarySpans` lazily allocated (two arrays off every
  default-path state). *Gate*: born-RED `diagnostics-scoped.test.ts` (non-empty `expected`
  reaches the caller; interleaved scopes don't cross-contaminate; retained-heap flat clause);
  value.js suite green against the 1.1.0 tarball.
- **PT-T.3 — V8 hidden-class normalization**: uniform 3-field `Parser.context` (kill the
  shape-splitting `{}` default); retire `mapState` (the `Object.create` dictionary-mode hazard).
  *Gate*: shape-stability clause (`%HaveSameMap` probe or own-keys-order check); json-regression
  floor holds.
- **PT-T.4 — legacy/state-tier sweep**: retire `save`/`restore`/`from`/`getLineNumber`/
  `getColumnNumber` (unconsumed, superseded); assess `getLazyParser`/`lazy` decorator; bound
  `PARSER_STRINGS`; delete the stale `span-combinators.md` doc teaching an excised API (07 T-1,
  a live falsehood) and extend `proof:no-span-surface` to grep docs/. *Gate*: dist == source
  barrel; born-RED `proof:no-legacy-state-surface`; consumer typechecks green.
- **PT-T.5 — dispatch megamorphism lever, carry-iff-measured**: 2-arm/≤4-arm dispatch forms ONLY
  if profiled dominant on value.js's real CSS shape; the runtime tagged-switch is permanently
  falsified (−10-14% on V8) and shall not be re-attempted. *Gate*: `proof:perf` real-corpus
  clause or DO-NOT-CARRY.

Cross-cutting gate, every PT-T wave: `npm run proof:all` green; the three backbone suites
(value.js, latex-paper, keyframes-transitive) run against the 1.1.0 candidate before publish.

### §3.4 Cross-repo sequencing — what bbnf gates on

- **bbnf gates on**: PT-R.5 (patch-table repoint) before GW0.c may remove `crates/`; and GW2b
  gates on "the canonical engine builds on the graduated stable toolchain against
  `crates-infra/egraph`" (PT-R.2 de-nightly is the combinator crate's need — the engine crate
  itself carries no `#![feature]` and is already edition 2024, 03:19-22). The graduated tree
  itself carries NO build-time cross-repo edge at any point: the engine and csp-solver arrive
  as vendor mirrors, egraph as a workspace member (§3.2 B).
- **Runs independent of bbnf**: the entire PT-T track (TS package, own repo, own gates); PT-R.1,
  PT-R.3, PT-R.6.
- **Paired across repos**: PT-R.4 ↔ GW2b (the fold-back's two halves land against each other's
  parity suites); GW3 ↔ its PT companion commit when the float-tax fix touches the canonical
  `eisel_lemire` module (§1.2 TW4.1 row — GW3 owns fix and gate; the vendor re-syncs in the
  same wave).

---

## §3c — THE NO-LEGACY MANIFEST (normative companion, from research/07)

Every legacy/deprecated item across both repos, its gestalt transposition, owning wave, gate —
none carried, none patched. Rows R-1..R-7, T-1..T-3, S-1..S-4 of research/07 are ratified in
full with wave letters resolved:

| Item | Transposition | Owner | Gate |
|---|---|---|---|
| R-1 triple `#![feature]` nightly gates | stable `core::arch` re-founding on the bbnf-simd pattern | PT-R.2 | stable build EXIT 0 + bench parity |
| R-2 smallbox[nightly] | BumpSlab arena / enum-over-boxing redesign | PT-R.2 | zero nightly features; bench neutral-or-better |
| R-3 mis-attributed toolchain pin | delete or truthfully rewrite | PT-R.2 | zero `bbnf-lang` refs |
| R-4 dual combinator stacks | ONE stack by deletion of the falsified `span_parser` + arena'd survivors | PT-R.1 (+R.2) | one module; tests green; alloc/parse ≤ current |
| R-5 orphaned bootstrap crate | delete; bench moves | PT-R.1 | gone from members |
| R-6 warm hand-timed CLI | delete; divan is the timing surface | PT-R.1 | benches only |
| R-7 cross-repo patch coupling | egraph re-home + repoint; dead patch lines deleted | GW0.b + PT-R.5 | builds post-archival; zero archived-path escapes |
| T-1 stale span-combinators doc | delete; proof extended over docs/ | PT-T.4 | grep-zero excised names in docs |
| T-2 PT-E1/PT-E2 open | executed to 1.1.0 (feature work, not legacy) | PT-T.1/T.2 | consumer suites green |
| T-3 TS config currency | recorded CLEAN — no re-audit | — | n/a |
| S-1 75 inline-test blocks / 34 files | relocate to tests/ (N12 re-scoped to true magnitude) | GW1 | zero `#[cfg(test)]` under src/ |
| S-2 edition 2021 committed | PROTO-A3 recipe applied AND committed | GW1 | edition-2024 stable build |
| S-3 path-deps into archived `crates/` | port-in ordering constraint (hard edge) | GW0.b | zero escaping path-deps |
| S-4 commented-code/TODO surface | recorded CLEAN — no re-audit | — | n/a |

---

## §4 — THE LEVERAGE PLAN: the distillation map + EVIDENCE-INDEX

Six named durable documents land in **GW0.e**, distilled from the archived corpus (which stays
git-resolvable behind its tags — distillation is why branch-archival beats delete). Ratified from
research/03 §3A with owners:

| Deliverable | Distilled from | Lands |
|---|---|---|
| `docs/ARCHITECTURE.md` | skinny SUBSTRATE/COMPILER/WORKSPACE.md; sk-v21 research/20, /38; SPEC.md — the surviving architecture (un-forked dispatch, grammar-derived projections, lazy cursor, fused sink, NEON kernels) | GW0.e |
| `docs/PERFORMANCE-THESIS.md` | skinny BENCH.md; research/22-25, /33 (methodology + history); **research/04 of this pass supplies the quoted numbers, anchored to 04 ALONE**: parse_tape 2.10×/1.41×/2.06× vs sonic cyc/byte; CSS track1 2.07-3.47× vs lightningcss (04:111-115, 194-198). The twitter figure carries a MANDATORY reconciliation note: report-33 measured 1.59× against a sonic baseline of ~2.48 cyc/B; 04 re-measured that sonic side at 3.371 cyc/B (+36%, undisclosed in 04) while the product lanes reproduced within ~3% — the ratio jump is baseline-side and was never cross-validated. **PRECONDITION (gates GW0.e, §1.5)**: one paired twitter sonic/parse_tape re-capture per §5 adjudicates 2.10× vs 1.59×; the thesis quotes the re-validated figure with the drift disclosure; the EVIDENCE-INDEX PT row anchors to 04's coordinates and NEVER cites 33:59 for a 2.10× claim. The thesis also carries the "inherited leaf ranking, fresh PMU floor" caveat: 04's leaf-level self-time is transferred from report-33, not freshly symbolized (04:161-182) | GW0.e; GW1+ append parity rows |
| `docs/NEGATIVE-RESULTS.md` | the do-not-relearn ledger (03 §3B): AoS-beats-SoA; DTA residue; the twice-buried bitmap pre-scan; fold-over-cursor must be measured; hand-asm obviated by well-shaped Rust — `vshrn` the lone
residual hand-win (the shrn/vshrn movemask, Lock-16); warm benchmarks are fraud — PLUS the four REDRESS family meanings (DEFERRED-LEDGER §0) and the falsified V8 tagged-switch (06 non-goals) | GW0.e |
| `docs/BENCH-PROTOCOL.md` | research/33, /21, PROTO-A1/A2 + **the §5 standing law verbatim** | GW0.e |
| `docs/ORACLES.md` | `parity_oracle.rs`, `capacity_probe.rs`, skinny corpora, test-fixtures | GW0.e |
| `docs/EVIDENCE-INDEX.md` | the navigational map (below) | GW0.e; every wave appends |

**EVIDENCE-INDEX design** (03 §3C, ratified): a flat table mapping every load-bearing claim →
durable doc section → archived report coordinate → primary anchor. Coordinates are **FULL
tag-relative paths** — `git show <tag>:<full-path>` must resolve; shortened forms 404. E.g.
`NR-1 | AoS beats SoA | NEGATIVE-RESULTS §1 |
restart/skinny/tranches/sk-v21/research/35-deferred-fold.md:83 @ pre-graduation-2026-07-09 |
restart/era-VI-restart.md:59-64`. GW0.e's authoring gate resolves every coordinate before the
index lands (§1.5). The standing rule: **a graduated claim with no index row is, by
construction, unsupported**; every wave close appends its rows; new negative results enter
NEGATIVE-RESULTS + the index in the same commit that measures them.

---

## §5 — THE PROFILING DISCIPLINE, made standing law

Charter correction 5, ratified as the campaign's bench protocol and written into
`docs/BENCH-PROTOCOL.md`. **Profiling is never skipped for load; it is accounted.** The loadavg<2
gate is formally retired (environmentally unachievable — every SK-V21 session ran at 8-25,
SPEC.md:22-24; the 04 session at 8.6-14.0) and replaced by the load-accounted method validated at
PROTO-A1/report-33 and re-validated at 04 (self-reproduction: product lanes within ~3%,
third-party baselines within ~6% — 04:29-33, 42):

1. **PMU cycles/byte via `proc_pid_rusage(RUSAGE_INFO_V5).ri_cycles`** — the certified column;
   ins/byte recorded as the fully load-INVARIANT check. Compute-bound lanes drift ≤3% across
   windows; memory-bound lanes are flagged (~±20% at loadavg 12 vs 10 — the canada fused
   exception, 04:50-55).
2. **Paired per-sample interleaving** for every ratio: both sides timed back-to-back inside one
   sample, mean + 95% CI over N≥200; a ratio is decision-grade only when every window's CI
   excludes 1.0 in the same direction (04:117-138).
3. **Cold-per-parse only** (N=200, one out-of-band touch, fresh state, min AND median reported);
   no figure is ever quoted warm (NEGATIVE-RESULTS item 6).
4. **Multi-window repetition**: ≥3 separated capture windows, cross-window agreement reported.
5. **Per-block loadavg recorded beside every table**; wall ns/byte and Mbps are labeled
   DIRECTIONAL; the median is the robust wall statistic.
6. **Instruments of record**: `json_canon_bench` + `css_canon_bench` (graduated into
   `crates/bbnf-bench`); the stage-ladder decomposition (`parse_only → parse_tape → fused`) is
   the primary attribution when symbolisation is load-unreliable (04:161-168).
7. **Every wave's no-regression clause binds to the research/04 graduation baseline** as the
   standing ANCHOR; each wave's regression PROOF is its own paired same-session before/after
   capture against the prior wave's end-state; GW1's measured channel ratio re-anchors the
   floor for GW2+ (04-adjusted, the adjustment ledgered). The anchor never moves; the proof
   always pairs (§1.1).

---

## §6 — Supersession statement

This SPEC amends the frozen SK-V21 SPEC in EXACTLY the following, and nothing else:

1. **TW1a is superseded** (the 247K `core` split-in-place). Its value is realized in skinny's
   topology; its Lock-14 re-key survives as GW0.d (02:193).
2. **The ADDENDUM §3.1 freeze/worksite resolution is inverted**: skinny is no longer
   "differential oracle + reference only, TW2+ work happens in `crates/` after the port" —
   skinny IS the worksite and the tree of record; the `sk-v19-frozen` tag semantics re-target to
   the archival-branch cut + graduation declaration (GW0.a).
3. **TW0 W0.1 and W0.6 are superseded** by the great archival (whole-tree dispositions replace
   per-surface deletions and the six-seam supersession doc).
4. **TW0 W0.11's loadavg<2 quiet leg is amended** to the §5 load-accounted protocol; research/04
   fills the baseline role; GW1's paired capture delivers the outstanding channel ratio.
5. **ADDENDUM §2's TW-P "rebuild the existing SPA" is superseded** (charter correction 1): the
   playground archives outright (`playground-spa-final`); the future shopwindow is greenfield;
   only the skinny-backed wasm prerequisite survives as a concept.
6. **The "parse-that obligations: nothing else" framing is superseded** (charter correction 4):
   the PT-R/PT-T uplift track of §3 is net-new normative scope, with backbone semver and the
   consumer suites as its discipline bar.

Everything else stands: SPEC §§0-10 (the architecture and performance theses, the Y.13 law, the
AoS/fused-sink/kernel adjudications, the type plane), the TW2-TW6 scope (re-homed per §1.2), the
DEFERRED-LEDGER's empty-chronic assertion (every row re-mapped by name; none orphaned; none
re-deferred without a named wave), and every closed negative result — which now also gains a
permanent home in `docs/NEGATIVE-RESULTS.md` and the EVIDENCE-INDEX.
