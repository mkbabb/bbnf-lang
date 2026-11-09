# Dig B0–B2: the toolchain-wall era and the build-time codegen transposition

This report excavates three consecutive tranche directories from the pre-restart
bbnf-lang campaign — B0, B1, B2 — plus their spillover into B3/B4. These three
tranches form a single arc: a build-and-tooling crisis that consumed the campaign
for roughly two weeks (2026-04-15 through 2026-04-25), during which no
parity-critical runtime work advanced. The arc culminates in one genuinely
excellent architectural transposition (B2's build-time codegen) and one
unresolved structural pathology (multi-megabyte per-grammar generated Rust files)
that the transposition relocated but did not cure.

All three tranches landed on master — their close commits (`7b223cf6`,
`1d0815dc`, `d276934a`, `6142387f`, `f375cd38`) are all ancestors of the current
master HEAD. The artifacts they produced are live in the current `crates/` tree,
which is the edition-2024 totality codebase (not the skinny holdout). The
build-time transposition B2 authored is what regenerates the parsers today.

---

## (a) What each tranche attempted

### B0 — AY execution runway (infra annex)

B0 was a "bounded prelude annex" whose sole job was to remove build/bench/
profiling/command-surface drag so that a runtime tranche (AY) could execute its
near-parity close gates quickly (`docs/tranches/B0/B0.md:1-16`). Its charter
explicitly forbade any parser-runtime or semantic architecture work
(`B0.md:19-27`). Three waves, 14 commits: W0 added cargo aliases + a Makefile
"AY Iteration Surface"; W1 added a `profiling-prep` profile + idempotent
prebuild scripts; W2 split routine vs heavy correctness surfaces and CI
(`docs/tranches/B0/FINAL.md:19-83`). The headline claim was a "45× iter-check
warm speedup" (0.16 s vs 7.16 s, `FINAL.md:36-40`).

### B1 — Dev-loop truth + proof-surface hardening (infra annex)

B1 was a second prelude annex, opened when a mid-tranche scope pivot in AY-II
revealed that `.cargo/config.toml` and proc-macro dev-deps fell outside the
declared file bounds (`docs/tranches/B1/FINAL.md:12-18`). It waived its own
research wave (`B1.md:11-28`) and executed a pre-authored 12-step toolchain
migration: pin the nightly, rewrite the alias surface, migrate the bench harness
from `bencher` to `divan`, adopt `cargo-nextest` as required, rewire CI, and
propagate the pin to sibling repos (`docs/tranches/B1/TOOLCHAIN-MIGRATION.md:154-283`).
Four waves; the decision ledger is frozen in `TOOLCHAIN-SOTA.md` (819 LOC of
first-principles research) and `TOOLCHAIN-MIGRATION.md`.

### B2 — Build-time codegen transposition (the load-bearing one)

B2 is the only one of the three that touched real architecture. It retired the
`bbnf_derive` proc-macro's IR-pipeline contract: the 17-pass IR pipeline plus a
~30 K-line `TokenStream` emission moved out of rustc's expand phase into
`cargo xtask regen`, a workspace build step that writes per-grammar Rust source
to disk under `crates/core/src/grammar/generated/<ident>.rs`; consumers switched
from `#[derive(Parser)]` to a marker struct plus a `pub use` re-export; the
`crates/derive/` crate deleted outright (`docs/tranches/B2/B2.md:1-46`). Five
waves, all closed complete (`B2.md:130-136`).

---

## (b) What shipped vs what died on the vine

Everything in all three tranches shipped to master (git merge-base confirms every
close commit is an ancestor of master). But "shipped" conceals three items that
were declared green while their load-bearing measurement was deferred, never
taken:

1. **Divan cold-per-parse numbers were never produced.** B1's entire thesis was
   "bench truth is cold-per-parse" via divan (`B1.md:66-70`). Yet the per-grammar
   divan JSON emissions (`post-B1-W1-divan-*.json`) were routed forward to
   "AZ-I.W0 close" because generating them requires the bench-binary compile that
   pays the same `bbnf-bootstrap` > 600 s wall (`docs/tranches/B1/FINAL.md:176`,
   invariant W1.5 "skipped-cap-routed" at `FINAL.md:123`). The ±5% divan-vs-bencher
   parity gate was closed "green-routed" on **architectural equivalence** — i.e.
   asserted because "each ported file preserves bench bodies verbatim modulo
   mechanical harness translation" — with the numeric verification explicitly
   deferred (`FINAL.md:124`, W1.6). So B1 migrated to a "cold-per-parse" harness
   and closed without measuring a single cold-per-parse number.

2. **The iteration-loop ceiling was never met; it was renamed.** B1 invariant 11
   originally claimed a ≤ 5 min `iter-check-full` cold ceiling; the wave halted at
   **25:30 wall-clock** on the `bbnf-bootstrap` > 600 s single-derive proc-macro
   expansion and "never completed" (`docs/tranches/B2/FINAL.md:191`, invariant 13;
   `B1/FINAL.md:92` "halted at 25:30"). The invariant was reworded mid-wave (commit
   `1926aed1`) from a numeric target to "measured truth, not a numeric target"
   (`B1/FINAL.md:152`), and the ≤ 5 min target routed to AZ-I.W0.

3. **327 failing tests were carried as "pre-existing, not my scope" across every
   B2 wave.** Workspace nextest at every B2 close: 1 160 pass / **327 fail** / 3
   timeout / 27 skip (`docs/tranches/B2/FINAL.md:115-118`). The failure class is a
   single debt — `FusedBuilder::finish called with N open value frames remaining`,
   a debug-build assertion at `crates/tape/src/builder/mod.rs:1066` — routed B2 →
   B4.W1 and never fixed inside the arc (`B2/FINAL.md:120-131, 164`). Release-mode
   parity was green, which is what let every wave close "no regression."

What genuinely shipped and works: the xtask regen substrate, the marker-struct
consumer contract, the portable `include_str!` path scheme, the `regen --check`
CI+pre-commit gate, the pinned nightly, and the ICE-recovery documentation. These
are all live in `crates/` today (`xtask/src/{regen.rs,main.rs}` exist; the
per-grammar generated tree exists).

---

## (c) Root causes of divergence

### The proc-macro build wall — the primary structural failure

The `#[derive(Parser)]` macro ran a full 17-pass IR pipeline and emitted a
~30 K-line `TokenStream` at **expand time, once per consuming crate**, forcing
rustc to re-tokenise, re-parse, re-borrow-check, and re-metadata-emit that surface
per consumer (`docs/tranches/B2/B2.md:16-23`). Cold `cargo expand -p
bbnf-bootstrap --lib` exceeded **80 minutes** (`B2/FINAL.md:99`,
`B2/B2.md:10-12`). This is the deepest root cause of the era: code generation was
implemented as a proc-macro contract, which is the wrong tool — the proc-macro
contract is for small local transforms, not workspace-wide IR pipeline execution.
B2's own architectural thesis names this exactly (`B2/B2.md:16-23`).

### The generated-size explosion — the pathology B2 relocated but did not cure

This is the single most important finding for SK-V21's stack-abrogation axis.
Moving codegen from expand-time to a checked-in file changed **where** the cost is
paid, not the **size** of what rustc must compile. The per-grammar generated files
at B2.W1 close (`docs/tranches/B2/audit/W1-close.md:32-42`):

| Grammar | Lines | Bytes |
|---|---|---|
| `css_l4` | **203 499** | **11 041 925** |
| `bbnf` | 34 046 | 1 590 750 |
| `google_sheets` | 21 532 | 984 603 |
| `ebnf` | 12 900 | 550 685 |

These files are still in the current `crates/` tree and remain multi-megabyte:
`crates/core/src/grammar/generated/css_l4.rs` is **108 406 lines / 6.1 MB** today;
`bbnf.rs` is 21 557 lines / 1.09 MB. A single grammar generates a 6-megabyte Rust
source file that the compiler must ingest on every clean build. This is the
concrete origin of the standing `generated-size-budget` memory precept
(per-tranche line-count budget; overflow blocks the wave until an O(N) generator
regression is traced). The B-era hit that wall and normalised it: the full-sweep
regen wall is **~12:43** (`B2/FINAL.md:92`), dominated not by the IR pipeline
(~73 ms per grammar, `B2/FINAL.md:94`) but by the `bbnf` lib rebuild each
per-grammar file overwrite triggers. **The transposition solved the 80-min
expand wall and left a 200 K-line-per-grammar compile wall standing.** SK-V21 must
attack generated size directly; relocating delivery is not enough.

### The ICE storm — ambient-nightly + incremental-cache staleness

The toolchain-wall story the charter flags: **93 `rustc-ice-*.txt` files at the
repo root, 2026-04-15 through 2026-04-22** (`docs/tranches/B1/TOOLCHAIN-SOTA.md:28-32`).
Every sampled file (10/10) had the identical terminal panic frame:

```
thread 'rustc' panicked at
compiler/rustc_middle/src/query/on_disk_cache.rs:663:9:
cannot decode `AttrId` with `CacheDecoder`
...
 7: OnDiskCache::load_side_effect
 9: DepGraphData::try_mark_previous_green
12: query_impl::typeck_root::execute_query_incr
15: rustc_hir_analysis::check_crate
```

with query stack `#0 [analysis] running analysis passes on crate bbnf_analysis`
(`TOOLCHAIN-SOTA.md:37-57`). Every file was produced by ambient nightly
`1.96.0-nightly (9602bda1d 2026-04-05)`; **there was no `rust-toolchain.toml`** —
the repo rode whichever nightly each developer's `rustup default` last landed on
(`TOOLCHAIN-SOTA.md:59-62`).

The diagnosis (`TOOLCHAIN-SOTA.md:64-121`): this is **not** a bbnf proc-macro ICE.
It is the classic incremental-compilation cache-staleness pathology
(rust-lang/rust#92485, #93602, #92221, #92708) — an incremental-cache entry
written by one rustc invocation becomes uninterpretable by the next because the
internal `AttrId` space shifted. The trigger chain: bbnf-derive's cached
`TokenStream` (keyed under `target/.bbnf-cache/`) invalidates on schema bump /
grammar edit / worktree checkout, which shifts attribute-index ordering in
downstream units; `bbnf_analysis` is the first consumer to re-typeck that graph
and panics on decode. The **temporal clustering** (four ICEs in 31 seconds,
`TOOLCHAIN-SOTA.md:103-109`) is the smoking gun: the parallel rustc front-end
emits **one ICE per worker that touches the poisoned query**, so one poisoned
`iter-check` per workday × ~10 workers ≈ 13 ICEs/day ≈ 93 over 7 days. The
`-Zthreads=8` flag the same tranche recommended for build speed
(`TOOLCHAIN-MIGRATION.md:71-78`) directly amplified the ICE count.

Remediation was cheap and correct: pin the nightly to `nightly-2026-04-11` (past
the ICE SHA, `TOOLCHAIN-MIGRATION.md:57-69`), document `rm -rf
target/*/incremental` as the recovery step, add an `ay-clean-incr` target
(`TOOLCHAIN-SOTA.md:137-167`). The root cause is a two-line hygiene failure
(no pin + a stale-cache-prone codegen path), not a compiler bug in bbnf's control.

### Bench-optimism framing

B0's headline "45× iter-check warm speedup" (`B0/FINAL.md:36-40`) is a **warm**
dev-loop number. It is not a parse benchmark, so it does not directly violate
`no-warm-benches`, but the "45×" headline framing on a warm measurement is
exactly the optimism-shaped reporting the memory precepts guard against — and it
sits one tranche away from B1 closing its cold-per-parse harness without a single
cold number.

---

## (d) Meta-process pathologies

### Three prelude annexes in a row — infra swallowing runtime

B0's own indefatigability clause promised it would prevent "the old failure mode
where tooling reform swallows parity-critical runtime work" (`B0/B0.md:84-87`).
The arc did precisely that: **B0 (infra) → B1 (dev-loop infra) → B2 (codegen
architecture)** are three consecutive tranches, none of which advanced AY-II's
runtime parity, and each of which blocked the AY-II.W0' close ceremony
(`B0.md:23`, `B1.md:6-8`, `B2.md:123-126`). The AY-II runtime resumption was
deferred across all three. The "bounded annex" framing was applied recursively:
each annex declared itself bounded and carried "no successor debt tree," yet the
sequence itself became the successor debt tree.

### Mid-tranche tranche spawning

B2 spawned **B3 (parser-baseline restoration) and B4 (codegen-emission
correctness) mid-execution**, at its own W0.c, when the first regen hit parser
hangs (`docs/tranches/B2/PROGRESS.md:113-118`). B2's W0.c then had to "re-execute
against the post-B3 + post-B4.W0 substrate" (`B2/audit/W0c-close.md:3`). The B-era
alone spawned eight tranches (B0–B7 all exist as directories). This is scope
fractal-branching: a tranche that cannot close forks children, which fork
grandchildren; the `FusedBuilder` debt alone was routed B2 → B4.W1 → AY-II.W0'
polish (`B2/FINAL.md:130-131, 164`).

### The "theatrical vs load-bearing" audit — a rare honest correction

One genuinely healthy meta-process artifact: the 2026-04-25 deep audit (AUDIT-B)
found that the AY-II.W0' close ceremony contained **three theatrical steps**
(cache-cleared cycle-2, a fat-LTO 5-bench matrix, per-grammar samply at W0' close)
and **one load-bearing step** (cycle-1 regen + invariant verification), and
compressed the ceremony from ~15 theatrical minutes to its load-bearing core
(`B2/PROGRESS.md:29-34, 89-99`). AUDIT-C likewise proved that AZ-I.W0's prescribed
derive-cache-relocation + Watt mechanisms "do NOT reduce the cold-miss expansion
wall" and retire entirely once B2 removes the proc-macro (`B2/PROGRESS.md:35-41,
100-103`). This is the process working: an audit deleting speculative future work
rather than executing it. SK-V21 should preserve this instinct.

### Doc/code inversion risk

B1 waived its research wave and opened "directly as an implementation annex"
(`B1.md:28`) on the strength of pre-committed planning docs, then discovered on
the live host that "B1's planned surface is not yet live in the checkout" —
`rust-toolchain.toml` absent, nextest not installed, four aliases not live, the
Makefile missing targets (`B1.md:31-49`). The plan described a surface that did
not exist; a preflight-truth pass caught it before dispatch. The lesson: planning
docs drifted ahead of the checkout, and only an explicit "does this command
resolve on the current host" gate prevented a false dispatch.

---

## (e) Worth resurrecting for the totality plan

1. **B2's build-time codegen transposition, in full.** `cargo xtask regen` →
   per-grammar checked-in source → marker-struct consumer contract → portable
   `include_str!` via `concat!(env!("CARGO_MANIFEST_DIR"), "/../../", <rel>)` →
   `regen --check` CI+pre-commit drift gate. This is a correct, idiomatic Rust
   architecture and it is live in `crates/` today (`B2/FINAL.md:37-86`). The
   AUDIT-D decision that chose it — T3 (xtask + checked-in generation) over T1
   (build.rs codegen), T2 (pre-serialised IR), T4 (msgpack blob + thin proc-macro)
   on simplicity + idiomatic fit + total elimination of the proc-macro contract —
   is a reusable design record (`B2/PROGRESS.md:38-41`).

2. **The ICE diagnosis and the negative result it encodes.** Ambient nightly +
   incremental cache + a stale-prone codegen cache + parallel front-end = an ICE
   storm that is invisible until you count files at the repo root. Pin the
   toolchain from day one; never ship a codegen cache whose invalidation shifts
   downstream `AttrId` ordering. The full forensic chain at
   `TOOLCHAIN-SOTA.md:24-167` should not be re-derived.

3. **The frozen toolchain SOTA decision ledger** (`TOOLCHAIN-MIGRATION.md:24-151`):
   divan over criterion (criterion is warm-first by architecture, conflicting with
   cold-per-parse) and over bencher (no allocation counting, no JSON); nextest
   required; `-Zthreads=8` and `-Zshare-generics=y`; cranelift dev-only; lld as
   opt-in on macOS with the corrected `brew install lld` path (not `llvm`,
   `TOOLCHAIN-MIGRATION.md:366-392`). The **rejection rationales are the valuable
   part** — sccache cannot cache proc-macro crates and bbnf-derive was exactly the
   crate dominating the cold wall (`TOOLCHAIN-MIGRATION.md:127-131`); watt was
   rejected as a 2–4-week structural change (`:133-138`). Do not re-evaluate these
   without new cause.

4. **The load-bearing NEGATIVE result that must not be re-learned.** The
   build-time transposition **alone does not solve generated size**. A 200 K-line
   (11 MB) per-grammar Rust file compiles slowly whether it is emitted by a
   proc-macro or read from disk. SK-V21's stack-abrogation axis must confront
   generated-code size as a first-class target — the divergence was not merely the
   proc-macro delivery mechanism, it was the decision to emit hundreds of thousands
   of lines of Rust per grammar. Relocating that emission (B2) bought a real win
   (80 min → seconds of expand) and left the deeper wall (200 K-line compile)
   untouched.

5. **The `FusedBuilder::finish` open-frames debt, still unclaimed.** 327 tests
   failing on `crates/tape/src/builder/mod.rs:1066` in debug builds, release-green,
   routed B2 → B4.W1 and never resolved in this corpus (`B2/FINAL.md:115-131`). Any
   totality plan that inherits the tape crate must either fix or formally retire
   this class rather than continue the "release-green, debug-red, not my scope"
   normalisation.

---

## Verdict

B0 and B1 were two infra annexes that, between them, produced one durable good
(the pinned toolchain + ICE recovery) and one over-claimed close (a cold-per-parse
harness that never measured cold, an iteration ceiling renamed rather than met).
B2 was the real work: it correctly identified that codegen-as-proc-macro was the
80-minute build wall and transposed it to build-time source on disk — an
architecture live in `crates/` today. But B2 also demonstrates the era's signature
failure: it relocated a cost without shrinking it (200 K-line generated files
persist), it spawned two child tranches mid-flight, and it closed atop 327 failing
tests it routed forward. For SK-V21: keep the xtask transposition and the toolchain
pin; attack generated-code size head-on; and refuse the annex-recursion and
debt-routing habits that turned a two-week tooling detour into an eight-tranche
subtree.
