# CRIT-PB-2 — The x86-purge build probe, adversarially read

Critique of `prototypes/PROTO-PB-2.md` against the codex section it tests:
SPEC-DRAFT §2 (the x86 abrogation), specifically §2.1 census, §2.2 four-layer
enforcement (C.1 emitter guarantee, C.2 token gate), §2.3 ownership +
transitional allowlist, and the TB1.W3 / TB2.W2 / TB3.W1 landings. Verdict of
this critique: PB-2's NARROW claim is honestly evidenced and its verdict
follows; but it proves the abrogation on the LEGACY surface the plan DELETES,
and both its headline "scoping refinements" are weaker than framed. Net
convergence for §2: **88%.** One genuine open design question survives.

## What I independently verified (all PASS — PB-2's census is grounded)

- **Emit sites real, x86 arms live on master.** `support.rs` has the AVX2
  dispatch arms at `crates/core/src/backend/rust/emitter/shapes/dispatcher/support.rs:541,595,644,683`
  (`#[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]` + `use
  core::arch::x86_64::*` at :598,:688). Regex emitter: the `'avx2_scan` block
  at `crates/core/src/generate/regex/emit/simd.rs:140-142`, portable
  complement at :216. SPEC §2.1's line refs (:541-545,:595-598,:644-645,:683+;
  simd.rs:133-142) are ACCURATE.
- **Generated stamps real.** 44 `target_arch = "x86_64"` lines across
  `grammar/generated/*.rs`; bbnf.rs carries 22 x86/`_avx2` grep hits, the other
  eight 10 each — consistent with PB-2's ~310-stamp / 891-deletion figures.
- **docs-archive AVX2 bodies real + non-compiled (finding 2 accurate).**
  `docs/tranches/AZ-II/archive/json-prototype/src/simd.rs:238`
  `nospace_bitmap_64_avx2`, `:349` `first_quote_or_backslash_avx2`, x86
  dispatch at :144/:296. Grep of root + skinny `Cargo.toml`: zero references →
  NOT a workspace member → non-compiled. Confirmed.
- **avx512bw residue real** at `skinny/crates/passes/src/lib.rs:380` (field
  :370, consumer :960, test seed :1944) — matches research/01's :380.
  **Discrepancy:** PB-2 finding #3 cites "live :374"; live master is :380.
  A 6-line drift (PB-2's worktree HEAD `d2b32f6e3` predates master
  `400b7a537`). Immaterial to the residue's existence; flagged for honesty.

## The load-bearing critique: PB-2 proves the abrogation on a tree the plan DELETES

The probe worktree diff (verified via `git status` on
`.claude/worktrees/wf_55cb3d1f-cb8-11`) touches exactly:
`crates/core/.../support.rs`, `crates/core/src/generate/regex/emit/simd.rs`,
`crates/simd-scan/*`, and the eight `crates/core/src/grammar/generated/*.rs`.
Per SPEC §2.1 (:112-117) EVERY one of these "DIE WITH THE ARCHIVE... all live
in `crates/`, which `git rm`s at TB1 [W2]. Deletion by archival; **no
transposition**." PB-2 performed precisely the transposition the plan forbids
on the legacy tree — it patched the legacy emitter and regenerated the legacy
stamps. The plan does not fix these; it removes them, and re-delivers C.1
through the GREENFIELD TB3 emit spine (§2.2 point 1, :144-147; §2.3 :170 "TB3
delivers C.1").

Consequence for what PB-2 actually PROVES vs what the codex may read into it:

- **PROVEN + transferable:** the regen→drift-gate MECHANISM works end-to-end
  (emitter change → `regen` purges → `regen --check` forbids regrowth), and the
  aarch64 NEON + scalar arms round-trip **byte-identical** without the x86 arm
  (zero-insertion regen diff). This genuinely de-risks TB3.W1's obligation to
  reproduce byte-identical aarch64 output. Credit it.
- **NOT proven (must not be read as C.1 discharged):** that the TB3 greenfield
  emit spine — ~1,800-2,200 NET-NEW LOC that does not yet exist (§1 table,
  TB3.W1 :269) — will itself never emit the x86 token. That is TB3's fresh
  obligation; PB-2's legacy-emitter proof asserts, does not establish it.
- **NOT proven:** that the graduated skinny-ROOT tree builds green. PB-2 built
  the pre-graduation `crates/` tree with simd-scan abrogated IN PLACE, not the
  post-`git rm` skinny-as-root tree TB1.W2 produces. Different build entirely.

## Both headline "scoping refinements" are weaker than PB-2 frames them

PB-2's bottom line demands two refinements "before the C.2 gate reads green."
Adversarially, both are pre-graduation artifacts largely outside the SPEC's
post-graduation C.2 scope:

1. **Finding #1 (gorgeous wasm32 gate) is MIS-ANCHORED.**
   `crates/gorgeous/src/lib.rs:1`'s `#[cfg(not(target_arch = "wasm32"))]`
   mimalloc gate is real — but `gorgeous` lives under `crates/`, which TB1.W2
   `git rm crates/` DELETES wholesale, and the C.2 gate is planted at TB1.W3
   AFTER removal (§3 TB1 wave order, :204). By the time C.2 runs, the cited
   file is gone. I confirmed the graduated skinny tree has NO surviving wasm32
   cfg gate (`grep target_arch="wasm32"` over `skinny/` → only the mimalloc
   dep line in Cargo.toml, no `#[cfg]`, no `#[global_allocator]`) and no
   `gorgeous` dependency. PB-2's cited evidence does not survive graduation.
   **BUT the underlying design point has merit** (see open question below):
   the C.2 token set literally lists `target_arch = "wasm32"` (§2.2 :151),
   and skinny already carries the mimalloc dep — it will likely re-acquire the
   idiomatic wasm32 allocator gate (bbnf ships WASM/playground targets). So the
   over-broad-token risk is real; the anchoring file is not.

2. **Finding #2 (docs archive) is ALREADY out of C.2 scope by design.** SPEC
   §2.2 (:148-149) scopes C.2 to "`git ls-files` over src + generated +
   emitters" — which excludes `docs/**`. PB-2's own gate script scoped to
   `crates/**/src + generated` and found the docs survivor ONLY via a separate
   FULL-TREE grep the plan's C.2 does not perform. "Path-scope out
   `docs/**/archive/**`" is redundant belt-and-suspenders, not a required fix.

## Evidence-quality audit (method / load / cold / verdict-follows)

- **Cold?** No bench was run at all. PB-2 argues runtime delta is zero BY
  CONSTRUCTION (every deleted arm was `#[cfg(x86_64)]`/`#[cfg(wasm32)]` → dead
  on aarch64; zero-insertion regen proves the live arms are byte-identical;
  synthetic parity corroborates). This is a legitimate structural argument and
  a stronger guarantee than a noisy paired bench — I accept it. It means the
  "smoke bench" criterion is satisfied by construction, NOT measured; PB-2 is
  honest ("deferred to TB2"). Good.
- **loadavg-labeled?** Yes — "regen-start 15.5, build-start 11.6 of 18 cores."
  Load is HIGH (86% / 64%). Since no decision figure is quoted, this only taxes
  the 14.4s build time, which PB-2 correctly calls "directional." The build was
  "warm over regen" — a caveat, but build time is not the bench, so acceptable.
- **Parity honesty.** 8 synthetic NEON-vs-scalar tests PASS; the 5
  `*_canonical` tests could NOT run (gitignored `data/{css,json}` absent from
  the worktree — `correctness.rs:29`). PB-2 correctly attributes this to a
  fixture artifact, not a regression, and claims ZERO parity-assertion
  failures. Fair — BUT the honest read is: abrogated dispatch is proven
  byte-identical to scalar on SYNTHETIC shapes only; **real-corpus parity is
  UNRUN.** The codex must not read PB-2 as corpus-parity proven.
- **Verdict follows?** For the NARROW claim (build + regen + token-gate +
  synthetic-parity green on the abrogated LEGACY surface): yes, and confidence
  92% is defensible. As evidence that SPEC §2's design is executable
  end-to-end: optimistic — corpus parity unrun, greenfield-emitter transfer
  asserted, skinny-root build unbuilt.

## The one genuine open design question (blocks §2 from 100%)

**The C.2 forbidden-token set includes bare `target_arch = "wasm32"` (§2.2
:151), which cannot distinguish a wasm32 SIMD gate (forbidden regrowth) from a
wasm32 platform/allocator gate (legitimate).** §2.3's transitional allowlist
(:163-166) names ONLY `passes/src/lib.rs:380` + four bbnf-bench string sites —
it does NOT resolve this ambiguity. The decision — narrow the wasm32 token to
SIMD-module context, OR allowlist platform gates as a class — is unmade. This
is the correctly-surfaced (if mis-anchored) contribution of PB-2 finding #1,
and it must be wave-owned at TB1.W3.

## Residuals, ownership disposition

- **[OPEN DESIGN Q — TB1.W3]** wasm32-token over-broadness (above). The single
  item keeping §2 below 100%.
- **[WAVE-OWNED — TB3.W1]** greenfield emitter x86-freedom is TB3's fresh
  obligation; PB-2 de-risks the mechanism, not the new emitter. §2.3 books it.
- **[WAVE-OWNED — profiling law / TB gates]** real-corpus parity + any paired
  bench require the gitignored `data/` fixtures materialized (PB-2 finding #4);
  the SPEC's per-corpus close gates already imply this but do not explicitly
  book "materialize `data/` before gating." One-line addition warranted.
- **[FLAGGED, not blocking]** `release.yml` x86-target drop is an
  owner-adjudicated policy call (§2.1 :127-128) — appropriately flagged.
- **[MINOR]** PB-2 finding #3 line drift (:374 vs live :380); finding #5 (git
  rm vs rm atomicity) is already handled if each TB1 wave commits.

## Convergence

**88%.** §2's census is fully evidence-grounded (every line ref verified),
the four-layer enforcement is sound, ownership is clean (TB1.W3 plant → TB2.W2
empty → TB3 deliver C.1). PB-2 validates the load-bearing regen-drift
mechanism and proves the aarch64 path is pure-subtraction byte-identical —
real value. The 12-point gap: (a) the wasm32-token over-broadness is a live
undecided design question, not yet wave-owned in §2.3; (b) PB-2's PROVEN
verdict rests on the LEGACY emitter the plan deletes — the greenfield TB3
emitter's x86-freedom and the skinny-root build remain unproven by this probe;
(c) both headline scoping refinements are pre-graduation artifacts, so the
codex must not spend scope "fixing" them.
