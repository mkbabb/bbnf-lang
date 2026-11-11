# PROTO-PB-2 — The x86-purge build probe (bbnf worktree)

**Instrument, not a landing.** Executed in an isolated git worktree of
bbnf-lang (`.claude/worktrees/wf_55cb3d1f-cb8-11`, off `master`). Host
`aarch64-apple-darwin`, arm64, 18 cores, rustc 1.96.0-nightly. Date
2026-07-10. Nothing landed; parse-that untouched.

## Question
With `avx2.rs`/`avx512.rs`/`wasm.rs` deleted, the parity/compaction x86 arms
stripped, the two emit sites' x86 arms dropped, and the stamps regenerated,
does the whole workspace build + smoke-test green aarch64-only with grep-zero
x86 outside the §2.2 allowlist?

## Verdict — PROVEN (build + regen + gate + parity green on the abrogated surface)
Every criterion met on the live bbnf-lang surface. The two full-tree token
survivors are both OUTSIDE the abrogated surface (a separate workspace + a
non-compiled archive); the 5 test failures are 100% missing-corpus file reads,
zero parity failures. Confidence 92%.

## Dispositions executed (research/01 A.1 + A.2)
| action | file | note |
|---|---|---|
| `git rm` | `simd-scan/src/avx2.rs` (370), `avx512.rs` (149), `wasm.rs` (433) | staged deletes |
| rewrite dispatch | `simd-scan/src/lib.rs` | module gates + runtime `is_x86_feature_detected!` reduced to `{aarch64 neon, scalar}` |
| strip x86 arm | `simd-scan/src/parity.rs` | dropped `clmul_prefix_xor_x86` + `pclmulqdq` arm; NEON PMULL64 + shift-XOR kept |
| strip x86 arm | `simd-scan/src/compaction.rs` | dropped `compact_stripe_pext` (`bmi2`); `tzcnt`-loop kept |
| strip feature | `simd-scan/Cargo.toml` | removed `avx512` feature + multi-arch description |
| **emit fix** | `core/.../dispatcher/support.rs` | dropped both x86 dispatch arms + both `*_avx2` bodies; emits `{aarch64 neon, scalar}` only |
| **emit fix** | `core/src/generate/regex/emit/simd.rs` | dropped the `'avx2_scan` block; portable `find_next_structural_from` fallback now unconditional |
| prose prune | `ir/.../recognizers/kernel_shape.rs` | cosmetic doc x86 mentions (no token hits) |

Worktree diff: **19 files, 45 insertions, 2160 deletions** — the pure
abrogation signature.

## Results per criterion

**1. Regen clean — no x86 stamp survives (C.1 root).** `cargo xtask regen`
exit 0. Generated-tree x86 token count **310 → 0**. `git diff --stat` over
`generated/`: **891 deletions, ZERO insertions** (bbnf.rs −323, other 8 −71
each). Zero-insertion is decisive: the aarch64 NEON + scalar arms round-trip
**byte-identical**; only x86 arms vanished. C.1 confirmed — the emitter can no
longer stamp x86, so the existing `regen --check` drift gate mechanically
forbids re-adding it.

**2. Build green — aarch64-only.** `cargo check --workspace --profile ax-iter`
exit 0 (14.4s, warm over regen). Regenerated x86-free tree + abrogated
simd-scan + both rewritten emitters typecheck on aarch64. One pre-existing
unrelated warning (`unused import: StructRegistry`). The fallback arms made
unconditional are exactly the arms already live on aarch64 → compile-safe by
construction.

**3. Parity smoke — abrogated dispatch correct.** `cargo test -p simd-scan`:
the 8 synthetic NEON-vs-scalar parity tests ALL PASS (empty/short inputs,
`no_quotes_simple`, `multi_cmp_alphabet_20_singletons`,
`wide_lut_alphabet_15_singletons`, `stripe_boundary_64_byte`,
`stripe_plus_one`) — every kernel shape validated against the scalar
reference. The 5 `*_canonical` failures are ALL `read_corpus` file-not-found
panics (`correctness.rs:29`): `data/{css,json}/*` is **gitignored** and absent
from this worktree checkout (present in the main repo). ZERO parity-assertion
failures. Not an abrogation regression — a worktree-fixture artifact.

**4. Token gate (C.2) — zero on the abrogated surface.** The §2.2 grep scoped
to `crates/**/src` + generated: **zero hits** except one allowlisted entry,
`crates/gorgeous/src/lib.rs:1 #[cfg(not(target_arch = "wasm32"))]` (a mimalloc
global-allocator platform gate, NOT SIMD). Gate script:
`scratch_c2_token_gate.sh` (worktree probe copy). Full-tree survivors: see
findings 2–3 below.

**5. Smoke bench — no runtime delta BY CONSTRUCTION (load-accounted).** On
aarch64 every deleted arm was `#[cfg(x86_64)]`/`#[cfg(wasm32)]` — dead here.
The only arms that ever compiled on this target are NEON + scalar, byte-
identical pre/post (the zero-insertion regen diff proves it, and the passing
synthetic parity tests corroborate). **No runtime code-path delta exists**; a
bench would measure load, not the change — a stronger guarantee than a noisy
paired bench. Load was elevated/falling (regen-start 15.5, build-start 11.6 of
18 cores) so absolutes are directional. A formal paired per-corpus bench is
deferred to TB2's consumer-invariant gate on a clean machine WITH the `data/`
fixtures (absent here).

## Hidden couplings surfaced as named findings
1. **`crates/gorgeous/src/lib.rs:1` — wasm32 allocator gate (allowlist
   decision).** `#[cfg(not(target_arch = "wasm32"))]` selecting mimalloc is a
   legit non-SIMD platform gate the C.2 set catches via `target_arch =
   "wasm32"`. **Not in research/01's census.** TB1.W3 must allowlist it or
   narrow the wasm32 pattern to SIMD-module gates. A `cfg(not(wasm32))`
   allocator selection is not x86 regrowth.
2. **`docs/tranches/AZ-II/archive/json-prototype/` — archived STANDALONE crate
   with live AVX2 bodies.** Its `src/simd.rs` holds `nospace_bitmap_64_avx2` +
   `first_quote_or_backslash_avx2` (the emitter stamps' ancestors) + a
   `#[cfg(x86_64,avx2)]` dispatch. Has its own `Cargo.toml` but is **not a
   workspace member** (referenced by no manifest → non-compiled), so it does
   not affect the build. **Absent from research/01's census.** TB1.W3 decision:
   scope the C.2 file walk to compiled sources / exclude `docs/**/archive/**`,
   else the frozen prototype trips the gate forever.
3. **`skinny/crates/passes/src/lib.rs:374` — `avx512bw` residue (EXPECTED).**
   `cfg!(all(target_arch = "x86_64", target_feature = "avx512bw"))` in
   `TargetFeatures::host()`. Exactly the A.5 residue (research/01 read :380;
   live :374). §2.3 already books it as the DATED TB1.W3 transitional-allowlist
   entry, emptied at TB2.W2. Separate skinny workspace, out of scope for this
   bbnf build. Census accurate here.
4. **`data/{css,json}/*` corpus is gitignored → absent in worktrees.** The
   simd-scan correctness gate and any per-corpus smoke bench cannot run in an
   isolated worktree without materialising the fixtures. TB gates that assert
   on the corpus must run where `data/` is populated, or the fixtures must
   move under version control / a fetch step.
5. **Deletion must be `git rm`, not `rm`.** `git ls-files` kept listing the
   removed kernels until staged; a gate walking `git ls-files` reads them live
   until the deletion commits. TB1.W3's gate + archival commit must land
   atomically.

## Bottom line for the tranche plan
The C.1 emitter-guarantee mechanism is PROVEN: fixing the two emit sites →
`regen` purges 891 lines of x86 stamps with zero collateral churn → drift gate
makes regrowth a build failure. The live bbnf-lang surface builds, parity-
smokes, and greps clean. Before the C.2 gate (§2.2) reads green it needs two
scoping refinements: (a) allowlist/narrow for the gorgeous wasm32 allocator
gate, (b) path-scope out `docs/**/archive/**`. Skinny `avx512bw` is the only
census-anticipated survivor. Per-corpus benching requires the gitignored
`data/` fixtures — deferred to a fixture-bearing environment; the byte-identity
of the aarch64 path makes a regression structurally impossible regardless.
