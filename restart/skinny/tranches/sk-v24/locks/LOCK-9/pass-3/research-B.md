# LOCK-9 · Family 9B — NATIVE-REBIRTH (parse-that keeps its own kernels)

Charter: parse-that's scan/kernel layer reborn in **idiomatic stable NEON, homed in
parse-that**, each kernel carrying a scalar parity oracle and checkasm-liveness, no
cross-repo rlib coupling; the gate is measured **no-regression** per the PROVE-09
refutation. This report develops that route to its strongest concrete form. Host is
aarch64-apple-darwin (Apple M, Darwin 25.4.0); x86 is dead (constitution iv). All
parse-that paths are absolute-relative to `/Users/mkbabb/Programming/parse-that/`; bbnf-simd
to `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-simd/`. parse-that is READ-ONLY;
every landing rides a scratch copy (R3 §8).

---

## 1. The mechanism, made concrete: the two lawful movemask producers

The PROVE-09 refutation is the load-bearing fact. The de-nightly shim's uniform
`to_bitmask` (`simd128.rs:322-330` in PROVE-09 patch.diff) packs lane→bit with a
bit-position `vandq` + **two `vaddv_u8` halves** — the *double-addv*. Adversarial rebuild
disproved the 1.34-1.57× claim (parity ±15%) and showed the shim emits **2271 `addv.8b`
vs 509 baseline** while `tbl` was 516≡516 — the shim does *more* reduction work, never
less. LAW: **stable NEON must be measured no-regression; `vshrn` narrow, never the
double-addv.** A census of parse-that today confirms the disease is systemic and the cure
is entirely absent: `grep vshrn` over `parsers/scan/` + `scanners.rs` returns **zero**;
every producer routes through the one refuted `to_bitmask`.

Native-rebirth's mechanism is a **contract-split** of the movemask into two lawful
producers, authored fresh into a parse-that-owned `simd/` module (colocated with the
scanners, no sibling rlib), each keyed to what its *consumer* actually needs. The split is
not aesthetic — it maps 1:1 to the consuming code (§2), and conflating them is precisely
what the refuted shim did.

**LEAN — nibble-`vshrn`** (for consumers that only want the first set position):
`cmeq.16b` → `shrn.8b vd, vn, #4` reinterpreting the 16×`u8` compare result as 8×`u16` and
narrowing each 16-bit lane to its high nibble → `fmov x, d` → a 64-bit word with **4 bits
per input lane**. The consumer does `trailing_zeros() >> 2` for the first match index. This
is the Langdale/Kutenin "beating SSE movemasks on AArch64" idiom and exactly what memchr's
NEON backend emits — PROVE-10 already disassembled it live in-tree
(`shrn.8b v0,v0,#4; fmov x8,d0; ands x8,#0x8888888888888888`, PROVE-10 REPORT §2c). One
`shrn`, no `addv`, no per-lane gather.

**DENSE — `vsli`/`vpaddq` tree** (for consumers that need an *exact* 1-bit-per-byte 64-bit
bitmap): the simdjson arm64 `neon_movemask_bulk` construction — take the four 16-lane
compare vectors of a 64-byte stripe, AND each against a repeating `[0x01,0x02,…,0x80]` bit
vector, then reduce the four with a `vpaddq_u8` (add-pairwise) tree collapsing 64 lanes ×
1 bit into one `u64`. Cost is ~1 `and` + `addp` chain per stripe, amortised over 64 bytes;
no `addv`, no scalar packing loop.

Both intrinsics — `vshrn_n_u16`, `vpaddq_u8`, `vsli_n_u8`, `vqtbl1q_u8`, `vld1q_u8`,
`vceqq_u8` — are **stable `core::arch::aarch64` since Rust 1.61**; no `portable_simd`, no
nightly. The reborn kernels are unsafe fns (NEON is aarch64 baseline) behind safe wrappers.

Note bbnf-simd's extant `movemask_u8x16` (`src/aarch64/movemask.rs`) is **not** the
canonical LEAN producer: it starts from `vshrn_n_u16::<4>` correctly but then runs a
`vand`/`vsri`/`vzip1q` + **scalar `for pair in 0..8` packing loop** (lines 17-24) to emit a
compact u16 — a gather tail no hot path wants. The directive's premise holds: the canonical
producers are absent from *both* homes and must be authored once regardless of ownership.

---

## 2. Consumer census — the contract-split is dictated by call sites

Every `to_bitmask` / movemask consumer in parse-that, classified by the contract it needs
(verified by grep + Read):

**LEAN (tzcnt-only → nibble-vshrn is exact-enough):**
- `span_parser/scan.rs:152` — `is_excluded.to_bitmask().trailing_zeros()`
- `scanners.rs:208` — `(!mask).to_bitmask().trailing_zeros()` (first non-ws)
- `scanners.rs:286` — `is_match.to_bitmask().trailing_zeros()` (`find_first_of_nibble_lut`)

**DENSE (exact bit positions fed downstream — a nibble mask CORRUPTS them):**
- `parsers/scan/structural_bitmap.rs:43,57` — `classify_16` returns `to_bitmask() as u16`,
  `classify_stripe` shifts four into a u64 (`mask |= m << (k*16)`). The canonical DENSE-64
  site.
- `parsers/scan/quote_parity.rs:81-82` — `q`/`b` = quote/backslash `to_bitmask() as u64`
  over a STRIPE=64 window, consumed by `prefix_xor` (clmul, `:41`) and escape-parity.
- `parsers/scan/quoted_simd.rs:45-46`, `parsers/scan/decode.rs:82-83,189-190` —
  quote/backslash `to_bitmask() as u32`, feeding `odd_parity_backslashes` → in-string bitmap.
- `parsers/scan/ws_comment.rs:81,150,247`, `scanners.rs:163` — comment `/` and ws bitmaps
  assembled into u16/u64 words.

**No-go / abrogate:** `parsers/scan/number_simd.rs` — see §3.

**Vector-tier (no movemask, pure lane ops):** `decode.rs store_chunk` (`u8x16`→unaligned
16-B arena store, `:227`) — a straight `vst1q_u8`, ports with zero movemask concern.

Count: **3 LEAN · 6 DENSE · 1 no-go · 1 pure-vector**. This answers F1's owed scanner-file
census: of the 8 `portable_simd` files, one is the canonical DENSE-64 kernel
(structural_bitmap), five are DENSE-16/64 (decode, quoted_simd, quote_parity, ws_comment,
scanners-partial), three sites are LEAN (span_parser/scan, scanners-partial, number's dead
site), `state.rs` (2 hits) is comments on `PaddedView` not a kernel, number_simd is no-go.
Kernel-tier (movemask-bearing) vs vector-tier (lane-only) splits ~9:1 by site.

---

## 3. `number_simd.rs` is triple-refuted — abrogate, do not rewrite

`count_leading_digits_neon` (`number_simd.rs:29-61`) embodies **three** refutation-law
violations at once: (a) it is **digit-scan SIMD** — SPEC §0 / constitution-iv law "no
digit-scan SIMD"; (b) it uses the **double-`vaddv_u8`** movemask (`:54-55`,
`lo_sum | (hi_sum<<8)`) — the PROVE-09 forbidden idiom verbatim; (c) it carries a live
**x86 SSE4.2** path with runtime `is_x86_feature_detected!("sse4.2")` (`:145-235,:222`) —
dead by constitution iv. Per abrogate-before-patch, native-rebirth **deletes** the entire
SIMD surface of number_simd and keeps only the scalar SWAR fold (`parse_eight_digits`, three
multiply-shift ops, called at `:76-81`) that already backs the 9..15-digit and short-run
paths. This is the cleanest rebirth move in the plane: a deletion that removes ~200 LOC and
two refuted patterns, the scalar SWAR retained as the sole path (its own parity oracle by
construction). The `number_fastpath_test`/`number_simd_test` suites (33 tests, R3 §7) are
the deletion's regression floor.

`quote_parity::prefix_xor` (`:41-68`) is the *opposite* case: its aarch64 `vmull_p64`
carry-less-multiply path (`:51-56`, gated `target_feature="aes"`) is a **legitimate keep** —
polynomial-multiply prefix-XOR is the correct simdjson quote-parity kernel, not a movemask.
Rebirth purges only its x86 `_mm_clmulepi64_si128` twin (`:42-49`) and keeps the 6-shift
scalar fallback (`:58-68`) as the parity oracle.

---

## 4. F2 — stable toolchain + the correctness oracle

**Stable build blast radius.** Three nightly blockers (R3 §2), all removable without a home
change: `portable_simd` (8 files) dissolves the moment the scanners call the native `simd/`
producers instead of `std::simd` — no shim, the producers *are* the vocabulary;
`stmt_expr_attributes` is already dead (excised in PROVE-09); `smallbox` nightly →
`smallbox!` macro is alloc- and codegen-neutral (`parse.rs:74`, PROVE-09 confirmed inline
S32 identical). The **sole residual** stable blocker is `fmt_helpers_for_derive`, forced NOT
by any kernel but by `src/regex/generated.rs` emitting the *expanded* `#[derive(Debug)]`
(`debug_tuple_field1_finish`). Per clean-regen it is a generator fix (emit `#[derive(Debug)]`
not its expansion + `scripts/bootstrap-regex.sh`), outside the kernel plane — 9B's kernel
rebirth reaches full `core::arch` stable with that one generator regen as its only external
dependency. Evidence that own-home native NEON already builds: `number_simd.rs:29-136` is a
live `core::arch::aarch64` kernel today.

**The differential oracle.** Mirror bbnf-simd's checkasm harness, which parse-that wholly
lacks (parse-that has ~400 functional tests but no asm-liveness or bit-parity oracle).
bbnf-simd supplies the exact protocol to copy: `tests/checkasm_common.rs` — `Xorshift64`
deterministic fill (`:3-31`), `with_stack_canary_xor_fold` (1024-B canary, pre/post xor-fold
clobber check, `:50-72`), and `callee_saved_register_then` (writes sentinels into x19-x28
via inline asm, asserts the NEON kernel preserves them, `:85-112`). Each reborn producer gets:
1. a bit-exact parity assert against a `[u8;16]`/`[u8;64]` scalar reference over `black_box`
   xorshift inputs across an **alignment sweep** (offset 0..64 within the padded view) —
   bbnf-simd's `corpus_parity.rs`/`classifier_parity.rs` are the template;
2. an **INJECT_BUG** perturbation (flip one lane's compare) that must make the parity assert
   fail — proving the oracle has teeth;
3. checkasm-liveness (canary + callee-saved) wrapping the call.

**The parity counterexample (F2's decisive test).** A naive nibble-stride form on a DENSE
site is not merely slower — it is **wrong**. The DENSE quote/backslash bitmap feeds
`prefix_xor` (clmul over the u64 quote mask) and `odd_parity_backslashes`; a nibble mask
places each lane's bit at stride-4 instead of stride-1, so the carry-less multiply computes
the in-string region over the wrong bit lattice → the escape-parity bitmap diverges → JSON
string decode corrupts. The oracle must include a targeted case: feed the nibble producer
into the quote-parity site and assert the emitted in-string bitmap **differs** from the DENSE
reference on an input with an odd backslash run (e.g. `"\\\""`). This is the concrete "does a
naive nibble-stride form on the dense-64 site corrupt parity? — **yes**" evidence F2 demands,
and it is the architectural justification for the two-producer split being a correctness
contract, not an optimisation.

---

## 5. F1 — the two-home residual (answered from the own-kernel side)

9B's stance is **own-home, physical**: a parse-that `src/simd/` directory module (colocated
with `parsers/scan/`), no bbnf-simd rlib dependency. After authoring the canonical LEAN/DENSE
producers into a scratch shared home vs the parse-that home, the residual substrate
divergence is small but real:

- **padded-view / clamp convention.** parse-that: `INPUT_PAD_BYTES=64` trailing-NUL tail
  (`state.rs:57`, `PaddedView` `:122`) lets every stripe load skip a tail guard — the NUL
  region classifies as non-match. bbnf-simd: an explicit `eob_pad_clamp` kernel masks off-end
  lanes. Unifying forces one convention on both; parse-that's is strictly cheaper for its hot
  loop (no per-stripe clamp op) and is the reason its scanners are branch-free. A genuine
  divergence cost against one-home.
- **shuffle table arity.** parse-that classification is single-table nibble-LUT (`vqtbl1q_u8`,
  hi/lo `[u8;16]` luts — `find_first_of_nibble_lut`, `structural_bitmap` classify_16).
  bbnf-simd `classify_tbl4.rs` is a 4-table `tbl4` classifier. Different strategies → a shared
  home must carry both or force a convergence.
- **capacity constants.** parse-that `STRIPE=64`/`CHUNK=16`; bbnf-simd all `*_64`. Nominally
  aligned, differently named.

**The non-inlinable boundary (F1 middle Q, precisely).** `#[inline]` crosses the rlib seam
for `uint8x16_t`-taking movemask producers **only** when the callee carries an explicit
`#[inline]`/`#[inline(always)]` attribute — Rust does **not** cross-crate MIR-inline a
non-generic, non-`#[inline]` fn under the default `codegen-units>1` without workspace
`lto`/`embed-bitcode`. bbnf-simd's `movemask_u8x16` is `#[inline(always)]` (crosses); but any
producer authored without that attribute, consumed across the crate boundary under
`[profile.dev]` (cgu=256, no LTO), becomes a real `bl` call passing a NEON register through
the ABI — a measurable in-loop direct call. `[profile.bench]` (fat-LTO, cgu=1, R3 §6) masks
it entirely. So the seam is real but attribute-gated: one-home is a robustness argument (the
seam cannot regress if the boundary does not exist), not a bench-profile perf argument. The
maintenance/drift basis: two homes means two checkasm harnesses + a manual sync-discipline;
one home means one clamp convention forced on both consumers. 9B judges the own-home cost
lower because the clamp/shuffle divergences above are load-bearing performance choices
parse-that loses in a merge, and its scanners already ship a native `core::arch` kernel
proving the own-home path builds.

---

## 6. Prior art (citations)

- **Geoff Langdale, "Fitting My Head Through The ARM Holes" / Danila Kutenin, "Bit twiddling
  with Arm Neon: beating SSE movemasks"** — the `shrn #4` 64-bit-nibble movemask (LEAN
  producer); tzcnt/4 to recover lane index.
- **simdjson arm64 backend** (Lemire, Langdale, Keiser — "Parsing Gigabytes of JSON per
  Second", VLDBJ 2019 + the arm64 port): `neon_movemask_bulk` — the `vand`+`vpaddq` bit-set
  reduction over a 64-byte block (DENSE-64 producer); `vmull_p64` carry-less-multiply
  prefix-XOR for quote parity (validates keeping `quote_parity.rs:51-56`).
- **memchr / regex-automata** (Andrew Gallant): aarch64 NEON `shrn`-based movemask + the
  memchr1/2/3 prefilter ladder (PROVE-10 productionised the latter into `bbnf-regex`).
- **checkasm** (dav1d/x264): callee-saved-clobber + stack-canary liveness verification — the
  pattern bbnf-simd's `checkasm_common.rs` already implements and 9B mirrors.
- **Rust `core::arch::aarch64`**: `vshrn_n_u16`, `vpaddq_u8`, `vsli_n_u8`, `vqtbl1q_u8` stable
  since 1.61.0; `is_aarch64_feature_detected!` for the `aes`/`pmull` gate on the clmul path.

## 7. Named risks & counterexamples

- **Window-asymmetry trap (PROVE-09's exact failure).** Any "faster" claim is presumed false.
  Rebirth's gate is *no-regression* (ratio ≥ ~0.98× on the least-preempted min), not a
  speedup; the DENSE-64 rewrite could plausibly be a hair slower than the shim's double-addv
  on short stripes and still PASS. Two windows, opposite arm order, base-pinned to a stated
  HEAD, mandatory.
- **LEAN on a DENSE site = silent decode corruption** (§4). The split is a correctness
  contract; a single mis-routed producer passes functional smoke tests on escape-free JSON and
  fails only on odd backslash runs.
- **`vpaddq` tree lane-order.** The DENSE-64 assembly must preserve lane→bit order across the
  four 16-lane sub-vectors; an off-by-16 in the pairwise-add tree reverses stripe halves —
  caught only by the bit-exact oracle, not by `any()`.
- **Short-run reality (R3, PROVE-10).** These scanners see short windows; a small-multistate
  `vqtbl1q` chain is a measured negative on short runs. Rebirth must not over-vectorise —
  membership + structural tiers only; the number path *deletes* rather than re-vectorises.
- **`aes`/`pmull` feature gate.** The clmul prefix-XOR requires `+aes`; the runtime-detect or
  build-target must be explicit, else the scalar 6-shift fallback silently takes over (a perf,
  not correctness, cliff).

## 8. What a prototype must test to move family 9B

1. Author `src/simd/{lean.rs,dense.rs}` in parse-that: LEAN `movemask_lean(uint8x16_t)->u64`
   (`shrn #4`, tzcnt/4 contract) and DENSE `movemask_dense_64(&[uint8x16_t;4])->u64`
   (`vand`+`vpaddq` tree). Convert the 3 LEAN + 6 DENSE consumers (§2) to the correct
   producer. **Delete** `number_simd`'s SIMD + all x86 (§3).
2. **Instruction census** (`otool -tv` / `cargo asm`): DENSE sites show `and`+`addp` tree,
   LEAN sites show `shrn.8b …,#4`; **zero `addv.8b` in any movemask producer** (was 2271 in
   the refuted shim). Base-pinned to a stated commit.
3. **Paired cold divan, N≥200, two windows, opposite arm order**, `[profile.bench]` fat-LTO
   identical both arms; gate = no-regression (min-col ratio ≥ ~0.98×) across the JSON+CSS
   corpora — reproducing PROVE-09's *method* so no window-asymmetry win is claimed.
4. **Differential parity oracle** per kernel: bit-exact vs `[u8;N]` scalar reference over
   `black_box` xorshift + alignment sweep + INJECT_BUG; plus the decisive
   nibble-on-DENSE-quote-parity case that **must** show a divergent in-string bitmap on an
   odd-backslash input.
5. **checkasm-liveness** mirrored from bbnf-simd (`checkasm_common.rs`): canary xor-fold +
   callee-saved x19-x28 sentinels around each kernel.
6. **Stable build log**: `cargo +stable build -p parse_that` with the `simd/` producers,
   showing the *only* residual error is `regex/generated.rs` fmt_helpers_for_derive
   (generator-regen scope, not kernel scope).
