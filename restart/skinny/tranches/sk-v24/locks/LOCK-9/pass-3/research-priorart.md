# LOCK-9 pass-3 — PRIOR-ART: the scanner & kernel plane (parse-that rust)

Researcher pass, no family allegiance. Sweeps web + codebase for what exists, what won and
lost elsewhere for THIS problem class (grammar-driven NEON byte-scanners + a movemask-emulated
structural index over a padded window), the constraints our own evidence base already imposes,
and the named traps in comparable systems. Citations inline. REGISTRY deliberately not read.

---

## 0. The problem class, stated

parse-that's scan/kernel layer is a **simdjson-lineage stage-1 byte classifier**: over 64-byte
(4×`u8x16`) windows it (a) classifies bytes into structural/quote/ws/comment sets via nibble-LUT
or `vceqq` fans, (b) reduces each 16-byte compare vector to a bitmask via an **emulated
PMOVMSKB** (ARM has none), (c) stitches four masks into a 64-bit word, and (d) prefix-XORs the
quote mask (carry-less multiply) to resolve in-string regions, then CTZ-iterates to emit
structural offsets. Everything downstream (the combinator parsers) consumes those offsets. The
kernel-sharing question is whether bbnf-lang's `bbnf-simd` and parse-that's eight scanner files
are two copies of ONE library or two natives. Prior art for every one of these organs exists and
has a settled winner-and-loser record.

---

## 1. The movemask — the load-bearing trap, and its resolved idiom (§0/09 is LAW here)

**x86 `PMOVMSKB` has no NEON equivalent.** Two emulation families exist; picking wrong is
exactly the PROVE-09 refutation.

- **The OLD family (`vand` bit-position vector + pairwise-add reduction / `vaddv`/`addp` tree).**
  Geoff Langdale's "Fitting My Head Through The ARM Holes" (branchfree.org, 2019) is the canonical
  survey of the *two sequences* that substitute for the missing PMOVMSKB; the reduction-tree
  variant carries a long dependency chain (AND→pairwise-add serialised). simdjson historically
  shipped `neon_movemask_bulk` amortising **4** compares into one reduction because a single one
  is not worth the tree ([simdjson #309 "losing half the mask"](https://github.com/simdjson/simdjson/issues/309)
  documents that the narrowing is bug-prone — half the mask silently dropped — so a parity oracle
  is mandatory, not optional).
- **The NEW family (`vceqq` → `vshrn_n_u16::<4>` narrow → 64-bit NIBBLE mask).** Danila Kutenin's
  ARM community post ["Porting x86 vector bitmask optimizations to Arm NEON"](https://developer.arm.com/community/arm-community-blogs/b/servers-and-cloud-computing-blog/posts/porting-x86-vector-bitmask-optimizations-to-arm-neon)
  is the authority: `shrn` shifts the 16 `u16` compare lanes right by 4 and truncates to 8 bits,
  yielding a **64-bit mask at 4-bits-per-lane** in ONE instruction, avoiding the 2-cycle AND→addv
  dependency chain. Measured **10–15% on SPEC CPU2017 `strlen`** vs the old `vand`+`addp`. Because
  ARM lacks `ctz`, consumers recover the byte index via `rbit`+`clz` then `>> 2` (nibble stride);
  Kernighan `m &= m-1` still iterates, stride 4. Confirmed independently by Lemire
  ([number-parsing NEON](https://vgatherps.github.io/2022-12-04-dec-2/)) and the sse2neon peephole
  work ([DLTcollab/sse2neon #712 — `vceqq+vshrn` idiom normalization](https://github.com/DLTcollab/sse2neon/issues/712)).

**What our evidence base already fixed.** PROVE-09's `simd128.rs` shim used the OLD family — a
bit-position `vand` + **two** `vaddv_u8` halves — and `otool -tv` showed **2271 `addv.8b` vs
baseline 509** (≈4× the reduction work); the verify fleet measured neutral-to-slower and REFUTED
the 1.34–1.57× headline (PROVE-09 VERIFY attack #2). **This is the constitution's clause iv law:
stable NEON is a measured *no-regression floor*, never a speed claim; vshrn narrow, never
double-`addv`; paired interleaving mandatory (two-arm window asymmetry is the self-deception).**

**Where the fix already lives.** `bbnf-simd/src/aarch64/movemask.rs` ALREADY implements the
CERTIFIED `vshrn_n_u16::<4>` idiom (SPEC §0/03 "fused NEON string scan, vshrn narrow" CONFIRMED
0.898–0.931× cyc/B). **Trap it still carries:** after the correct `vshrn`, it re-packs the nibble
mask back to a dense 16-bit `u16` via a scalar `for pair in 0..8` loop (lines 17–24) — re-serialising
what the ARM blog says to keep as a 64-bit nibble word and iterate at `tz>>2`. The load-bearing
call-site work PT-2.W1 names (~5 consumers migrate their bit-scan to nibble stride) is precisely
what lets that scalar tail die. Any LOCK-9 route that shares this kernel inherits BOTH the win
and the obligation to push nibble-stride semantics to consumers rather than narrow-back.

---

## 2. Classification — shufti/Teddy is the settled winner; bbnf-simd already fields it

simdjson's byte classification is the **PSHUFB nibble-LUT** borrowed from Hyperscan's
**shufti/Teddy** ([Branch Free / simdjson paper](https://branchfree.org/2019/02/25/paper-parsing-gigabytes-of-json-per-second/);
[json_structural_indexer.h](https://github.com/simdjson/simdjson/blob/master/src/generic/stage1/json_structural_indexer.h)).
On NEON PSHUFB → `vqtbl1q_u8` (`tbl`). Validark's ["Eine Kleine Vectorized Classification"](https://validark.dev/posts/eine-kleine-vectorized-classification/)
is the modern survey of high/low-nibble-LUT vs `vceqq`-fan trade-offs. Teddy
([aho-corasick/teddy/README](https://github.com/BurntSushi/aho-corasick/blob/master/src/packed/teddy/README.md))
scans 16/32-byte chunks against precomputed fingerprints — the same shufti family, the same
16-byte cadence. **Our evidence base:** SPEC §0/01 (NEON shufti/truffle prefilter, GATE=16)
CONFIRMED **~7.5×/4.7× skip-heavy** (floor 0.115 cyc/B), dense-neutral. `bbnf-simd`'s
`structural_classify_64` is exactly a simdjson-stage-1 `vceqq` fan with a grammar-NEUTRAL
`KindPartition` (caller data) — the shufti winner already lives here, grammar-agnostic, so it can
serve both repos. Loser named: nothing beats shufti for set-membership; naive per-byte `vceqq`
without the nibble-LUT loses on large alphabets (Validark).

---

## 3. Kernel-sharing (9A) — the dav1d/checkasm model is the precedent; cross-repo is a vendor decision

There is **no mainstream Rust precedent for one SIMD-kernel crate consumed by two repos under a
sync gate** — the search returns only single-repo kernel libraries (`simsimd`,
[`simde`](https://github.com/simd-everywhere/simde) portable emulation, sse2neon). The relevant
precedent is the *methodology*, not the packaging:

- **dav1d / FFmpeg `checkasm`** ([FFmpeg School of Assembly Language, HN](https://news.ycombinator.com/item?id=43140614)):
  a scalar C reference IS the executable specification; every SIMD/asm kernel is parity-checked
  against it across arches. `bbnf-simd` already implements exactly this — `tests/checkasm_parity.rs`,
  scalar reference as spec, NEON body parity-gated, the dav1d correctness anchor cited in its own
  `prim` docs. This is the strongest possible validation of the ONE-KERNEL-LIBRARY family's
  *quality bar*.
- **The cross-repo form is already in-flight in THIS repo** by a different mechanism: `csp-solver`
  is **vendored from the sibling `csc411` repo under a sync gate** (git log: "re-vendor src/ from
  csc411@… (T3-W5+W6)"). That is the physical-isomorphism template 9A would reuse — vendored
  copy + sync gate + canonical-diff commits — not a novel invention.

**Constraint the base imposes:** parse-that consumes `bbnf-simd`? No — R3 confirms parse-that's
scanners are `portable_simd` self-contained and parse-that is READ-ONLY to the bbnf campaign; any
sharing is bbnf-lang→parse-that via the letter, or a jointly-vendored third home. The kernels are
already grammar-neutral on the bbnf side (`KindPartition`, `StructuralAlphabet` = caller data),
which is the precondition for sharing; the blocker is packaging + the placement law, not design.

---

## 4. Scanner-as-DFA (9C) — prefilter/DFA wins for literals, but does NOT subsume the quote-parity kernel

The regex-continuum dissolution has real prior art and a real boundary:

- **Prefilter over DFA is the settled accelerator.** aho-corasick uses Teddy/memchr as a
  **prefilter** to accelerate DFA search ([aho-corasick DESIGN.md](https://github.com/BurntSushi/aho-corasick/blob/master/DESIGN.md));
  Hyperscan's shufti IS NFA/DFA acceleration. Our own PROVE-10 / SPEC §0/10 (bbnf-regex
  `Dfa::find` prefilter tier) CONFIRMED **8.8–29.5×** on prefilterable patterns, compile-neutral,
  44/44 — so ws/keyword/delimiter scanners CAN lower to compiled DFA+prefilter products of the
  bootstrapped grammar.
- **The boundary (the trap for a naive 9C).** simdjson's stage-1 is NOT a regex DFA: the
  **quote-parity prefix-XOR** (carry-less multiply `vmull_p64`/`pclmulqdq`) and the
  **backslash-escape odd-parity** mask are bit-parallel algorithms with cross-block carry, not
  automaton transitions. A DFA cannot express "toggle in-string state by XOR-scan of the quote
  mask" cheaply. [JSONSki's bit-parallel fast-forward](https://dl.acm.org/doi/pdf/10.1145/3503222.3507719)
  and simdjson streaming ([#1361](https://github.com/simdjson/simdjson/issues/1361)) keep these as
  dedicated kernels precisely because they beat character-automaton scanning. So 9C wins for the
  *literal/ws/token* scanners and must **carry** the prefix-XOR quote/escape kernels as
  irreducible primitives — dissolving them into a general DFA would regress the JSON hot path.

---

## 5. Streaming / lazy surface (9D) — real design center, but two of our laws fence it

Streaming SIMD parsing is proven: simdjson streaming, JSONSki bit-parallel fast-forward
("processes a word-size of characters at once, no char-by-char"). BUT the evidence base fences it:

- **§0 law: fold-over-lazy-cursor is DEAD** (2.3–4.9× behind at HEAD). A lazy/incremental scanner
  must not resurrect the per-element cursor fold; windows must stay 64-byte SIMD blocks.
- **The `PaddedView` invariant** (`parse_that/src/state.rs:57`, `INPUT_PAD_BYTES=64`, trailing 64
  NUL): every stripe load skips a tail guard because the buffer is over-allocated. A streaming/
  window design that drops this invariant re-introduces per-block bounds branches and loses the
  branchless tail — the pad invariant is load-bearing and any 9D window plane must preserve it.

So 9D is legitimate but constrained: block-granular lazy windows over a padded ring, kernel
selection at combinator-compile time (a cost-model decision — LOCK-2 territory), never a byte
cursor.

---

## 6. Constraints our evidence already imposes (the binding set for every LOCK-9 route)

1. **§0/09 REFUTED — no shim/port perf claim.** Stable NEON = measured no-regression floor only;
   paired interleaving mandatory; the value of de-nightly is STABILITY (PT-2 charter).
2. **vshrn narrow, never double-`addv`** (§0/03 CONFIRMED; the 2271-vs-509 counterexample is law).
   Consumers iterate at nibble stride; a re-narrow-to-16-bit tail is a latent regression.
3. **§0/02 NO_GO — no digit-scan SIMD** (Mula/Lemire CPI inversion 0.13→0.30). `number_simd.rs`
   ports its NEON path byte-for-byte and purges x86; it does NOT pursue in-vector digit accum.
4. **§0/04 — no block-scheduling.** The lever is movemask-narrow + vectorized compaction, not
   multi-block software pipelining (M-class OoO already pipelines one block).
5. **x86 DEAD (4-layer abrogation, both repos); M-series is NEON-only** (no SVE/SVE2/SME for byte
   work). The `pclmulqdq`/SSE4.2 arms in `quote_parity.rs`/`number_simd.rs` are purged, NEON kept.
6. **Warm benches are disinformation; cold per-parse, PMU cyc/B, paired, N≥200, multi-window.**
7. **Parity oracle mandatory** (simdjson #309 half-mask precedent + dav1d checkasm): every NEON
   kernel cross-checked against a scalar reference that IS the spec — already the bbnf-simd law.

---

## 7. Named traps in comparable systems (avoid list)

- **Half-mask loss on narrow** (simdjson #309): silent correctness bug when stitching narrowed
  masks — oracle catches it, guessing does not.
- **Two-arm window asymmetry** (PROVE-09 VERIFY): the classic self-deception; "obvious" NEON wins
  measured neutral once paired. Never bank a speedup from unpaired windows.
- **Double-`vaddv` movemask** (PROVE-09 REPORT): the OLD reduction-tree family emits ~4× the
  reduction work; looks like a faithful port, costs cycles.
- **DFA-ifying the quote-parity kernel** (§4): a general regex DFA cannot cheaply express the
  bit-parallel prefix-XOR in-string toggle; naive 9C regresses JSON.
- **Dropping the 64-byte pad invariant** (§5): a streaming window that re-adds tail bounds checks.
- **Kernel that duplicates instead of shares, or shares by coupling two repos' build** (9A): the
  csc411 vendored-sync-gate template exists precisely to avoid a live cross-repo build edge under
  the placement law.

---

## Sources

- Kutenin / ARM, "Porting x86 vector bitmask optimizations to Arm NEON" — the `vshrn_n_u16::<4>` idiom, 10–15% strlen. https://developer.arm.com/community/arm-community-blogs/b/servers-and-cloud-computing-blog/posts/porting-x86-vector-bitmask-optimizations-to-arm-neon
- Langdale, "Fitting My Head Through The ARM Holes" (missing PMOVMSKB, two sequences). https://branchfree.org/2019/04/01/fitting-my-head-through-the-arm-holes-or-two-sequences-to-substitute-for-the-missing-pmovmskb-instruction-on-arm-neon/
- simdjson #309 (neon_movemask_bulk losing half the mask). https://github.com/simdjson/simdjson/issues/309
- simdjson json_structural_indexer.h (stage-1 shufti classification). https://github.com/simdjson/simdjson/blob/master/src/generic/stage1/json_structural_indexer.h
- Langdale/Lemire, "Parsing Gigabytes of JSON per Second". https://branchfree.org/2019/02/25/paper-parsing-gigabytes-of-json-per-second/
- Validark, "Eine Kleine Vectorized Classification". https://validark.dev/posts/eine-kleine-vectorized-classification/
- vgatherps, "Parsing numbers with SIMD 2 — NEON". https://vgatherps.github.io/2022-12-04-dec-2/
- Lemire, "Checking whether an ARM NEON register is zero". https://lemire.me/blog/2025/01/20/checking-whether-an-arm-neon-register-is-zero/
- sse2neon #712 (SSE idiom normalization, vceqq+vshrn peephole). https://github.com/DLTcollab/sse2neon/issues/712
- aho-corasick Teddy README + DESIGN.md (prefilter over automaton). https://github.com/BurntSushi/aho-corasick/blob/master/src/packed/teddy/README.md , https://github.com/BurntSushi/aho-corasick/blob/master/DESIGN.md
- JSONSki (bit-parallel fast-forward streaming). https://dl.acm.org/doi/pdf/10.1145/3503222.3507719
- simdjson #1361 (total streaming). https://github.com/simdjson/simdjson/issues/1361
- simde (portable SIMD emulation shims). https://github.com/simd-everywhere/simde
- FFmpeg/dav1d checkasm (scalar-reference parity model), HN. https://news.ycombinator.com/item?id=43140614
- In-repo: SPEC.md §0 (01/02/03/04/09/10 + refutation laws); PROVE-09 REPORT+VERIFY; PT-2.md; R3-parse-that-rust.md; bbnf-simd/src/{lib.rs,aarch64/movemask.rs,aarch64/structural_classify_64.rs}; csp-solver csc411 vendored-sync-gate git log.
