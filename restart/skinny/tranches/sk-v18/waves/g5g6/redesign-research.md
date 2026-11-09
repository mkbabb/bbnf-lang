# SK-V18 G6 — REDESIGN RESEARCH: dispatch-targeting NEON classification kernel

Wave: G5/G6 (redesign branch; AMENDMENT.md ratified 2026-06-02). Host: aarch64 / Apple
M-series. Discipline: **dav1d** — scalar-reference FIRST, checkasm differential over REAL
corpora BEFORE wiring, aarch64 NEON/dotprod ONLY (no x86 — P1 deleted it; no SVE).

> **HEADLINE (read first).** The ratified redesign retargets the NEON from the *inert-run
> skip* (which the measurement proved ineffective: inert runs median 6–9 B ≪ 64 B block) to
> the *per-significant-byte DISPATCH* that the S-P1 sample attributes the 79.5% to
> (`generated.rs:663` load, `:664` `delimiters.contains(&byte)` membership test, `:667`
> `match byte` dispatch — now living in `runtime_scan.rs:62–70` / `dispatch_structural`). The
> redesign is a simdjson-stage-1 **structural classification**: ONE vectorized pass classifies
> 64 bytes into (a) a SIGNIFICANT bitmask + (b) a KIND-per-byte vector; the scalar shell then
> CTZ-iterates the bitmask, reading each structural position's KIND from the vector with NO
> re-load and NO re-match, and makes the structural decision (recurse/open/close/string/
> comment/error). **This DOES address where the 79.5% goes** — but the honest ceiling is set by
> Amdahl: the classification amortizes only the *recognize+dispatch-classify* fraction of the
> per-byte work; the *structural decisions* (recurse/string-skip/error) remain irreducibly
> scalar and per-structural-byte. The full feasibility verdict + the honest payoff bound is §6.
>
> **CRITICAL DESIGN FINDING (drives the kernel choice).** The 11-byte significant set
> `' " / ( ) [ ] { } ; :` has **lo-nibble collisions** (`; [ {` all = lo-nibble 0xB; `] }` =
> 0xD) AND a **lo-6-bit collision** (`;`=0x3B and `{`=0x3B). So neither a single
> `vqtbl1q_u8` low-nibble LUT NOR the existing `vqtbl4q_u8` lo6-table (`classify_tbl4.rs`) can
> assign a *distinct kind* per byte by itself — exactly the "lo6-collision" that made §8 REJECT
> the table-classifier-unify (Candidate C). The kernel design must therefore use either the
> simdjson **two-LUT (lo-nibble × hi-nibble) AND-fold** or a small **`vceqq` fan**. §2 picks the
> fan-for-bitmask + a 2-LUT-or-fan for kind, with the lo6 collision as the load-bearing reason.

---

## 1. THE SCAN MAP — what the per-byte dispatch (the 79.5%) actually spends time on

### 1.1 The live scan (post-G2, post-P3): `runtime_scan.rs` + the singular generated call

The generated CSS provider has exactly ONE scan call site (`css_l4_declaration_values/
generated.rs:517`): `css_balanced_component_scan(self.bytes, pos, stop, &SKIP_ALPHABET)`. Four
grammar-derived STOP sets reach it (`generated.rs:29–35`): `AT_RULE_STOP=b";{}"`,
`QUALIFIED_STOP=b"{};"`, `DECLARATION_STOP=b";}"`, `COLON_BEFORE_STOP=b":{};"`. The structural
SKIP family is fixed-9: `SKIP_ALPHABET = ' " / ( ) [ ] { }` (`generated.rs:27`).

The scalar hot loop is now `css_balanced_component_scan` (`runtime_scan.rs:52–73`) + its inner
recursive twin `css_consume_balanced` (`:80–103`) + `dispatch_structural` (`:109–128`). This is
the same machine the S-P1 sample saw as `find_component_delim` (79.5%) +
`consume_balanced_at` (14.6%) — G2 lifted it out of `generated.rs` into the named primitive but
the per-byte algorithm is byte-identical.

### 1.2 The per-byte dispatch — EXACTLY what the 79.5% is

The outer loop body (`runtime_scan.rs:59–71`), per iteration, after the inner skip returns the
next significant `pos`:

```
let byte = bytes[pos];                       // (A) the byte LOAD       — S-P1 generated.rs:663
if delimiters.contains(&byte) {              // (B) MEMBERSHIP TEST     — S-P1 generated.rs:664
    return Ok(Some((byte, pos)));            //     (≤4-elem linear .contains)
}
pos = dispatch_structural(bytes, pos, byte, skip)?;  // (C) MATCH-ON-KIND — S-P1 generated.rs:667
```

and `dispatch_structural` (`:116–126`) is the **match-on-kind**:

```
match byte {
    b'\'' | b'"'              => consume_string(...)        // string-skip (scalar, cold)
    b'/' if next == Some(b'*')=> consume_comment(...)       // comment-skip (scalar, cold)
    b'(' => css_consume_balanced(..., b')', ...)            // RECURSE  (open paren)
    b'[' => css_consume_balanced(..., b']', ...)            // RECURSE  (open bracket)
    b'{' => css_consume_balanced(..., b'}', ...)            // RECURSE  (open brace)
    b')' | b']' | b'}' => Err("unexpected closing delimiter")  // ERROR (top-level close)
    _ => pos + 1,                                            // ADVANCE-ONE (a `/` not before `*`)
}
```

**Per significant byte the scalar shell pays, in order:**

1. **(A) LOAD** `bytes[pos]` — one byte load (`:66`).
2. **(B) MEMBERSHIP TEST** `delimiters.contains(&byte)` — a `≤4`-element linear scan +
   compare-branch (`:67`). This is the STOP test (`; { } :`). On the hot inner skip it returns
   *false* for every structural byte that is NOT a stop, so it is paid on EVERY significant
   byte, not just the stops.
3. **(C) MATCH-ON-KIND** `match byte { ... }` — a jump-table / compare-chain over 9–11 arms
   (`:116`), re-classifying the SAME byte the membership test just examined, into one of:
   recurse-open / string / comment / unexpected-close / advance-one.
4. **THE STRUCTURAL DECISION** — the *body* of the matched arm: recurse (`css_consume_balanced`,
   the 14.6% inner half), skip a string/comment (cold), error, or `pos + 1`.

The 79.5% self-time concentrates in **(A)+(B)+(C)** — the LOAD + MEMBERSHIP + MATCH — per S-P1
`a2-css-hot-leaves.md:64–66` ("`find_component_delim` self-time concentrates in `:664`
membership, `:667` match, `:663` load"). The inner-half `consume_balanced_at` concentrates in
`:700` (its own inner `match`) + `:696`/`:709` (load + loop). **Steps (A)+(B)+(C) are RE-WORK:
they re-examine each significant byte two or three times (the membership linear-scan, then the
match jump-table) to recover information the scan already knows — "this byte is significant" —
and to recover its CLASS.** That re-examination, paid per significant byte, IS the dispatch
overhead the redesign targets.

### 1.3 What the existing inert-run skip (`find_css_significant`) does — and why it barely helps

`find_css_significant` (`runtime_simd.rs:169`) is the inner skip: a two-fan eq-set OR-reduce
(`set_a = fixed[..8]`, `set_b = fixed[8] ++ delimiters`, `:184–192`) over
`byte_class_from_eq_set_64`, 64 bytes at a time (`:195–204`), returning the FIRST significant
byte via `mask.trailing_zeros()`. It vectorizes the *gaps between* significant bytes. But (per
research.md §6 + AMENDMENT.md) the gaps are median 6–9 B, so the 64-B fast path productively
fires on ~0% (animate) / 2.5% (bootstrap) / 24.6% (material) of bytes — and on every short run
it returns inside block 0, doing MORE setup than a 6-byte scalar walk. The skip is correct +
neutral but addresses the wrong term: it shrinks the (already short) inert gaps, not the
(frequent) per-significant-byte dispatch (A)+(B)+(C). **The redesign keeps the
significant-bitmask production but adds the KIND vector, and moves the win from "skip the gap"
to "iterate the structurals without re-loading + re-matching each one."**

### 1.4 Significant-set census + the lo-nibble / lo6 collision (drives §2)

The union significant set is **11 distinct bytes**: `' " / ( ) [ ] { } ; :` (SKIP_ALPHABET's 9
∪ the stop bytes' net-new `; :`). Per call it is ≤11 (the ≤4 `delimiters` overlap SKIP on
`{ }`, so the net-new is at most `; :`). Byte/nibble structure:

| byte | hex  | hi-nib | lo-nib | lo6 |  | byte | hex  | hi-nib | lo-nib | lo6 |
|------|------|--------|--------|-----|--|------|------|--------|--------|-----|
| `"`  | 0x22 | 2      | 2      | 22  |  | `;`  | 0x3B | 3      | **B**  |**3B**|
| `'`  | 0x27 | 2      | 7      | 27  |  | `[`  | 0x5B | 5      | **B**  | 1B  |
| `(`  | 0x28 | 2      | 8      | 28  |  | `]`  | 0x5D | 5      | **D**  | 1D  |
| `)`  | 0x29 | 2      | 9      | 29  |  | `{`  | 0x7B | 7      | **B**  |**3B**|
| `/`  | 0x2F | 2      | F      | 2F  |  | `}`  | 0x7D | 7      | **D**  | 3D  |
| `:`  | 0x3A | 3      | A      | 3A  |  |      |      |        |        |     |

- **lo-nibble collisions:** `; [ {` ≡ lo-nib 0xB; `] }` ≡ lo-nib 0xD → a single `vqtbl1q_u8`
  16-entry low-nibble LUT CANNOT separate `;`/`[`/`{` (three different KINDS: stop vs
  open-bracket vs open-brace) nor `]`/`}` (close-bracket vs close-brace/stop).
- **lo6 collision:** `;`(0x3B) ≡ `{`(0x3B) under `& 0x3F` → the existing `vqtbl4q_u8` lo6-table
  (`classify_tbl4.rs:30`, `match_tiny_plain_string.rs`) ALSO cannot separate `;` from `{`. This
  is precisely the "lo6-collision" §8 cites to REJECT Candidate C (table-classifier unify). It
  recurs here and **forbids the cheap single-LUT kind assignment** — the kernel needs either a
  hi-nibble disambiguation (the simdjson 2-LUT) or a compare fan.
- **hi-nibble values:** only {2, 3, 5, 7} — sparse, so a hi-nibble LUT is viable as the second
  factor of a 2-LUT AND-fold (the simdjson lo×hi scheme).

---

## 2. THE NEON CLASSIFICATION KERNEL (grammar-neutral, in bbnf-simd)

### 2.1 Output contract (what the kernel produces for the shell)

For a 64-byte block the kernel returns **two products**:

```rust
pub struct StructuralClassify64 {
    pub significant: u64,       // bit i set iff src[i] ∈ significant-set (caller data)
    pub kind: [u8; 64],         // kind[i] = structural class of src[i] (only meaningful where significant)
}
```

- `significant` — the bitmask the shell CTZ-iterates (replaces `find_css_significant`'s
  return-first-bit; now we return the WHOLE mask so the shell visits every structural in the
  block without re-scanning).
- `kind` — one byte per position, an enum-coded class. The shell reads `kind[ctz]` directly
  (no re-load of `src`, no re-match). The kind alphabet (caller data, ≤ a handful):
  `STOP`(membership-hit), `OPEN_PAREN`, `OPEN_BRACK`, `OPEN_BRACE`, `CLOSE`(any of `) ] }`),
  `QUOTE`(`' "`), `SLASH`(`/`). The shell still re-checks `/`+`*` and quote identity scalar-side
  (cheap, only on the rare quote/slash), so the kind table only needs the coarse class.

The `kind` is delivered as a `[u8;64]` (a 64-byte SIMD store of the per-lane class vector) — NOT
a packed bitmask, because the shell needs the FULL class per position, and a `vst1q_u8` ×4 of
the class vector is one cheap store per stripe. (Alternative: deliver kind as several parallel
bitmasks — `open_mask`, `close_mask`, `quote_mask`, `slash_mask`, `stop_mask` — and the shell
tests `(1<<ctz) & open_mask` etc. This is the simdjson-native form, avoids the `[u8;64]` store,
and is the RECOMMENDED variant; see §2.4. Either is grammar-neutral.)

### 2.2 The SIGNIFICANT bitmask — `vceqq` fan, reusing the proven `byte_class_from_eq_set_64`

The significant bitmask is exactly today's two-fan OR-reduce (`set_a`/`set_b`, both ≤8), already
checkasm-anchored as `byte_class_from_eq_set_64` (`bbnf-simd/src/aarch64/
byte_class_from_eq_set_64.rs`): four `uint8x16` stripes, each `vceqq_u8`-fanned against each
broadcast set member, `vorrq_u8`-reduced, packed to u64 via `movemask_u8x16`
(shrn/narrow trick, `movemask.rs:4` — the project's standard NEON→u64 movemask). **No new
significant-mask code is needed — the redesign REUSES this primitive verbatim**, only it now
returns the full mask (the kernel keeps it; the shell iterates) rather than the first bit. This
preserves the dav1d-anchored, byte-exact mask the existing parity test already covers.

### 2.3 The KIND assignment — `vceqq` fan (RECOMMENDED) vs `vqtbl1q` 2-LUT

Two candidate aarch64 schemes for the per-byte KIND, with the lo6/lo-nibble collision as the
discriminator:

**(α) `vceqq_u8` per-class fan (RECOMMENDED).** For each kind, OR-reduce the `vceqq_u8`
comparisons of its member bytes, yielding one mask per kind:
```
open_mask  = class('(') | class('[') | class('{')   // 3 vceqq + 2 vorr per stripe
close_mask = class(')') | class(']') | class('}')
quote_mask = class('\'') | class('"')
slash_mask = class('/')
stop_mask  = OR over caller `delimiters` (≤4)        // == the membership test, vectorized
```
Total per stripe ≈ 11 `vceqq` + ~7 `vorr` (the 11 significant bytes, each compared once, fanned
into 5 kind buckets). This is the SAME instruction family as the significant fan — in fact the
significant mask is just `open|close|quote|slash|stop`, so the significant bitmask FALLS OUT of
the kind fan for free (one extra `vorrq` per stripe). **This is the cleanest: it reuses the
existing `vceqq`/`vorrq`/`movemask` toolkit, has ZERO collision hazard (equality is exact), and
needs no LUT construction.** It emits the kind as the simdjson-style **parallel bitmasks** of
§2.1's RECOMMENDED variant (one u64 per kind), which the shell tests by bit. Cost: ~11 compares
× 4 stripes = 44 `vceqq` + ~28 `vorr` + 5 `movemask` per 64-B block — but ALL branch-free,
data-parallel, and amortized over 64 bytes (vs the scalar (A)+(B)+(C) paid per significant
byte). On the dense-structural corpora this is the win (see §6).

**(β) `vqtbl1q_u8` two-LUT (lo-nibble × hi-nibble) AND-fold (simdjson-native; viable but heavier
to get right).** Build two 16-entry LUTs: `lo_lut[lo_nibble] = bitset of kinds whose members
have that lo-nibble`, `hi_lut[hi_nibble] = bitset of kinds whose members have that hi-nibble`;
then `kind_bits = vqtbl1q(lo_lut, src & 0xF) & vqtbl1q(hi_lut, src >> 4)`. The AND resolves the
lo-nibble collisions (`; [ {` share lo-nib 0xB but have hi-nibs 3/5/7 → the hi-LUT separates
them). This is the canonical simdjson stage-1 classifier. Cost per stripe: 2 `vqtbl1q` + 1
`vand` + a `vshr` for the hi-nibble = ~4 ops, INDEPENDENT of set size — cheaper than the
11-compare fan IF the set were large. **But:** (i) the kinds must be encoded as a *bitset per
lane* (so a lane can carry a class-membership byte), requiring a careful kind→bit assignment that
stays collision-free under the lo×hi factoring; (ii) it is NOT grammar-neutral for free — the two
LUTs are FUNCTIONS of the caller's significant-set + kind-table, so they must be BUILT from
caller data at call setup (like `build_lo6_table`, `classify_tbl4.rs:8`), which is fine but adds
a per-call LUT-build (amortized over the whole corpus scan, negligible). (iii) The lo6 collision
that killed the `vqtbl4q` lo6-table does NOT bite the lo×hi 2-LUT (the hi factor is the
discriminator) — so (β) is *correct*, unlike the rejected Candidate C; the rejection was
specifically of the SINGLE lo6 table, not the lo×hi pair.

**Verdict (which fits the ≤11-set + kind-table best on aarch64, NEON no-SVE):** **(α) the
`vceqq` fan for BOTH the significant mask and the kind buckets.** Reasoning: the set is small
(≤11) so the fan's compare count is modest and *the significant mask comes free as the OR of the
kind buckets* — one kernel, one toolkit (`vceqq`/`vorrq`/`movemask`), one scalar reference, one
checkasm anchor (extend `byte_class_from_eq_set_64`'s). The 2-LUT (β) wins only at larger set
sizes and costs a per-call LUT-build + a delicate collision-free kind-bit encoding; its only
genuine advantage (set-size-independence) is moot at ≤11. **Recommend (α); document (β) as the
upgrade path IF a future grammar widens the significant set past ~16 (where the fan's compare
count would dominate).** Both are grammar-neutral and live in `bbnf-simd`.

### 2.4 The bitmask production — standard NEON→u64 movemask, REUSED

Each kind bucket's 64-lane compare vector packs to a u64 via the project's
`movemask_u8x16` (`aarch64/movemask.rs` — `vshrn_n_u16::<4>` narrow + nibble fold, the standard
aarch64 movemask spill; also the `byte_class_from_eq_set_64.rs:79` variant). Four stripes → four
16-bit packs → shifted into a u64. **No new movemask primitive** — the kind masks ride the exact
packing the significant mask already uses, so the bitmask production is dav1d-anchored on day one.

### 2.5 Grammar-neutrality (the binding constraint)

The kernel signature is grammar-NEUTRAL: it takes the **significant set + the kind partition as
CALLER DATA**, exactly like `byte_class_from_eq_set_64` takes `set`. Concretely the caller
(`runtime_scan.rs`, the §6 primitive) passes the SKIP_ALPHABET-derived structural classes + the
grammar-derived `delimiters` as the STOP class; the kernel names no CSS byte. A `.bbnf` mutation
that changes the stop set or the bracket family changes the ARGS, not the kernel (the
arg-mutation falsifier). The kernel lives in `bbnf-simd` beside `byte_class_from_eq_set_64`
(`significant_set_is_caller_data = true`), and the generated code emits a CALL to the
`runtime_scan` primitive, never vector code per grammar (`css_scan_call_site_singular = true`,
the P3-collapsed single site `generated.rs:517`).

---

## 3. THE SCALAR-SHELL INTEGRATION — bitmask-iterate replaces per-byte dispatch

### 3.1 The new outer loop (illustrative; the shell still owns recursion/strings/errors)

```
while pos < len {
    // classify a 64-B block once: significant bitmask + per-kind masks
    let c = structural_classify_64(bytes, pos, &kind_partition);   // ONE vectorized pass
    let mut m = c.significant >> (pos & 63 within block) ...;       // (block-relative; see §3.3)
    while m != 0 {
        let i = block_base + m.trailing_zeros();                    // CTZ to next structural
        let bit = 1u64 << (i - block_base);
        if c.stop_mask & bit != 0 {                                 // KIND read — no re-load, no re-match
            return Ok(Some((bytes[i], i)));                         //   (the membership test, now a bit-AND)
        }
        // structural decision from KIND (still scalar, still shell-owned):
        if c.open_mask  & bit != 0 { pos = css_consume_balanced(bytes, i, close_of(kind), skip)?; restart-block; }
        else if c.quote_mask & bit != 0 { pos = consume_string(bytes, i)?; restart-block; }
        else if c.slash_mask & bit != 0 && bytes[i+1]==b'*' { pos = consume_comment(bytes, i)?; restart-block; }
        else if c.close_mask & bit != 0 { return Err("unexpected closing delimiter", i); }
        else { m &= m - 1; continue; }                              // lone `/` → advance, next bit
    }
    pos = block_base + 64;                                          // whole block inert/consumed
}
```

### 3.2 What this eliminates — the (A)+(B)+(C) re-work

- **(A) the LOAD** — gone for the membership/match decision: the KIND is read from `c.*_mask &
  bit`, a register bit-AND, not a `bytes[pos]` re-load. (`bytes[i]` is loaded only when a STOP is
  RETURNED — once per returned delimiter, not per inert structural.)
- **(B) the MEMBERSHIP TEST** `delimiters.contains(&byte)` — replaced by `c.stop_mask & bit`, a
  single bit-AND of a precomputed mask. The ≤4-element linear `.contains` per significant byte
  becomes ONE vectorized `vceqq`-fan per 64 bytes + a bit test per structural.
- **(C) the MATCH-ON-KIND** `match byte { ... }` — replaced by the bit tests against the kind
  masks. The 9–11-arm jump-table per byte becomes a few register bit-ANDs, and the actual
  classification (which byte is which kind) is done once, vectorized, in the kernel.

**The shell still owns** recursion (`css_consume_balanced` on open — the 14.6% inner half;
itself uses the SAME classify kernel for its inner skip), string-skip, comment-skip, and all
error positions. The redesign moves the RECOGNIZE+DISPATCH-CLASSIFY off the scalar critical path
into the kernel; it does NOT touch the structural DECISIONS (which are correctly scalar +
per-structural). That boundary is the Amdahl line of §6.

### 3.3 The recursion / block-restart wrinkle (correctness, not perf)

`css_consume_balanced` recursion and string/comment skips JUMP `pos` forward by an arbitrary
amount (past a nested block / string). So the block classification must be RE-DERIVED after each
such jump (the cached `c` is stale once `pos` leaves the block). The clean shape: the kernel
classifies the block lazily and the shell re-classifies whenever `pos` advances past the block
boundary OR a structural decision jumps it. Within a single inert-dense run (no recursion), the
shell drains the whole `significant` bitmask before re-classifying — that is the hot case the
redesign accelerates. The inner-recursive `css_consume_balanced` reuses the SAME kernel for its
own scan (one entry, `significant_set` = `{close} ∪ SKIP`), so the 14.6% inner half is
accelerated identically. This re-derivation is the honest cost the §6 estimate must charge.

---

## 4. DAV1D VERIFICATION PLAN (scalar-ref + checkasm differential, BEFORE wiring)

### 4.1 The scalar reference (the spec — already exists, promote it)

The scalar per-byte classification IS the reference. It already exists as the scalar
`dispatch_structural` + the `significant_ref` (`runtime/src/lib.rs:488`) twin. The new kernel's
reference is the obvious scalar twin:
```
fn structural_classify_64_scalar(src, significant_set, kind_partition) -> (u64 significant, per-kind u64 masks)
    for i in 0..64 { classify src[i] into its kind bucket; set the bit in significant + the kind mask }
```
byte-exact, the executable spec (mirrors `byte_class_from_eq_set_64_scalar`,
`scalar/byte_class_from_eq_set_64.rs:26`). Authored FIRST, in `bbnf-simd/src/scalar/`.

### 4.2 The checkasm DIFFERENTIAL — over the REAL 71KB–495KB corpora, in checkasm_parity.rs

The NEON `structural_classify_64` MUST match the scalar per-byte classification at EVERY block
offset over the REAL corpora — NOT a toy micro-test. Concretely, extend
`bbnf-simd/tests/checkasm_parity.rs` (the mature harness: signal-guard longjmp trampoline,
`with_stack_canary_xor_fold`, alignment sweep `0..64`, `BBNF_SIMD_INJECT_BUG` falsifier,
`BBNF_SIMD_STRICT` corpus-strict mode):

1. **Load the four CSS corpora.** The harness currently loads JSON via
   `test_fixtures::load_available_bench_fixtures` (`checkasm_parity.rs:349`). For CSS, add the
   four `css-l4-sk-v14` corpora (animate 71KB → material 495KB) via either a dev-dep on
   `bbnf-bench::css_l4_corpus` (`corpus_dir()` at `css_l4_corpus.rs:56`) or `include_bytes!` of
   the four files. (Mind the dependency direction: `bbnf-simd` is below `bbnf-bench`; prefer
   `include_bytes!` or a small `test-fixtures` CSS entry to avoid an upward dev-dep cycle.)
2. **Block-classify differential.** For each corpus, for each 64-B block (sweeping the start
   offset `0..64` for misalignment), for each of the four caller STOP partitions
   (`b";{}"`, `b"{};"`, `b";}"`, `b":{};"`): assert NEON `significant` == scalar `significant`
   AND each NEON kind mask == its scalar kind mask, bit-exact. The scalar-canary memcmp catches
   source-scribbling; the longjmp trampoline turns OOB into a failure.
3. **End-to-end shell differential** (the three retarget seams). Assert
   `css_balanced_component_scan` with the NEON kernel == the fully-scalar reference scan over
   each whole corpus, for each STOP set — covering (a) the kind-fan / significant-mask parity,
   (b) the CTZ-iterate boundary (the shell visits exactly the scalar's structural sequence),
   (c) error-position reproduction (`ScanError.offset` identical for unclosed string/comment +
   unexpected-close). This is the `neon_significant_skip_matches_scalar` upgrade: relocate it OUT
   of `runtime/src/lib.rs:544` (inline `#[cfg(test)]`, 5 toy strings — violates `no-inline-tests`
   AND the "not micro-cases" §8 clause) into the parity harness over the REAL corpora.
4. **Injection falsifier.** Mutate ONE byte of a kind bucket (e.g. drop `{` from `open_mask`) →
   the differential FAILS over the corpora; revert → PASS. This is the §8 parity falsifier,
   re-aimed at the kind classification.

aarch64 NEON/dotprod ONLY (the kernel is `#[cfg(target_arch="aarch64")]` → NEON, else the scalar
reference, mirroring `byte_class_from_eq_set_64`'s dispatch, `lib.rs:284–288`). No x86, no SVE.

---

## 5. ADMISSION — reaching the hot path live (non-test) + the census

### 5.1 The live caller chain (post-redesign)

```
generated.rs:517  css_balanced_component_scan(self.bytes, pos, stop, &SKIP_ALPHABET)   [non-test]
   → runtime_scan.rs  css_balanced_component_scan / css_consume_balanced               [non-test]
      → runtime_simd  structural_classify_64(...)   (the new kernel entry, replaces find_css_significant)
         → bbnf-simd  structural_classify_64_neon (aarch64) / _scalar (else)
```

### 5.2 The census predicate (carry the research.md AMENDMENT)

Per research.md §3, the §8 literal predicate `rg runtime_simd::find_… grammars/*/generated.rs`
is mis-aimed: the generated code names the §6 PRIMITIVE (`css_balanced_component_scan`), not the
kernel — the kernel call is one layer down in `runtime_scan.rs` (a GOOD neutrality move). The
admission census must target the live CHAIN:
- `css_balanced_component_scan` non-empty in `generated.rs` (non-test) — the singular P3-collapsed
  site `generated.rs:517`; AND
- `structural_classify_64` non-empty in `runtime_scan.rs` (non-test) — the new kernel call,
  replacing the two `find_css_significant` calls (`runtime_scan.rs:62,92`).
`simd_admission_caller` = the `runtime_scan.rs` kernel call (product path); the dead test caller
(`lib.rs:556`) is DELETED with the inline test (§4.3).

### 5.3 The runtime-reachability profile (the binding second conjunct)

`simd_admission_profile_sampled = true`: capture a samply sample of `css_canon_bench`
`CSS_CANON_PROFILE=track1_rich` (the harness exists, `css_canon_bench.rs:196–222`, tight-loops
the real corpora) and assert the `structural_classify_64` / its `_neon` body entry carries
NON-ZERO self-time. RED predicate: `self_time_samples == 0` → `acceleration_at_admission = dead`.
**Realism note (better than the skip):** unlike `find_css_significant` (whose 64-B fast path was
profile-COLD because runs < 64 B), the classify kernel runs on EVERY block regardless of run
length (it classifies all 64 bytes, dense or sparse) — so it is reachable + sampled on ALL four
corpora, not just material. The admission profile should therefore GREEN robustly (the kernel is
on the critical path for every block), where the skip's was corpus-cold. This is the first
concrete sign the redesign is better-aimed than the as-specified skip.

---

## 6. FEASIBILITY + EXPECTED PAYOFF (the honest Amdahl bound)

### 6.1 Does the dispatch-classification ACTUALLY reduce the 79.5%? Partially — bounded by Amdahl.

Decompose the 79.5% per-significant-byte work into VECTORIZABLE vs IRREDUCIBLE:

| term | what | per | redesign disposition |
|------|------|-----|----------------------|
| (A) LOAD `bytes[pos]` | byte load for membership+match | per significant byte | **vectorized** — folded into the block load; read from kind mask, not re-loaded |
| (B) MEMBERSHIP `delimiters.contains` | ≤4-elem linear stop test | per significant byte | **vectorized** — `stop_mask & bit`, one fan per 64 B |
| (C) MATCH-ON-KIND `match byte` | 9–11-arm jump-table | per significant byte | **vectorized** — kind classification done once per block; shell does bit tests |
| (D) STRUCTURAL DECISION | recurse / string-skip / comment-skip / error / advance | per STRUCTURAL byte | **IRREDUCIBLE** — stays scalar, shell-owned |
| (E) CTZ-iterate + block re-derive | next-bit + re-classify after jumps | per structural + per recursion | **new scalar cost** the redesign adds |

The redesign collapses (A)+(B)+(C) — the RE-WORK that re-examines each significant byte to
recover "significant?" + "which kind?" — from per-byte scalar into one vectorized block pass +
cheap per-structural bit tests. **(A)+(B)+(C) is the bulk of the 79.5%** (S-P1 attributes the
self-time precisely to `:663` load + `:664` membership + `:667` match — i.e. (A)+(B)+(C), NOT to
the recurse/skip bodies, which roll up into `consume_balanced_at`'s separate 14.6% leaf and the
cold string/comment paths). So the redesign targets the RIGHT term, unlike the skip.

**But the honest ceiling:** the speedup is `1 / (s + p/N_eff)` where `s` = the irreducible
fraction (D)+(E) and `p` = the vectorizable (A)+(B)+(C). Two effects bound `N_eff` below the
naive 64×:
1. **Dense structurals shrink the amortization.** With median runs of 6–9 B, a 64-B block holds
   ~7–10 significant bytes. The classify pass is paid ONCE per 64 B but the shell still does
   per-structural bit-tests + decisions (D) for each — so the amortization of (A)+(B)+(C) is over
   ~64 bytes (good) but the residual per-structural scalar work (D)+(E) is paid ~7–10× per block
   (unchanged). The classify-then-iterate STILL wins over per-byte-dispatch because the iterate's
   per-structural cost (a CTZ + a few bit-ANDs) is strictly CHEAPER than the per-byte-dispatch it
   replaces (a load + a ≤4 linear membership + an 11-arm match) — but the win is the DELTA between
   "cheap bit-test per structural" and "expensive load+membership+match per structural", PLUS the
   one-time vectorized classify, NOT a 64× collapse.
2. **The block re-derive (E) on every recursion / string / comment jump.** Each open-bracket
   recurse, each quote, each comment invalidates the cached block classification. On
   bracket-dense CSS (selectors `:not(...)`, `calc(...)`, `url(...)`) the re-derive is frequent,
   eroding the amortization. The honest model: the classify pays off WITHIN inert-dense
   declaration-value runs (the `a:b; c:d; e:f` spine — long stretches of `:` `;` with few
   brackets) and pays LESS in bracket-dense selector/function regions.

### 6.2 Honest estimate

- **The vectorizable fraction (A)+(B)+(C) is the majority of the 79.5%** (load + membership +
  match are the attributed lines; the decisions are the separate 14.6% + cold leaves). Call it
  ~60–75% of the 79.5%, i.e. ~45–60% of parser self-time is the per-byte recognize+dispatch
  re-work the kernel addresses.
- **The realized speedup is the per-structural DELTA × the structural density, plus the
  one-time classify**, MINUS the re-derive (E). On the declaration-value spine (stop-dense, low
  bracket) the delta is large and re-derive is rare → real win. On selector/function regions
  (bracket-dense) the re-derive erodes it. So the corpus profile should INVERT the skip's:
  bracket/selector-heavy corpora gain LESS, declaration-value-heavy corpora gain MORE — the
  opposite skew from the inert-skip (which only paid on material's long data-URI runs).
- **Net honest verdict: the payoff is REAL and better-aimed than the skip** (it touches the
  attributed term, runs on every block, samples warm on all corpora), but it is **NOT a clean
  79.5%→small collapse** — Amdahl caps it at `1/(s + p/N_eff)` with `s` = the irreducible
  per-structural decisions (D) + the re-derive (E), which on dense CSS is a non-trivial floor.
  A defensible expectation: a meaningful but sub-2× scan speedup on the recognizer, larger on
  declaration-dense corpora, smaller on selector/bracket-dense ones — to be MEASURED at H1
  (corpus-in-timer), not asserted here.

### 6.3 The residual surfaced honestly

- **If the structural decisions (D) dominate** (e.g. a corpus that is mostly nested
  brackets/functions, where every significant byte triggers a recurse) — then `s`→1 and the
  classify wins little, because the per-structural scalar decision is the cost, not the
  recognize. The redesign cannot vectorize a recursion. THIS IS THE RESIDUAL: on
  recursion-saturated input the dispatch-classification helps as little as the skip did on
  short-run input. The four CSS corpora are NOT recursion-saturated (declaration values dominate
  bytes), so the redesign should net-win, but the honest framing for H1 is "scan-recognize
  accelerated; structural-decision floor remains; magnitude corpus-dependent."
- **The re-derive (E) is a genuine NEW cost the skip did not have.** It must be measured, not
  assumed negligible. If H1 shows the re-derive eats the classify win on bracket-dense corpora,
  that is the honest close (an `S`/amendment), NOT a hidden regression.

### 6.4 Feasibility verdict

**FEASIBLE and better-aimed than the as-specified skip.** The kernel is a strict extension of
the proven `byte_class_from_eq_set_64` (same `vceqq`/`vorrq`/`movemask` toolkit, same dav1d
anchor), grammar-neutral by the same caller-data contract, and reaches the hot path on EVERY
block (warm admission on all corpora, unlike the skip). It targets the ATTRIBUTED term
(A)+(B)+(C), not the mis-aimed inert gap. The honest ceiling is the Amdahl floor of the
irreducible structural decisions (D) + the block re-derive (E); the magnitude is corpus-skewed
(declaration-dense > selector-dense, inverting the skip's skew) and MUST be measured at H1, not
promised. **This is a payoff the skip could not deliver — but it is bounded, not unlimited.**

---

## 7. THE >SOTA INVARIANT (no regression)

- The classification is a **recognizer optimization, not a semantic change**: the shell makes the
  IDENTICAL structural decisions in the IDENTICAL order (the checkasm end-to-end differential of
  §4.3 PROVES the NEON scan == the scalar scan, including error positions). The 9-field cssparser
  parity (rules, at_rules, qualified_rules, declarations, selectors, dimensions, numbers, colors,
  functions) is computed by the SAME tape walk on the SAME offsets — the scan returns the SAME
  `(byte, pos)` STOP hits, so the tape is bit-identical and the 9 fields are EXACT (G2-LEDGER
  `g2_cssparser_9field_parity = EXACT` holds by construction; the redesign changes HOW the scan
  finds the next stop, never WHICH stop or WHERE).
- **>SOTA must IMPROVE or HOLD**, measured at H1 cold corpus-in-timer (`css_canon_bench`,
  corpus-in-timer symmetric, no warm micro-fixtures). The current bar is 1.66×–3.38× vs
  lightningcss (a2-css-hot-leaves.md:29–34). The redesign cannot REDUCE the ratio without a
  correctness regression (excluded by §4) — worst case it is within noise on bracket-dense
  corpora and improves on declaration-dense ones. NO regression. `g6_speedup_median_mbps` stays
  null pre-H1 (correctness+admission plane only); the magnitude is the H1 figure, framed per §6.

---

## 8. IMPLEMENTATION STEP PLAN (dav1d order)

1. **Scalar-reference confirm (dav1d step 1).** Author `structural_classify_64_scalar` in
   `bbnf-simd/src/scalar/` — the byte-exact per-byte classification into `(significant: u64,
   per-kind masks)`. Confirm it reproduces the existing `significant_ref` (`lib.rs:488`) for the
   significant bit, and that the kind partition matches `dispatch_structural`'s arms
   (`runtime_scan.rs:116`). This is the spec.

2. **NEON classify kernel (bbnf-simd).** Author `structural_classify_64_neon` in
   `bbnf-simd/src/aarch64/` (the `vceqq` fan, scheme α §2.3): per-stripe kind-bucket OR-reduces
   (`open/close/quote/slash/stop`), significant = OR of buckets, pack each to u64 via
   `movemask_u8x16`. Dispatch `#[cfg(target_arch="aarch64")]` → NEON, else scalar (mirror
   `byte_class_from_eq_set_64`, `lib.rs:284`). Reuse the significant fan from
   `byte_class_from_eq_set_64` verbatim. NO new movemask. (Document scheme β 2-LUT as the
   >16-set upgrade path; do NOT build it.)

3. **Checkasm differential over REAL corpora (dav1d step 2 — BEFORE wiring).** Extend
   `bbnf-simd/tests/checkasm_parity.rs`: load the four `css-l4-sk-v14` corpora (include_bytes! or
   a CSS test-fixtures entry, avoiding the upward bench dep); assert NEON block-classify == scalar
   block-classify (significant + every kind mask) at every offset over each corpus for each of the
   four STOP partitions; carry the injection falsifier (drop one kind member → FAIL); add the
   end-to-end shell differential (`css_balanced_component_scan` NEON == fully-scalar reference,
   covering the three seams + error positions). `checkasm_differential = PASS`,
   `neon_significant_skip_matches_scalar = PASS` (real corpora). DELETE the inline
   `lib.rs:544`/`:567` micro-tests (subsumed; `no-inline-tests`).

4. **Scalar-shell bitmask-iterate integration.** Rewrite `css_balanced_component_scan` /
   `css_consume_balanced` (`runtime_scan.rs`) to call `structural_classify_64` once per block and
   CTZ-iterate the significant bitmask, reading each structural's KIND from the masks (§3.1),
   making the structural decision scalar-side, re-deriving the block after each recursion/skip
   jump (§3.3). `dispatch_structural` collapses into the iterate's bit-test arms. Land kernel +
   consumer in ONE commit (addendum-6 no-orphan law).

5. **Admission census + profile (dav1d step 4).** Source census (amended predicate §5.2):
   `css_balanced_component_scan` non-empty in `generated.rs` (non-test) AND
   `structural_classify_64` non-empty in `runtime_scan.rs` (non-test). Profile: samply
   `CSS_CANON_PROFILE=track1_rich css_canon_bench`; assert the kernel entry carries non-zero
   self-time (`simd_admission_profile_sampled = true`). The kernel runs on every block → robust
   GREEN (unlike the skip's corpus-cold profile).

6. **G5 — neutralize `json/scan.rs`** (folds in, LOW; unchanged from research.md §4): census-prove
   no `scan_structurals` reaches `parse_direct`; keep `structural_capacity_for`; fold the heavy
   dead entries onto the shared surface or gate behind the probe. `json_scan_rs_neutralized =
   neutralized`, `json_guard_held` (51/51 ±1.0%).

7. **Delete the dead `find_comment_close`** (`runtime_simd.rs:112`) + its test (`lib.rs:567`) —
   comments cold, scalar `consume_comment` is live. No `#[cfg(test)]`-only NEON kernel survives.
   (The old `find_css_significant` is SUBSUMED by `structural_classify_64` — its significant fan
   is reused inside the new kernel; delete the standalone entry once the iterate consumes the
   classify.)

8. **Parity + bench gate.** `checkasm_differential = PASS` ∧ `neon_significant_skip_matches_scalar
   = PASS` (real corpora) ∧ `acceleration_at_admission = admission` (both conjuncts) ∧
   `css_scan_call_site_singular = true` ∧ `significant_set_is_caller_data = true` ∧ 9-field
   cssparser parity EXACT ∧ `g6_speedup_median_mbps = null` (pre-H1) ∧ `json_scan_rs_neutralized
   = neutralized` ∧ `json_guard_held = true`. Outcome `C` (correctness+admission proven, speedup
   deferred) → `A` on the H1 corpus-in-timer figure (framed per §6: real but bounded,
   corpus-skewed; `R` only on a measured >SOTA regression, which §4+§7 exclude by construction).

**Revert slice (one unit):** the classify kernel + scalar ref + checkasm extension + the
shell iterate-integration + the `find_comment_close` deletion + the G5 neutralization; restore
`SK-V18-open` RESULTS; REDRESS names the failed seam (kind-fan parity / CTZ-iterate boundary /
error reproduction / block re-derive correctness / caller census / profile attribution). Caps:
MED-HIGH (deepest ASM of the campaign); redress 45 min (G5 LOW).

---

## CROSS-REFERENCES

- `crates/runtime/src/runtime_scan.rs:52,62,80,92,109,116` — the §6 primitive + the per-byte
  dispatch (the 79.5% machine) the redesign rewrites to bitmask-iterate.
- `crates/runtime/src/runtime_simd.rs:169` — `find_css_significant` (the inert-skip; its
  significant fan is SUBSUMED into the new kernel); `:112` `find_comment_close` (DEAD; delete).
- `crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs` — the `vceqq`/`vorrq`/`movemask`
  fan the significant mask + kind buckets REUSE; `src/scalar/byte_class_from_eq_set_64.rs:26` —
  the scalar-ref pattern the new scalar twin mirrors.
- `crates/bbnf-simd/src/aarch64/movemask.rs:4` — the standard NEON→u64 movemask (shrn/narrow).
- `crates/bbnf-simd/src/aarch64/classify_tbl4.rs:8,30` / `match_tiny_plain_string.rs` — the
  `vqtbl4q` lo6-table (scheme β prior art; the lo6 collision `;`≡`{` is why it can't be the kind
  table alone); `aarch64/unescape_uxxxx.rs:81–83` — the `vqtbl1q` nibble-LUT prior art (scheme β).
- `crates/runtime/src/lib.rs:488,544,567` — `significant_ref` (promote) + the inline micro-tests
  (DELETE, relocate to checkasm).
- `crates/bbnf-simd/tests/checkasm_parity.rs:345` — the corpus-parity harness to EXTEND for CSS.
- `crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:27,29-35,517` —
  SKIP_ALPHABET + the 4 STOP sets + the singular P3-collapsed call site (admission census target).
- `crates/bbnf-bench/src/bin/css_canon_bench.rs:196` — `CSS_CANON_PROFILE` samply harness.
- `crates/bbnf-bench/src/css_l4_corpus.rs:56` — the 71KB–495KB corpus loader.
- `restart/.../p1/a2-css-hot-leaves.md:64-66` — the 79.5% line attribution (`:663` load / `:664`
  membership / `:667` match) — the term the redesign targets.
