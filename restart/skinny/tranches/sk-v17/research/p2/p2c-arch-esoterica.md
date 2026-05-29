# SK-V17 P2-C: Host-Architecture ASM/SIMD Esoterica

Pass: S-P2 Research. Cycle: V3.
Date: 2026-05-29.
Scope: Host-architecture instruction inventory keyed to the S-P1 CSS-tape hot leaves —
primary aarch64 (TBL/TBX, the shrn-fused movemask, UDOT/DotProd, CSSC/CLZ/RBIT, PMULL,
the wide-shift family); secondary x86 (AVX2/AVX-512/GFNI/VBMI2/VPCLMUL) noted only as
REDRESS-blocked out-of-scope. Each candidate instruction route is bound to a named S-P1
hot leaf or flagged orphan; every REDRESS-blocked instruction route is named with its
refutation.
Output: this file.
P1 hot-leaf antecedents: `CssFullParser::find_component_delim` (`css_l4_declaration_values/generated.rs:288`,
56.52–59.24% self) + `consume_balanced_at` (`generated.rs:320`, 10.31–11.05% self) =
ONE byte-class-membership scan primitive reached two ways; `emit_fact_stream`
(`generated.rs:5`, 24.59–25.01% + ~64% alloc floor) = tape target, NOT a SIMD target;
`push_ascii_lower_hex` (`generated.rs:628`, 8.98–9.11%) = FNV diagnostic, NO primitive.
No number/unicode/dispatch/tape leaf is hot on either CSS plane (P1-E §2.5).
Lock surface: Lock 14 (grammar-neutral instruction routing via `select_classifier(alphabet)`) +
Lock 16 (aarch64-only primitive manifest, scalar-oracle-first, checkasm parity); Lock 1
touched only insofar as the structural index a NEON leaf produces IS the tape (no sidecar).

## §1 — Findings (concrete; file:line on bbnf claims, citation on external claims)

### 1.1 The host and its ISA envelope (the admission boundary)

Host: Apple M5 Max, `aarch64-apple-darwin` (P1-E:11). The aarch64 feature envelope that
SK-V17 may exploit, and the exact envelope it may NOT, is fixed by the contract and the
hardware:

- **NEON (ASIMD)** — baseline on every Apple core; the only SIMD ISA on the host
  (P1-E:14 "NEON is the host SIMD ISA"). All admissible vector work is NEON.
- **DotProd (`udot`/`sdot`)** — present on Apple M-series; already used in
  `digit_mac.rs:40` via an inline `asm!("udot …")` block under
  `#[target_feature(enable = "dotprod")]` (`digit_mac.rs:25`). This is a real, present,
  compile-time-gated instruction route — but it is an **orphan** (§2-C5, §4).
- **i8mm (`usmmla`/`ummla`)** — a *feature flag* on the host but **grep-clean-absent as a
  kernel** in skinny (`is_aarch64_feature_detected!("i8mm")` returns ZERO matches,
  SYNTHESIS:63, alphaE:114). Any i8mm kernel is NET-NEW.
- **CSSC (Common Short Sequence Compression: `CTZ`, `CLZ`-family, `ABS`, `SMAX`/`UMAX`)**
  — these are scalar-integer instructions. The relevant route is the
  next-set-bit / first-match-index extract: `rbit + clz` (or the bare `trailing_zeros`
  the Rust intrinsic already lowers to `clz`-after-`rbit` or `ctz` on a CSSC core).
  **Already used implicitly** — `find_ascii_set_member64` (`lib.rs:215`) and
  `byte_class_from_eq_set_64_neon` (via `mask.trailing_zeros()`) and
  `bitmap_next_set_bit_scalar` (`scalar/bitmap_next_set_bit.rs`) all lower
  `trailing_zeros()` to the host CTZ path. The *esoteric* route — promoting CTZ into a
  dedicated bulk next-bit consumer — is **REDRESS-89 rejected** (§3).
- **PMULL / `vmull_p64`** — carryless multiply, the canonical prefix-XOR / quote-region
  fill primitive (simdjson string-region mask). `bitmap_prefix_xor_64_neon`
  (`aarch64/bitmap_prefix_xor_64.rs`) currently delegates to the **scalar 6-step
  shift-XOR cascade** (`scalar/bitmap_prefix_xor_64.rs`: `mask ^= mask<<1; <<2; <<4;
  <<8; <<16; <<32`). Promoting PMULL into the hot prefix-XOR body is **REDRESS-88
  rejected** (§3).
- **SVE / SVE2 / SME / AMX** — **absent on Apple cores** (no SVE; AMX is not a public
  NEON-equivalent ISA). SVE paths are dead code on M5 Max (alphaC §6, sk-v16-arch:265-266).
  REDRESS pre-block (§3).
- **x86 AVX2 / AVX-512 / GFNI / VBMI2 / VPCLMUL** — the `bbnf-simd/src/x86_64/` tree
  exists (`avx2/`, `avx512_{bitalg,gfni,kmask,vbmi2,vnni,vpclmul}/`, `avx_ifma/`) but is
  **out of scope for the entire SK-V17 pass** (alphaC §6, REDRESS x86 sites). Noted only
  as the secondary inventory the scope matrix asks for, all of it diagnostic-only / dead
  on the host (§3).

### 1.2 What the host ISA is ACTUALLY doing on the hot leaf today: nothing

The dominant CSS hot leaf is a **scalar byte-at-a-time loop with zero SIMD**
(`find_component_delim`, `generated.rs:288-311`):

```rust
while pos < self.bytes.len() {
    let byte = self.bytes[pos];
    if delimiters.contains(&byte) { return Ok(Some((byte, pos))); }   // :295 linear membership over &[u8] of len ≤ 4
    pos = match byte {                                                 // :298 per-byte structural dispatch
        b'\'' | b'"' => self.consume_string_at(pos)?,                 //   string skip
        b'/' if self.byte_at(pos + 1) == Some(b'*') => …,             //   comment skip
        b'(' => self.consume_balanced_at(pos, b')')?,                 //   nested balance (recurses into THE SAME inner loop)
        b'[' => …, b'{' => …,
        b')' | b']' | b'}' => return Err(…),
        _ => pos + 1,                                                 // :307 advance
    };
}
```

`consume_balanced_at` (`generated.rs:320-340`) is byte-for-byte the same `while pos<len` +
per-byte `match`, differing only in the membership test (`byte == close` vs
`delimiters.contains`). P1-E §2.3 and the V4 consolidation §3.3 establish these collapse
to **ONE NEON byte-class-scan target**, ~69% of recognition self-time. The instruction
count today is the scalar `ldrb` + `cmp`-chain per byte (the `delimiters.contains` over a
≤4-byte slice is an unrolled `cmp` ladder, NOT a vectorized membership). The whole point of
the SK-V17 NEON gate is to replace this scalar ladder with a single vector classify over a
64-byte stripe. **But** — load-bearing — the loop is **not a pure membership scan**: it
must skip strings/comments and track bracket nesting depth. That shape constrains which
host-ISA primitive is admissible (§2-C1/C2).

### 1.3 What real NEON bodies already exist (the building blocks, audited at source)

The grammar-neutral SIMD vocabulary in `bbnf-simd` is two-tier: a few genuine NEON bodies,
and several `*_neon` symbols that are **scalar-delegate stubs**. Audited this cycle:

| Symbol | file:line | Genuine NEON? | Instruction route |
|---|---|---|---|
| `classify_tbl4::classify_chunk_from_table` / `classify_structural_terminator_chunk_from_table` | `aarch64/classify_tbl4.rs:22,75` | **YES** | `vld1q_u8` + `vandq_u8`(low6) + **`vqtbl4q_u8`** (the 64-entry TBL class lookup) + `vceqq_u8`/`vcgtq_u8` + the shrn-fused movemask |
| `byte_class_from_eq_set_64_neon` | `aarch64/byte_class_from_eq_set_64.rs:33` | **YES** | four `vld1q_u8` stripes × `set.len()`-many `vceqq_u8` fanned through `vorrq_u8`, packed via `movemask_u8x16` |
| `movemask_u8x16` (the canonical one) | `aarch64/movemask.rs:4` | **YES** | **`vshrn_n_u16::<4>`** shift-narrow + `vand`/`vsri` nibble pack — the SOTA aarch64 movemask spill (NOT the slower multiply-by-2^i variant duplicated in `byte_class_from_eq_set_64.rs:79`) |
| `match_tiny_plain_string` | `aarch64/match_tiny_plain_string.rs:87` | **YES** | `vqtbl4q_u8` + shrn movemask + `rbit/clz` first-set (REDRESS-28/33 blocked as a *consumer*, §3) |
| `byte_class_from_table_64_neon` | `aarch64/byte_class_from_table_64.rs:2` | **NO — scalar stub** | delegates to `byte_class_from_table_64_scalar` |
| `bitmap_prefix_xor_64_neon` | `aarch64/bitmap_prefix_xor_64.rs:2` | **NO — scalar stub** | delegates to the 6-step shift-XOR cascade; PMULL promotion REDRESS-88 blocked |
| `bitmap_next_set_bit_neon` | `aarch64/bitmap_next_set_bit.rs:2` | **NO — scalar stub** | delegates to `trailing_zeros`; CSSC CTZ bulk-consumer promotion REDRESS-89 blocked |
| `bulk_emit_positions_64_neon` | `aarch64/bulk_emit_positions_64.rs:2` | **NO — scalar stub** | delegates to scalar bit-iterate (REDRESS-126 admitted as ASM-gen consumer on JSON, not CSS) |

The dispatch vehicle is `select_classifier(alphabet)` (`dispatch.rs:42`) → `classify_tbl4`
when `lo6_table_admissible(alphabet)` (`dispatch.rs:101`, slot computed `byte & 0x3f` at
`:106`: the alphabet's bytes are distinct under the low-6-bit mask), else scalar. The
neutrality is exact: the **alphabet is the only grammar-specific datum**; the kernel is shared
with JSON (`json/scan.rs` — the NEON `scan` fn opens at `:207`, the
`classify_structural_terminator_block_from_table` call is `:217-219`, the `escape_mask_64` /
`prefix_xor_64` string-region work is `:237-239`). The esoterica below therefore reduce to
**which host instruction the shared classify-stripe should issue for the CSS alphabet**, plus
whether any *secondary* leaf (digit, balance) admits a host instruction beyond the classify.

## §2 — Candidate primitives (each: shape + scalar-ref status + arch + P1 antecedent)

The §2 enumeration is the load-bearing artefact. Each candidate is a HOST-INSTRUCTION ROUTE,
grounded in a named S-P1 hot leaf, with the precise aarch64 instruction it issues. All are
gated behind tape activation (there is no structural index to scan into until the tape decodes
CSS — P1-E anomaly 3; SYNTHESIS §0.1 NEON gate). The lever order is **tape FIRST, then NEON
on the surviving scan** (V4 consolidation §3.4-2). The candidates here are the *NEON-on-the-
surviving-scan* designs P2-D/E/F evaluate for tape integration; P2-C owns only the instruction
selection and the REDRESS-block flagging.

### C1 — TBL classify-stripe for the CSS delimiter/balance alphabet (the primary route)

- **Shape.** Replace the `find_component_delim`/`consume_balanced_at` scalar
  `delimiters.contains` + per-byte `match` inner loop with a 64-byte-stripe classify:
  `vld1q_u8` × 4 → `vandq_u8(chunk, 0x3f)` → **`vqtbl4q_u8(lo6_table, low6)`** → `vceqq_u8`
  against the original byte → shrn-fused `movemask_u8x16` → a `u64` structural mask, then
  `mask.trailing_zeros()` (host CTZ) to find the next delimiter index. The lo6 table encodes
  the CSS structural alphabet `{; { } ( ) [ ] : , " ' /}` (the union of the three delimiter
  slices `b";{}"`, `b"{};"`, `b":{};"` plus the string/comment/bracket openers the per-byte
  `match` handles). The instruction issued per 64 bytes: ~4×(`tbl` + `eq` + `shrn` + `and`)
  vs. the scalar ~64×(`ldrb` + cmp-ladder). This is **byte-for-byte the kernel JSON already
  runs** at `json/scan.rs:217-219` — zero new SIMD source; the only new datum is the CSS alphabet
  const + `lo6_table_admissible` check.
- **Scalar-ref status: PRESENT.** `scalar::classify_chunk` (the `classify_tbl4` scalar twin,
  `dispatch.rs:21`) + `find_ascii_set_member64` scalar tail (`lib.rs:215`) +
  `scan_structurals_scalar` (the JSON structural-index scalar reference to mirror for CSS).
  No new scalar oracle needed — reuse.
- **checkasm parity: PRESENT + reused.** The existing `checkasm_byte_class_from_table_64`,
  `checkasm_structural_terminator_64`, `checkasm_ascii_set_member_find_64` gate the kernel;
  the CSS-specific gate is `corpus_parity`: NEON index == scalar index byte-exact over the
  four CSS corpora. Same-wave consumer = the C1/C2 tape build consumes the `Vec<u32>` index.
- **Arch.** aarch64 NEON: `vqtbl4q_u8` (TBL with a 4×16=64-entry table), `vceqq_u8`,
  `vandq_u8`, `vcgtq_u8`, `vshrn_n_u16::<4>` movemask, host CTZ. Admissible iff
  `lo6_table_admissible(css_alphabet)` — i.e. the CSS structural bytes are distinct under the
  **low-6-bit mask (`byte & 0x3f`)** the guard computes (`dispatch.rs:106`, NOT a true modulo).
  Verification this cycle: `;`=0x3B → `0x3B & 0x3f = 0x3B`; `{`=0x7B → `0x7B & 0x3f = 0x3B` —
  **`;` and `{` COLLIDE under the low-6-bit mask** (both map to slot 0x3B). Note this collision is
  specific to the `& 0x3f` MASK the guard actually uses: under a *true* modulo `0x7b % 0x3f = 0x3c`
  would NOT collide with `0x3b`, so the conclusion holds only because the kernel masks the low six
  bits. This is a HARD finding: the naive lo6-table route is INADMISSIBLE
  for the full CSS alphabet, so C1 falls to either the scalar path (honest, `lo6_table_admissible`
  returns false) OR C2's eq-set route (which does not use the lo6 table). This is exactly the
  `lo6_table_admissible` guard doing its job, NOT a defect — but it means **the primary CSS
  scan route is C2, not C1** (see C2).
- **P1 antecedent.** `find_component_delim` 56.52–59.24% + `consume_balanced_at` 10.31–11.05%
  (P1-E §2.3, V4 §3.3), the single ~69% byte-class-membership scan leaf.

### C2 — Equality-set fan classify-stripe (`vceqq` × `vorrq`) — the admissible CSS scan route

- **Shape.** Because the CSS structural alphabet collides under the low-6-bit mask (`byte & 0x3f`,
  C1 finding), the
  admissible NEON route for the CSS delimiter find is the **equality-set fan**, NOT the lo6
  TBL: for a delimiter set S of ≤8 bytes, fan `set.len()`-many `vceqq_u8(stripe, vdupq_n_u8(member))`
  through `vorrq_u8` into one mask-vector, then shrn-movemask to a `u64`, then host CTZ. This
  is **already a genuine NEON body**: `byte_class_from_eq_set_64_neon` (`byte_class_from_eq_set_64.rs:33`),
  consumed by `find_ascii_set_member64` (`lib.rs:209-222`). The CSS delimiter slices
  (`b";{}"`=3, `b"{};"`=3, `b":{};"`=4) are all ≤8, so the eq-set body admits directly. The
  per-64-byte instruction count: 4 stripes × (|S| `vceqq` + |S| `vorrq`) + 4 movemask — for
  |S|=4 that is ~32 vector ops per 64 bytes vs. ~256 scalar cmp + 64 ldrb. **The membership
  half** of `find_component_delim:295` maps cleanly. **But** the per-byte `match` (string skip,
  comment skip, bracket-balance recursion) does NOT — C2 finds the *next candidate delimiter
  position*, and the scalar spine must still resolve whether that position is inside a string/
  comment/nested-bracket. The honest framing (matches JSON): C2 produces a structural-candidate
  `Vec<u32>` index; the tape-consuming spine resolves context. This preserves the nested-aware
  semantics — it does not flatten them.
- **Scalar-ref status: PRESENT.** `byte_class_from_eq_set_64_scalar` (the parity anchor named
  in the kernel header) + the `find_ascii_set_member64` scalar tail (`lib.rs:222`). No new
  oracle.
- **checkasm parity: PRESENT.** `checkasm_byte_class_from_eq_set_64` (named in REDRESS Item 89
  evidence, `skinny/REDRESS.md:2555`) gates eq-set == scalar byte-exact; reused for CSS.
- **Arch.** aarch64 NEON: `vceqq_u8`, `vorrq_u8`, `vdupq_n_u8` (a SIMD lane-splat of one alphabet
  member, issued |S| times per stripe in a compile-time-fixed loop — this is an ordinary NEON
  splat instruction and is wholly unrelated to the §0.4 "broadcast" pre-block, which forbids the
  *evidence-measurement* practice of projecting one CSS timing tuple across 24 conceptual rows,
  SYNTHESIS §0.2/§0.4, p2a:371-372; that pre-block governs benchmark rows, not SIMD ops),
  `vshrn_n_u16::<4>` movemask, host CTZ. No lo6-collision constraint (does not use the TBL
  table). **This is the recommended primary CSS scan instruction route**, because it admits
  the CSS alphabet the lo6 TBL cannot.
- **P1 antecedent.** Same as C1 — the `find_component_delim`/`consume_balanced_at` ~69% leaf;
  C2 is the *admissible* instruction route for it given the C1 lo6 collision.

### C3 — shrn-fused movemask consolidation (`vshrn_n_u16`) — a sub-task, not a standalone primitive

- **Shape.** The tree carries **two** movemask implementations: the SOTA `vshrn_n_u16::<4>`
  shift-narrow nibble-pack (`aarch64/movemask.rs:4`) and a slower multiply-by-`2^(i mod 8)` +
  `vaddv` variant duplicated inside `byte_class_from_eq_set_64.rs:79`. Any CSS NEON leaf must
  route through the single `vshrn` movemask (the SOTA aarch64 mask spill: one `shrn` narrows
  the 16×u8 compare-result to an 8×u8 nibble vector, halving the lanes for free). This is the
  "fix movemask first" sub-task the architecture doc named (alphaE C2 REDRESS-pre-block:362:
  "replace the divergent impls with one cascaded `vpaddq`/`vshrn`"). It is **not a standalone
  primitive** — it is a correctness/consistency precondition on C1/C2.
- **Scalar-ref status: N/A** (movemask is a bit-packing of a vector compare; its "scalar
  reference" is the bit-set semantics already gated by the eq-set/TBL checkasm tests).
- **checkasm parity: covered transitively** by C1/C2 corpus-parity (a divergent movemask would
  flip an index bit and fail byte-exact parity).
- **Arch.** aarch64 NEON `vshrn_n_u16`, `vand_u8`, `vsri_n_u8`. SOTA per Lemire/Mula JSON
  movemask spill (cited in `byte_class_from_eq_set_64.rs:64`).
- **P1 antecedent.** Indirect — it is the mask-extract on C1/C2's `find_component_delim` leaf;
  it has no independent self-time, so it ships ONLY folded into C1/C2 (no orphan-kernel risk).

### C4 — host CTZ first-match extract (`rbit + clz` / `trailing_zeros`) — already-present, fold-only

- **Shape.** Once C1/C2 produce a `u64` structural mask, the next-delimiter index is
  `mask.trailing_zeros()` — which the host lowers to `rbit` + `clz` (or a CSSC `ctz`). This is
  **already how `find_ascii_set_member64:215` and `byte_class_from_eq_set_64.rs:71` extract the
  first match.** C4 is the host-CTZ extract folded into C1/C2; it admits as part of them.
- **Scalar-ref status: PRESENT** (`bitmap_next_set_bit_scalar`, `scalar/bitmap_next_set_bit.rs`,
  the `trailing_zeros` reference; `bitmap_next_set_bit_neon` currently delegates to it).
- **checkasm parity: PRESENT** (`checkasm_bitmap_next_set_bit`, REDRESS:2555).
- **Arch.** aarch64 `rbit`+`clz` (or CSSC `ctz`). **NB:** promoting CTZ into a *dedicated bulk
  next-bit consumer* (a standalone loop body that iterates set bits via CTZ) is the
  **REDRESS-89-rejected** route (§3). The admissible form is the single `trailing_zeros` extract
  per mask, exactly as today — fold-only, no new bulk consumer.
- **P1 antecedent.** Indirect — the index extract on the C1/C2 `find_component_delim` leaf. Ships
  only folded; no independent CSS self-time.

### C5 — UDOT/DotProd 4-digit decode (`udot`) — ORPHAN, NO benched CSS antecedent

- **Shape.** `parse_4_digits_dotprod` (`digit_mac.rs:27`) packs 4 ASCII digits into a 16-byte
  lane, `udot`s against `[100,10,1,0,…]` weights, reads lane 0, multiplies and adds the unit
  digit. A real, present, dotprod-gated kernel — but **never called in prod** (the orphan).
- **Scalar-ref status: PRESENT** (`digit_mac.rs:15-22`, the `#[cfg(not(target_feature="dotprod"))]`
  byte loop).
- **checkasm parity: REQUIRED-NEW** (a `checkasm_digit_mac` gating `udot == scalar` byte-exact;
  the kernel exists, the test is the new artefact — alphaE C4a).
- **Arch.** aarch64 DotProd `udot {acc}.4s, {digits}.16b, {weights}.16b` (`digit_mac.rs:40`).
- **P1 antecedent: NONE on either benched CSS plane.** P1-E §2.5 and V4 §3.3 are explicit: "No
  number/unicode/dispatch/tape leaf is hot" — the CSS recognition path COUNTS, it does not decode
  dimensions, so there is **zero digit-parse self-time** in any CSS profile, and no `bbnf_simd`
  frame appears in either CSS `.json.gz`. **C5 is orphan-blocked on the current planes.** Its
  ONLY re-admission condition (per the contract, alphaE C4b GATE and V4 §3.3 orphan clause):
  re-profile the *typed lazy-`ValueRef`* path AFTER W1/W2 — a plane that does not yet exist — and
  prove the digit/dimension-decode leaf is a top-N tailwind self-time leaf. S-P2 must NOT inherit
  a CSS digit-kernel hypothesis from this profile (profile-first non-negotiable, ORCHESTRATOR §8).
  Listed here ONLY to bind the host instruction (`udot`) to its re-admission gate and to mark it
  CH4-orphan, NOT to propose it.

### C6 — i8mm matrix-multiply digit/dimension decode (`usmmla`) — GATED CONTINGENCY, NOT an active candidate (twins P2-F CF-4b)

- **Shape.** A net-new i8mm kernel batching multiple short integer decodes via an 8×8 i8 matrix
  multiply (the dav1d-style i8mm "many small dot products at once" route). Hypothetically faster
  than per-quad `udot` for dense dimension lists (tailwind). This is the aarch64 twin of P2-F
  CF-4b and adopts its disposition verbatim: a **gated contingency, NOT an active candidate** —
  S-P3 must NOT shortlist it; it admits only if the CF-4b re-profile gate fires.
- **Scalar-ref status: WOULD-BE-REQUIRED, NET-NEW.** Must land with a scalar twin that does not
  yet exist. The parity oracle is **C5's `parse_4_digits` scalar twin** (`digit_mac.rs:15-22`):
  i8mm batches the same digit-run decode C5 does per-quad, so the i8mm output is checked
  byte-exact against the existing `parse_4_digits` scalar reference (no second oracle invented).
- **checkasm parity: WOULD-BE-REQUIRED-NEW** (`checkasm_i8mm_*`, i8mm == `parse_4_digits` scalar
  byte-exact).
- **Arch.** aarch64 i8mm `usmmla`/`ummla` — a *feature flag* on the host, **grep-clean-absent as
  a kernel** (SYNTHESIS:63, alphaE:114). NET-NEW source.
- **P1 antecedent: NONE** — same as C5 plus stronger: not only is the digit leaf cold, the kernel
  itself does not exist. **C6 is doubly orphan-blocked.** Re-admission gate (alphaE C4b, P2-F
  CF-4b): the post-CF-1/CF-2 re-profile on the benched typed `ValueRef` path (N≥50) must prove the
  digit/dimension leaf is top-N tailwind self-time; if unmet, C6 does NOT land (no net-new orphan
  kernel — CH4). Listed ONLY to inventory the host instruction and bind it to its gate as a gated
  contingency, never as a proposal.

### Candidate summary table

| # | Instruction route | aarch64 op | P1 antecedent (named hot leaf) | Scalar-ref | checkasm | Verdict |
|---|---|---|---|---|---|---|
| C1 | lo6 TBL classify-stripe | `vqtbl4q_u8` | `find_component_delim` 56–59% / `consume_balanced_at` 10–11% | PRESENT (reuse) | PRESENT (reuse) | **INADMISSIBLE for CSS alphabet** (`;`/`{` collide under low-6-bit `& 0x3f`); falls to C2 |
| C2 | eq-set fan classify-stripe | `vceqq_u8`×`vorrq_u8` | same ~69% scan leaf | PRESENT (reuse) | PRESENT (reuse) | **ADMIT — primary CSS scan route** |
| C3 | shrn-fused movemask | `vshrn_n_u16::<4>` | mask-extract on the scan leaf | N/A | transitive | ADMIT — sub-task of C1/C2, no orphan |
| C4 | host CTZ first-match | `rbit`+`clz`/`ctz` | index-extract on the scan leaf | PRESENT | PRESENT | ADMIT — fold-only; bulk-consumer form REDRESS-89 |
| C5 | UDOT 4-digit decode | `udot` | **NONE** (no CSS digit self-time) | PRESENT | REQUIRED-NEW | ORPHAN — gated on typed-path re-profile |
| C6 | i8mm batch decode | `usmmla` | **NONE** (cold leaf + kernel absent) | would-be (oracle = C5 `parse_4_digits` scalar twin) | would-be | GATED CONTINGENCY, not active candidate (twins CF-4b) — doubly orphan-blocked |

## §3 — Grammar-neutrality (each candidate: JSON-only or CSS/Sheets/BBNF-self generalisable)
### (and the REDRESS-blocked instruction-route flags PASS-2 §2 row P2-C asks for)

**Grammar-neutral verdict per candidate (the CH2 / Lock 14 disposition):**

- **C2 (eq-set fan classify) — GRAMMAR-NEUTRAL, JSON+CSS witnessed.** The kernel
  `byte_class_from_eq_set_64_neon` takes a `set: &[u8]` of ≤8 bytes as a *runtime parameter* —
  it is the alphabet that is grammar-specific, never the instruction. JSON's structural scan
  (`json/scan.rs:217-219`) and the CSS delimiter find share the identical instruction, parameterized
  by the alphabet through `select_classifier(alphabet)` (`dispatch.rs:42`). This is exactly the
  Lock 14 neutrality vehicle (SYNTHESIS §0.1 NEON gate: "the alphabet is the only CSS-specific
  datum; the kernel is shared"). The non-JSON exercise is `css_l4` (a real rider sharing the
  kernel, SYNTHESIS `simd_non_json_exercise:376`). Verdict: ADMIT, grammar-neutral.
- **C1 (lo6 TBL) — GRAMMAR-NEUTRAL but ALPHABET-INADMISSIBLE for CSS.** The TBL kernel is itself
  grammar-neutral (JSON uses it); the `lo6_table_admissible` guard is the neutral gate. CSS's
  alphabet collides under the low-6-bit mask (`byte & 0x3f`, `dispatch.rs:106`), so the *honest*
  answer is C1 falls back to C2/scalar for CSS — NOT a
  CSS special-case, exactly the guard's purpose. Verdict: the kernel is neutral; for CSS the
  route is C2.
- **C3 (shrn movemask) — VERDICT: PASS (grammar-neutral).** Pure bit-packing of a 16×u8 vector
  compare into a `u64`; it carries NO grammar datum whatever (no alphabet, no byte set, no role
  table) — the same `vshrn_n_u16::<4>` spill serves JSON structurals, the CSS delimiter scan, and
  every future Sheets/BBNF-self classify identically. Ships folded into C1/C2; no per-grammar
  template surface required.
- **C4 (host CTZ extract) — VERDICT: PASS (grammar-neutral).** Pure mask→first-set-index
  (`trailing_zeros` → `rbit`+`clz`); carries NO grammar datum. Identical across every grammar's
  classify mask. Ships folded into C1/C2; the bulk-consumer form remains REDRESS-89 blocked for
  all grammars equally (§3 item 2), so the neutrality is uniform.
- **C5 (UDOT 4-digit) — VERDICT: grammar-neutral IN SHAPE, but CSS-ORPHAN (deferred to P2-F CF-4a).**
  The digit-run decode shape is grammar-neutral (JSON numbers, CSS dimensions, Sheets numerics all
  decode a 4-digit run through one `parse_4_digits`); the parameter is the digit run, never a
  grammar role. The block is NOT a neutrality failure — it is a *missing-antecedent* failure (CH1:
  no named CSS hot leaf — P1-E §2.5 "no number leaf is hot"). Disposition is therefore DEFERRED, not
  rejected-for-overfit: it admits only through the P2-F CF-4a typed-path re-profile gate.
- **C6 (i8mm batch decode) — VERDICT: grammar-neutral IN SHAPE, but CSS-ORPHAN + kernel-absent
  (deferred to P2-F CF-4b).** Same neutral digit-run shape as C5; same missing-antecedent block,
  plus the kernel does not exist. Disposition is a gated contingency (P2-F CF-4b), NOT an active
  candidate; it admits only if the CF-4b re-profile gate fires. Reuses C5's `parse_4_digits` scalar
  twin as its parity oracle.

**REDRESS-blocked instruction routes (the P2-C flag deliverable — each named with refutation):**

1. **PMULL prefix-XOR as the hot `bitmap_prefix_xor_64` body — REDRESS-88 BLOCKED.**
   (`skinny/REDRESS.md:2510`, Item 88.) Measured refutation: PMULL as the default hot prefix-XOR
   body regressed escape-heavy and narrow parse-only JSON rows (`numbers/track1_generated`
   -10.04%, `unicode_escapes` -12.66%/-15.52%) even though the primitive is correct in asm.
   `bitmap_prefix_xor_64_neon` (`aarch64/bitmap_prefix_xor_64.rs:2`) MUST stay the scalar 6-step
   shift-XOR cascade. **CSS relevance:** the prefix-XOR is the quote/string-region fill — if a CSS
   NEON scan computes a string-body mask (to mask out delimiters inside strings, the
   `consume_string_at` semantics of the hot leaf), it must NOT promote PMULL into the hot body;
   the scalar cascade is the admissible carrier. Re-open test (CH3 fail): any `pmull`/`vmull_p64`
   text in the CSS prefix-XOR / string-region source on the hot path.

2. **CSSC CTZ as a dedicated bulk next-bit consumer — REDRESS-89 BLOCKED.**
   (`skinny/REDRESS.md:2544`, Item 89.) Refutation: the W10b candidate replaced the
   `bitmap_next_set_bit_neon` scalar delegate with a local `trailing_zeros` body and consumed it
   from a `bulk_emit_positions_64_neon` bulk loop — rejected; prefix-XOR stays scalar, the bulk
   CTZ consumer is not admitted (REDRESS:2595: "the CSSC CTZ/bulk consumer remains rejected by
   Item 89"). **The admissible form is C4** — a single `trailing_zeros` first-match extract per
   mask (already in `find_ascii_set_member64`), NOT a CTZ-driven bulk emit loop. Re-open test
   (CH3 fail): a CSS scan that promotes CTZ into a standalone bulk-bit-iterate consumer body.

3. **REDRESS-28/33 Class-A NEON tiny-string wiring — BLOCKED as a CSS consumer.** `match_tiny_plain_string`
   (`aarch64/match_tiny_plain_string.rs`) is a genuine NEON body but its *wiring as a hot consumer*
   is REDRESS-28/33 blocked (alphaC pre-block family, SYNTHESIS §0.4 inherited
   "28+33"). CSS string-skip (`consume_string_at`) must NOT route through a tiny-string NEON probe
   as a hot body. (Re-open test: a `match_tiny_plain_string` call on the CSS hot path.)

4. **REDRESS-82 single-quartet unicode classifier / REDRESS-84 object-pair compaction — BLOCKED.**
   Inherited family (SYNTHESIS §0.4 "82-84"). CSS treats `>=0x80` as a name byte with no codepoint
   work (P1-E §2.5: "no unicode decode") — there is no unicode-classify hot leaf, so no
   single-quartet classifier is even a candidate. Flagged for completeness: any CSS unicode-decode
   NEON kernel is orphan (no antecedent) AND REDRESS-82 blocked.

5. **x86 AVX2 / AVX-512 / GFNI (`gf2p8affineqb`) / VBMI2 (`vpcompressb`) / VPCLMUL — BLOCKED
   (whole-pass, out-of-scope).** (alphaC §6; REDRESS x86 sites `x86_64/avx2/classify.rs:31`,
   `avx512_vbmi2/classify.rs:28`, `avx512_gfni/classify_affine.rs:31`, `avx512_bitalg/multiclass.rs:30`.)
   The `bbnf-simd/src/x86_64/` tree exists but is diagnostic-only / dead on the aarch64 host. The
   esoteric x86 routes the scope matrix names — GFNI affine byte-classify (`gf2p8affineqb` as a
   one-instruction 8-bit-class map), VBMI2 `vpcompressb` (one-instruction mask-compaction, the
   `bulk_emit_positions` analog), VPCLMUL `vpclmulqdq` (vectorized prefix-XOR) — are genuinely
   powerful, but **none may appear as an SK-V17 same-wave consumer or row-movement claim** (CH4/CH6
   fail). They are noted in this inventory solely because the scope matrix requests the secondary
   x86 esoterica; the aarch64 analogs (TBL for GFNI-affine, the scalar bulk-emit for `vpcompressb`,
   the scalar cascade for VPCLMUL) are the admissible host routes.

6. **SVE / SVE2 / SME / AMX — BLOCKED (no host hardware).** Apple M5 Max has no SVE (alphaC §6,
   sk-v16-arch:265-266). SVE-predicated whole-vector classify, SVE `MATCH`/`NMATCH` (the
   one-instruction set-membership the eq-set fan emulates), SVE2 `HISTSEG`/`HISTCNT`, SME outer-
   products — all dead code on the host. Re-open test (CH4/Lock 16): any SVE/SME primitive filed as
   NEON, or `svptrue`/`svmatch`/`sme` text in the CSS path.

7. **Runtime `is_aarch64_feature_detected!` in the per-leaf hot loop — BLOCKED.** Feature detection
   threads through the `PrimitiveKernels` OnceLock table ONCE (`dispatch.rs:58`), never per leaf
   (alphaE C4a/C4b pre-block). DotProd is a compile-time `target_feature` (`digit_mac.rs:25`), not
   a per-leaf runtime check. Re-open test: `is_aarch64_feature_detected!` inside a per-byte/per-leaf
   loop.

## §4 — Risks (REDRESS entries any candidate must NOT re-open)

- **C5/C6 orphan-kernel risk (CH4 + the no-orphan-kernel non-negotiable).** The single largest
  P2-C risk is that the host carries seductive instructions (`udot` present and idle, `usmmla`
  available) with NO benched CSS antecedent. Any S-P3 shortlist that lands C5 or C6 without the
  Wave-5 typed-path re-profile proving a top-N tailwind digit leaf re-opens the orphan-kernel
  failure (architecture-doc "`select_classifier` built but dead for CSS"). C5/C6 are inventory-only
  here, explicitly NOT proposed; they admit ONLY through their gate (alphaE C4a unconditional-wiring
  for the *existing* udot is the narrow exception, but even it must show the leaf is reached, not
  just that the kernel parity-passes).
- **REDRESS-88 (PMULL hot body) re-open via a CSS string-region mask.** If a CSS NEON scan needs a
  string-body mask (to suppress in-string delimiters, the `consume_string_at` semantics), the
  prefix-XOR carrier must stay scalar. Risk: an implementor "optimizes" the string-region fill with
  PMULL. BLOCKED.
- **REDRESS-89 (CSSC CTZ bulk consumer) re-open via a bulk-emit CSS index.** If the CSS scan emits a
  position index from the structural mask, it must use the scalar `bulk_emit_positions` /
  single-`trailing_zeros` extract, NOT a CTZ-driven bulk-iterate body. BLOCKED.
- **REDRESS-28/33, 82-84 (Class-A tiny-string, unicode-quartet, object-pair) re-open via CSS
  string/comment skip.** The CSS string/comment-skip leaves must not route through the blocked NEON
  consumer families. BLOCKED.
- **x86 / SVE re-open (REDRESS x86 family, alphaC §6).** Any x86/AVX/GFNI/VBMI2/VPCLMUL or SVE/SME
  same-wave consumer or row-movement claim. BLOCKED whole-pass.
- **C1 lo6-collision masking risk (CH4/CH1).** The `;`/`{` low-6-bit (`& 0x3f`) collision means a careless lo6
  TBL route would silently produce wrong positions for CSS. The `lo6_table_admissible` guard
  (`dispatch.rs:101`) catches this and forces scalar/C2; the risk is an implementor bypassing the
  guard. The honest route is C2 (eq-set, no lo6 table).
- **Movemask divergence (CH4).** The duplicated slower movemask in `byte_class_from_eq_set_64.rs:79`
  vs. the SOTA `vshrn` in `movemask.rs:4` — C3 must consolidate to one, or a divergent mask flips an
  index bit. Caught by corpus-parity, but it is a live source-duplication risk.
- **No second substrate (Lock 1).** Every NEON candidate here produces ONLY a `Vec<u32>` structural
  index that the tape consumes (SYNTHESIS §0.1: "produces only a `Vec<u32>` structural index, and
  the tape consumes it"). A SIMD mask stream is a transient producer, not a retained sidecar
  (SYNTHESIS §0.4); cross-call classifier-state retention is REJECT under Lock 1 v+1. No candidate
  may retain the structural mask across calls.

## §5 — Sources (every external citation — comparator source, ISA manual, prior tranche)

- **S-P1 profile (the candidate pool):** `restart/skinny/tranches/sk-v17/research/p1/p1e-hot-leaf-attribution.md`
  §2.3 (`find_component_delim` 56.52% / `consume_balanced_at` 11.05%), §2.5 (no number/unicode/dispatch/tape
  hot leaf), §4 anomaly 3-4 (C4b orphan-block, NEON gated behind tape);
  `restart/skinny/tranches/sk-v17/research/p1/hardening/HARDENING-S-P1-V4-CONSOLIDATED.md`
  §3.3 (59.24% / 10.31%, primitive antecedents), §3.4 (lever order: tape first then NEON),
  orphan-blocked clause (udot `digit_mac.rs:27` no benched CSS antecedent).
- **Benched CSS hot-leaf source:** `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs`
  (`find_component_delim` :288-311, `find_colon_before` :314, `consume_balanced_at` :320-340,
  `emit_fact_stream` :5, `push_ascii_lower_hex` :628).
- **bbnf-simd aarch64 vocabulary (audited this cycle):** `crates/bbnf-simd/src/aarch64/classify_tbl4.rs`
  (`vqtbl4q_u8` :31,:82; `classify_structural_terminator_chunk_from_table` :75),
  `aarch64/byte_class_from_eq_set_64.rs` (genuine NEON eq-set fan :33-73, the slow movemask :79),
  `aarch64/movemask.rs` (SOTA `vshrn_n_u16::<4>` :4-25), `aarch64/match_tiny_plain_string.rs`
  (`vqtbl4q_u8`+`rbit/clz` :87, REDRESS-28/33-blocked consumer), the scalar-delegate stubs
  `aarch64/{byte_class_from_table_64,bitmap_prefix_xor_64,bitmap_next_set_bit,bulk_emit_positions_64}.rs`,
  `aarch64/digit_mac.rs` (`udot` asm :40, scalar twin :15-22, `sdot` :63).
- **Dispatch / neutrality vehicle:** `crates/bbnf-simd/src/dispatch.rs` (`select_classifier` :42,
  `PrimitiveKernels` :50, `primitive_kernels` OnceLock :58, `lo6_table_admissible` :101 — the
  low-6-bit (& 0x3f) collision guard); `crates/bbnf-simd/src/lib.rs` (`find_ascii_set_member64` :209-222,
  `prefix_xor_64` :170, `escape_mask_64` :175, `compact_mask` :229, `prim` table :251-282).
- **JSON antecedent (the shared kernel, the model to copy):** `crates/runtime/src/grammars/json/scan.rs`
  (the NEON `scan` fn :207, `classify_structural_terminator_block_from_table` call :217-219,
  `escape_mask_64`/`prefix_xor_64` string-region :237-239), `json/value.rs:143` (`value_from_ref` lazy cursor).
- **Scalar references (the checkasm anchors):** `crates/bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs`
  (the 6-step shift-XOR cascade PMULL would replace), `scalar/bitmap_next_set_bit.rs`
  (`trailing_zeros` host-CTZ reference), `scalar/byte_class_from_eq_set_64.rs`,
  `scalar/byte_class_from_table_64.rs`.
- **REDRESS-blocked instruction routes:** `skinny/REDRESS.md` Item 88 (:2510, PMULL prefix-XOR hot
  body rejected, measured -10/-12/-15% regression), Item 89 (:2544, CSSC CTZ bulk consumer + B6
  canary rejected), Item 90 (:2589, PMULL/CSSC remain rejected :2595), the C4 "keeps PMULL, CSSC,
  DotProd, SVE/SME, x86 AVX-512 unadmitted" note (:2123), `checkasm_*` test names (:2555).
- **Contract / locks / pre-blocks:** `restart/skinny/tranches/sk-v17/SYNTHESIS.md` §0.1 (NEON
  hot-leaf union gate, `simd_non_json_exercise`), §0.4 (REDRESS pre-block families "28+33, 50-55,
  60-72, 80, 82-84, 88, 89"; SVE/x86 block; transient-producer-not-sidecar), §63 (i8mm absent),
  Section 2 telemetry (`native_simd_status`, `simd_non_json_exercise`);
  `restart/skinny/tranches/sk-v17/research/alpha/alphaC-redress-digest.md` §6 (x86/AVX/SVE block),
  §7 (consolidated pre-block ledger), `alphaE-candidate-shortlist.md` C2 (NEON pre-scan, the
  `lo6_table_admissible` neutrality), C4a/C4b (udot orphan-wiring + i8mm gated, the "no orphan
  kernel" gate), :110-116 (PrimitiveKernels / digit_mac / i8mm-absent anchors).
- **ISA references (aarch64):** Arm Architecture Reference Manual for A-profile — `TBL`/`TBX`
  (vector table lookup, the `vqtbl4q_u8` 4-register form), `SHRN`/`SHRN2` (shift-right-narrow, the
  movemask spill), `UDOT`/`SDOT` (DotProd, FEAT_DotProd), `USMMLA`/`UMMLA` (FEAT_I8MM matrix
  multiply), `RBIT`+`CLZ` and FEAT_CSSC `CTZ` (count-trailing-zeros first-match extract), `PMULL`
  (FEAT_AES polynomial/carryless multiply). x86 secondary (SDM Vol. 2): `VPSHUFB` (AVX2 byte
  shuffle, the TBL analog), `GF2P8AFFINEQB` (GFNI affine byte-class), `VPCOMPRESSB` (VBMI2
  mask-compaction), `VPCLMULQDQ` (VPCLMUL carryless multiply). All x86/SVE/SME routes are
  out-of-scope per §3.5-6.
