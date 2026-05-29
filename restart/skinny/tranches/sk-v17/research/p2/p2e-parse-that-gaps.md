# SK-V17 P2-E: parse-that primitive gaps

Pass: S-P2 Research. Cycle: V3.
Date: 2026-05-29.
Scope: parse-that's primitive vocabulary (`skinny/crates/bbnf-simd/`) interrogated against the
S-P1 CSS-tape hot leaves; per gap — the missing primitive's shape, its scalar-reference sketch,
its Layer-0/Layer-1 placement in the bbnf-simd two-layer vocabulary; and an explicit ALREADY-PRESENT
ledger so S-P3 does not re-author extant kernels.
Output: this file.
P1 hot-leaf antecedents: `CssFullParser::find_component_delim` (`css_l4_declaration_values/generated.rs:288`,
56.52%/59.24% self-time, scan); `CssFullParser::consume_balanced_at` (`generated.rs:320`, 11.05%/10.31%,
structural-over-scan, SAME inner loop); `emit_fact_stream` String accumulator (`generated.rs:5`, 24.59%
+ 91% of the ~58–64% syscall/heap alloc floor, string/tape); `push_ascii_lower_hex` (`generated.rs:628`,
8.98%, FNV/hex DIAGNOSTIC — explicitly NO primitive). NO number/unicode/dispatch hot leaf on either
benched plane (P1-E §2.5; HARDENING-S-P1-V4 §3.3).
Lock surface: Lock 1 (substrate union — the scan produces only a `Vec<u32>` structural index the tape
consumes; no parallel substrate / no retained classifier state) and Lock 14 (grammar-neutrality — every
gap is an alphabet-parameterised byte-set / classifier / mask op, never CSS-keyed). Both touched.

## §1 — Findings (concrete; file:line on bbnf claims, citation on external claims)

### 1.0 — The two-layer vocabulary, as it actually exists in the tree

bbnf-simd already implements the `general-infra-crates` two-layer scheme (PASS-2 §8.3):

- **Layer-0 (vendored, include-only):** `skinny/crates/bbnf-simd/ext/x86/{x86inc.asm, x86util.asm, bbnf.asm}`
  — the FFmpeg/dav1d-vendored x86 assembler macro substrate (`build.rs:1` "assembles vendored + authored
  x86_64 .asm sources"; `build.rs:46` "the vendored headers are include-only"). On aarch64 hosts `build.rs`
  returns early; Layer-0 is x86-only and out of scope for SK-V17 (Apple M5 Max, NEON — `SYNTHESIS.md §0.4`
  no-x86 pre-block). **No aarch64 primitive may be placed in Layer-0**; there is no vendored aarch64 asm,
  and authoring one would be a new subsystem (CH5).
- **Layer-1 (bbnf-authored primitives):** `skinny/crates/bbnf-simd/src/{scalar,aarch64,x86_64}/<primitive>.rs`,
  one file per primitive (`no-god-modules` honoured — each is `byte_class_from_table_64.rs`,
  `bitmap_prefix_xor_64.rs`, etc.), each with a scalar twin under `src/scalar/` that is the executable
  specification and the checkasm parity anchor (`scalar/byte_class_from_eq_set_64.rs:1` "the executable
  specification"; Lock 16). Dispatch is the `PrimitiveKernels` OnceLock fn-table (`dispatch.rs:50-87`) and
  `select_classifier(alphabet)` (`dispatch.rs:42`).

**Every gap below is a Layer-1 aarch64 primitive (NEON) with a scalar twin.** No gap is Layer-0; no gap
re-authors a vendored construct.

### 1.1 — The hot leaf, decomposed (what `find_component_delim` actually does)

`find_component_delim` (`generated.rs:288-311`) is NOT a flat block scan. Its inner loop (`:293-308`) is a
byte-at-a-time walk that, per byte, does a 3-byte membership test (`delimiters.contains(&byte)` :295, the
56.5% hot leaf — sets are `b";{}"` / `b"{};"` / `b":{};"` / `b";}"` per call site, P1-D §2.5) AND, on a
miss, a state-transition `match` (`:298-307`) that SKIPS structured regions: strings (`consume_string_at`),
comments (`consume_comment_at`), and **nested balanced brackets** (`consume_balanced_at`, recursing on
`(`/`[`/`{`). `consume_balanced_at` (`:320-340`) is byte-for-byte the same inner loop reached recursively
(`:322-338 ≡ :293-308`), differing only in the membership test (`byte == close` :324 vs `delimiters.contains`
:295) — P1-E §2.3 / HARDENING-S-P1-V4 §3.3 establish these are ONE primitive target, ~69% of recognition
self-time combined.

This decomposition is load-bearing for the gap analysis: the cost is **byte-membership** (vectorisable today),
gated by a **stateful skip machine** (string/comment/bracket regions that must be jumped, not classified
flat). JSON's structural scan (`json/scan.rs`) handles ONE of these states — string-body masking via
prefix-XOR (`json/scan.rs:239`, `prefix_xor_64`) — but has NO comment state and NO nested-bracket-balance
state, because JSON containers are themselves structural (emitted to the index), whereas CSS component-value
brackets must be SKIPPED OVER as opaque until the matching close. That is the precise parse-that gap.

### 1.2 — ALREADY-PRESENT ledger (verified extant; S-P3 must NOT re-author these)

The membership-find that `find_component_delim`'s 56.5% `delimiters.contains` grounds is **already exposed**:

| Extant primitive | file:line | what it does | covers which part of the hot leaf |
|---|---|---|---|
| `find_ascii_set_member64(bytes, cursor, end, set≤8)` | `lib.rs:209-226` | block-stride scan; returns the first index ≥ cursor whose byte ∈ `set` (≤8), 64-byte NEON stride + scalar tail | the FLAT `delimiters.contains(&byte)` find (the 3-byte CSS delimiter sets are all ≤8) — the membership core of the hot leaf, **already vectorised**, already checkasm-gated (`checkasm_ascii_set_member_find_64.rs:30,159`) |
| `prim::byte_class_from_eq_set_64(src[64], set≤8)` | `lib.rs:282`; NEON `aarch64/byte_class_from_eq_set_64.rs:33`; scalar `scalar/byte_class_from_eq_set_64.rs:26` | 64-bit mask, bit i set iff `src[i] ∈ set` (≤8); NEON = per-member `vceqq_u8` fanned through `vorrq_u8`, packed via movemask | the kernel under `find_ascii_set_member64`; the mask form for tape emission |
| `select_classifier(alphabet:&[u8;64])` + `classify_tbl4` (`vqtbl4q_u8` lo6 table) | `dispatch.rs:42`; `aarch64/classify_tbl4.rs:47` | alphabet-parameterised 64-byte classify → `structural_mask`/`quote_mask`/`backslash_mask`/`control_mask`; `lo6_table_admissible` (`dispatch.rs:101`, computing `byte & 0x3f` at `:106`) falls back to scalar on low-6-bit (`& 0x3f`) collision | a LARGER (>8, ≤ lo6-admissible) delimiter alphabet; the JSON-shared neutrality vehicle |
| `prefix_xor_64(mask, carry_in)` | `lib.rs:170`; `bitmap_prefix_xor_64` | running-parity prefix-XOR → string-body mask from quote mask | the STRING-skip state (one of the three CSS skip states) — already used by JSON `scan.rs:239` |
| `escape_mask_64(bs_mask, carry)` | `lib.rs:175-206` | backslash escape masking across a 64-block | the escaped-quote sub-case inside the string-skip state |
| `compact_mask` / `bulk_emit_positions_64` | `lib.rs:229`; `aarch64/bulk_emit_positions_64.rs` | branchless mask → `Vec<u32>` position append | index materialisation into the tape |
| `bitmap_next_set_bit(mask, cursor)` | `lib.rs:265`; `bitmap_next_set_bit.rs` | next set bit ≥ cursor (the CSSC/CTZ family consumer) | iterate the structural index |
| `eob_pad_clamp(&[u8]) -> EobBlock` | `lib.rs:275`; `scalar/eob_pad_clamp.rs:8` | tail-block zero-pad clamp for sub-64 remainders | the scan tail (CSS sheets are not 64-aligned) |
| `parse_4_digits` / `parse_4_digits_dotprod` (udot) | `aarch64/digit_mac.rs:5,27` | 4-ASCII-digit → u32 via udot; scalar twin present | **ORPHAN** — no benched CSS digit self-time (P1-E §4.4a); re-admission gated to post-W1/W2 typed-path re-profile |

**Conclusion of the ledger:** the membership-find half of the 56.5% hot leaf is NOT a parse-that gap — it is
already exposed and checkasm-gated. The gap is everything the FLAT find cannot do: honour the CSS skip-state
machine (string/comment/bracket) so that delimiters *inside* a skipped region are not falsely matched, and
track nested-bracket DEPTH so `consume_balanced_at`'s recursion collapses to a flat block stride. Two of the
three skip states (string, escape) are covered; **comment-skip and bracket-balance-depth are the genuine
missing primitives.** The udot digit kernel is present-but-orphan (no antecedent on the current planes).

### 1.3 — Why the masks alone don't close it (the structural truth)

A flat `byte_class_from_eq_set_64` over `b";{}"` would set bits for every `;`/`{`/`}` in the 64-block —
INCLUDING those inside `"..."`, `/* ... */`, and `( ... )` component blocks, which are SEMANTICALLY not
delimiters. The scalar hot leaf is slow precisely because it walks byte-at-a-time to maintain the skip state
so it can ignore those. The vectorised path must reproduce that suppression as MASK ALGEBRA: produce the raw
delimiter mask, then AND-NOT the string-body mask (have it: `prefix_xor_64`), the comment-body mask (GAP G1),
and the bracket-interior mask (GAP G2). This is the simdjson/asmjson "stage-1 then suppress" shape
(simdjson `find_structural_bits` → `string_scanner` body suppression; Langdale & Lemire, *Parsing Gigabytes
of JSON per Second*, §3 "Identifying white-space and structural characters" + §4 string masking) — JSON ships
two suppressors (whitespace, string); CSS needs two MORE (comment, bracket-depth). That is the whole gap.

## §2 — Candidate primitives (each: shape + scalar-ref status + arch + P1 antecedent)

Five gaps. G1/G2 are the load-bearing missing primitives; G3 is a thin composition seam; G4 is a present
kernel needing only a parity test; G5 is the explicit NON-candidate (the FNV/hex leaf) recorded so it is not
mistaken for a primitive.

### G1 — `comment_body_mask_64` (NEON block-comment-region suppressor, digraph-parameterised)

- **Shape.** `fn comment_body_mask_64(src: &[u8;64], open: [u8;2], close: [u8;2], in_comment_carry: bool) -> (u64 /*body mask*/, bool /*carry out*/)`.
  The open/close digraphs are PARAMETERS, not literals: CSS passes `open=[b'/',b'*']`, `close=[b'*',b'/']`,
  but the kernel never references `/`/`*` — any grammar with a 2-byte block-comment digraph (SQL `/* */`,
  Pascal `(* *)`, HTML `<!-- -->` would need a 4-byte variant, out of this kernel's 2-byte shape) drives it
  by its own digraph. Produces a 64-bit mask with bit i set iff byte i lies inside an `open … close` block
  comment (inclusive of the open/close bytes), threading a 1-bit carry across blocks (a comment can span >64
  bytes). Construction: compute the `open`-mask (`vceqq_u8(chunk,open[0])` AND `(vceqq_u8(chunk,open[1])>>1)`)
  and `close`-mask (`vceqq_u8(chunk,close[0])` AND `(vceqq_u8(chunk,close[1])>>1)`) from two 2-byte shifted
  equality masks each, then run a prefix-region fill between open and close — the SAME prefix-XOR-region shape
  as the string-body mask, except the open/close tokens are 2-byte digraphs not a single self-terminating
  quote, so it needs a "match within a run" fill rather than parity. A correct construction is:
  `body = prefix_or_region(open_mask, close_mask, carry)` realised as a running fill (`open` sets the region
  on, `close` (shifted to include the trailing digraph byte) sets it off).
- **Scalar-ref status.** ABSENT — must be authored. Scalar reference sketch (the executable spec, `src/scalar/comment_body_mask_64.rs`)
  — note it tests `open[0]/open[1]` and `close[0]/close[1]`, never a literal `/` or `*`:
  ```
  fn comment_body_mask_64(src:&[u8;64], open:[u8;2], close:[u8;2], carry_in:bool) -> (u64,bool) {
      let mut mask = 0u64; let mut inside = carry_in;
      let mut i = 0;
      while i < 64 {
          if !inside && src[i]==open[0] && i+1<64 && src[i+1]==open[1] { inside=true; mask |= 3u64<<i; i+=2; continue; }
          if inside { mask |= 1u64<<i; if src[i]==close[0] && i+1<64 && src[i+1]==close[1] { mask |= 1u64<<(i+1); inside=false; i+=2; continue; } }
          i += 1;
      }
      (mask, inside)   // NB: i+1==64 digraph straddle handled by carry into next block's first byte
  }
  ```
  The NEON body is a `vceqq_u8(chunk, open[0])`/`vceqq_u8(chunk, open[1])` (and the close pair) set of compares
  + shifted-AND for the digraphs + a 64-bit running region-fill (the same `overflowing_add`/carry idiom
  `escape_mask_64` already uses at `lib.rs:188`). The compare operands are the digraph parameters, so the
  kernel is alphabet-driven by construction (the Lock-14 fix the V1 CHALLENGE required).
- **Arch.** aarch64 NEON (Layer-1, `src/aarch64/comment_body_mask_64.rs`); scalar twin `src/scalar/`. No x86.
- **P1 antecedent.** `find_component_delim`/`consume_balanced_at` skip-state machine — the `b'/' if byte_at(pos+1)==b'*'`
  arm (`generated.rs:300, 329`) that `consume_comment_at` (`:342-351`) services byte-at-a-time. Part of the
  ~69% combined scan self-time; the comment-suppress is one of the two missing AND-NOT masks (§1.3).

### G2 — `bracket_depth_mask_64` (NEON nested-bracket interior suppressor + depth carry)

- **Shape.** `fn bracket_depth_mask_64(open_mask: u64, close_mask: u64, depth_carry: i32) -> (u64 /*interior mask*/, i32 /*depth out*/)`.
  Given the open-bracket mask (`(`/`[`/`{`) and close-bracket mask (`)`/`]`/`}`) for a 64-block — both produced
  by the EXISTING `byte_class_from_eq_set_64` — and the running nesting depth carried in, produce a mask with
  bit i set iff byte i is STRICTLY INSIDE a component block (depth ≥ 1 at that byte, i.e. after an unmatched
  open and before its matching close), and the depth at block end. This is the primitive that collapses
  `consume_balanced_at`'s RECURSION into a flat block stride: instead of recursing per nested bracket, one
  block-parallel pass computes the interior mask, and the delimiter scan AND-NOTs it so top-level `;`/`{`/`}`
  inside `calc((a+b)*c)` etc. are suppressed.
  The `depth_carry` is an i32 threaded WITHIN a single `scan_components_to_index` call ONLY: it is initialised
  to 0 at the start of each parse and discarded at end-of-input — there is NO cross-call depth retention (the
  `SYNTHESIS.md §0.4` "carry stays within a single chunk-call; cross-call classifier state remains rejected"
  invariant, stated here at the candidate shape, not deferred to §4). It is a within-chunk running balance,
  never a retained cursor.
- **Scalar-ref status.** ABSENT — must be authored. Scalar reference sketch (`src/scalar/bracket_depth_mask_64.rs`):
  ```
  let mut mask = 0u64; let mut depth = depth_carry;
  for i in 0..64 {
      let opens = (open_mask>>i)&1==1; let closes = (close_mask>>i)&1==1;
      if opens { if depth>=1 { mask |= 1u64<<i; } depth += 1; }   // the '(' itself is interior iff already nested
      else if closes { depth -= 1; if depth>=1 { mask |= 1u64<<i; } }
      else if depth>=1 { mask |= 1u64<<i; }
  }
  (mask, depth)
  ```
  This is the well-known prefix-sum / running-balance shape. **The shipped/default body is the scalar running
  balance over the two precomputed masks** (the spec twin itself): 1 pass, no recursion, no per-bracket call
  frame — already a large win over the recursive `consume_balanced_at`, and REDRESS-89-clean because it carries
  NO CTZ next-bit bulk-iteration. The CTZ "ranges" refinement (open positions paired to close positions via
  `bitmap_next_set_bit` iteration on the combined mask, the Validark-style trick for blocks with no underflowing
  closes) is NOT co-equal: it is a checkasm-gated, parity-proven, CONSUMER-ONLY optional path admitted only after
  it beats the scalar balance on the bench — and if S-P3 ever shortlists it as the UNCONDITIONAL body, CH3 must
  REVISE it back to the consumer framing (the §4 REDRESS-89 bound, promoted inline here per the V1 CHALLENGE).
  This agrees with **`p2c-arch-esoterica.md` §3.2** (which forbids CTZ as a bulk-iterate body): the two artefacts
  are reconciled — bulk-default CTZ is REDRESS-89-blocked, and a parity-gated consumer-of-a-precomputed-mask is
  the only admissible CTZ use. The vector form is NOT pure mask algebra (a true prefix-sum of ±1 needs a scan),
  so the NEON variant of the running balance is honest about its cost and its parity is the checkasm gate.
- **Arch.** aarch64 NEON Layer-1 (`src/aarch64/bracket_depth_mask_64.rs`) + scalar twin. CTZ iteration uses
  `bitmap_next_set_bit` (extant, `lib.rs:265`) — the REDRESS-89 CSSC/CTZ family, admitted here as a CONSUMER
  of the depth pass, NOT as a default hot prefix-XOR body (the REDRESS-89 failure mode, §4).
- **P1 antecedent.** `consume_balanced_at` (`generated.rs:320`, 11.05%/10.31%) + the `(`/`[`/`{` arms of
  `find_component_delim` (`:301-303`). This is the SECOND missing AND-NOT mask (§1.3) and the primitive that
  directly retires the 11% recursive-scan self-time.

### G3 — `scan_components_to_index` (CSS structural-index assembler over the suppressor masks)

- **Shape.** `fn scan_components_to_index(input:&[u8], delim_set:&[u8;≤8]) -> Vec<u32>` — the CSS analogue of
  JSON's `scan_structurals` (`json/scan.rs:22`). Per 64-block: `raw = byte_class_from_eq_set_64(block, delim_set)`
  [extant]; `string = prefix_xor_64(quote_mask, carry)` [extant]; `comment = comment_body_mask_64(...)` [G1];
  `bracket = bracket_depth_mask_64(open, close, depth)` [G2]; `emit = raw & !string & !comment & !bracket`;
  `compact_mask(base, emit, &mut positions)` [extant]. Produces ONLY a `Vec<u32>` structural index (Lock 1 —
  the structural projection IS the tape, no sidecar).
- **Scalar-ref status.** Composition, not a kernel — its "scalar reference" is `scan_structurals_scalar`
  (`json/scan.rs:32`) mirrored for the CSS alphabet, i.e. the existing byte-at-a-time `find_component_delim`
  walk IS the executable spec the assembled index must match byte-exact (the parity anchor). No new asm.
- **Arch.** aarch64 (uses NEON kernels) with scalar fallback; lives in `runtime/src/grammars/css_l4_*/scan.rs`
  (a per-grammar consumer, NOT in bbnf-simd — the alphabet is the only CSS-specific datum; Lock 14 neutrality
  via `select_classifier(alphabet)`). This is the seam alphaE-C2 names; P2-E's contribution is that it is
  blocked on G1+G2 existing first — alphaE-C2 assumed the JSON kernels suffice, but the §1.3 suppression
  analysis shows two suppressor masks are MISSING.
- **P1 antecedent.** The whole ~69% recognition scan (`find_component_delim` + `consume_balanced_at`). This is
  the consumer that makes G1/G2 same-wave-consumed (CH4) and the tape the index feeds (Lock 1).

### G4 — `parse_4_digits` udot parity test (present kernel, missing checkasm gate)

- **Shape.** No new kernel — `parse_4_digits_dotprod` (`aarch64/digit_mac.rs:27`) and its scalar twin
  (`digit_mac.rs:15-22`) both EXIST. The gap is the MISSING `checkasm_digit_mac` parity test (verified absent:
  `ls tests/ | grep digit` = empty). Per Lock 16 a kernel without a checkasm parity gate is not admission-ready.
- **Scalar-ref status.** PRESENT (`digit_mac.rs:15-22`, the `#[cfg(not(target_feature="dotprod"))]` byte loop).
  The gap is the parity TEST, not the reference.
- **Arch.** aarch64 dotprod (`udot`); a `checkasm_digit_mac.rs` mirroring `checkasm_byte_class_from_eq_set_64.rs`.
- **P1 antecedent.** NONE on the current planes — P1-E §4.4a / HARDENING-S-P1-V4 §3.3 record ZERO digit-parse
  self-time on either benched CSS plane (recognition counts, it does not decode dimensions). **G4 is therefore
  GATED, not free-standing:** it admits ONLY after a post-W1/W2 re-profile on the typed lazy-`ValueRef` path
  proves a dimension/number-decode leaf is top-N (the SYNTHESIS §0.3 C4a/C4b split; alphaE C4a admits the
  WIRING unconditionally but the SK-V17 contract gates the i8mm net-new kernel C4b behind the re-profile).
  P2-E's parse-that-gap reading: the udot kernel is present-but-orphan; the only parse-that DELIVERABLE here
  is its checkasm gate, and even that is sequenced behind the typed-path re-profile that supplies the antecedent.

### G5 — NON-candidate (recorded to prevent mis-admission): `push_ascii_lower_hex` / FNV

- **Shape.** `push_ascii_lower_hex` (`generated.rs:628`, 8.98%/9.11% self-time) is FNV64 → lowercase-hex
  serialisation of a source-hash diagnostic field. It is a hot LEAF but it is NOT a primitive antecedent — it
  is a DIAGNOSTIC encode with no CSS-semantic value (HARDENING-S-P1-V4 §3.3: "NONE — FNV/hex DIAGNOSTIC encode
  … explicitly must NOT be carried into S-P2 as a primitive; FNV bench-only, HANDOFF").
- **Disposition.** Recorded as a NON-candidate. It vanishes wholesale with tape activation (the String
  accumulator that holds the hex field is retired). No NEON hex/FNV primitive is proposed. Any S-P3 shortlist
  entry that proposes a hex-encode or FNV kernel re-opens the FNV-production pre-block (`SYNTHESIS.md §0.4`)
  and is REJECTed. Listed here so the 8.98% leaf is not mistaken for a gap.

### Summary table

| Gap | Primitive | Shape | Scalar-ref | Layer | P1 antecedent | New asm? |
|---|---|---|---|---|---|---|
| G1 | `comment_body_mask_64` | `(&[u8;64], open:[u8;2], close:[u8;2], bool) -> (u64,bool)` | ABSENT — sketch §2-G1 | L1 aarch64 + scalar twin | comment-skip arm of find_component_delim/consume_balanced_at (~69% combined) | yes (NEON) |
| G2 | `bracket_depth_mask_64` | `(u64,u64,i32) -> (u64,i32)` | ABSENT — sketch §2-G2 | L1 aarch64 + scalar twin | `consume_balanced_at` 11.05% recursion | yes (NEON) |
| G3 | `scan_components_to_index` | `(&[u8],&[u8;≤8]) -> Vec<u32>` | mirror `scan_structurals_scalar` | per-grammar consumer (not bbnf-simd) | whole ~69% scan; the same-wave consumer | no (composition) |
| G4 | `parse_4_digits` checkasm gate | test only; kernel extant | PRESENT (`digit_mac.rs:15`) | L1 test artefact | NONE current — GATED behind W1/W2 typed re-profile | no |
| G5 | (NON-candidate) FNV/hex | — | — | — | diagnostic only; pre-blocked | no |

## §3 — Grammar-neutrality (each candidate: JSON-only or CSS/Sheets/BBNF-self generalisable)

- **G1 `comment_body_mask_64` — GRAMMAR-NEUTRAL (re-expressed per P2-F template).** A block-comment is not
  CSS-specific: it is a 2-byte-open / 2-byte-close opaque region, the same shape as C/Rust/JS `/* */`, SQL
  comments (different digraph), and any grammar with line/block comments. The primitive IS parameterised
  by the open/close digraph (`(open:[u8;2], close:[u8;2])` — see the §2-G1 signature and scalar sketch, which
  compare against `open[0]/open[1]/close[0]/close[1]` and never a literal `/`/`*`), NOT hard-coded to `/*`/`*/`. JSON has no comments
  (the JSON scan never reaches this state), so JSON is the witness that the suppressor is ADDITIVE and
  alphabet-driven, not the universal path. CSS exercises it. Verdict: grammar-neutral by digraph
  parameterisation; JSON+CSS-witnessed (CSS the only current exerciser, the digraph the only grammar datum).
- **G2 `bracket_depth_mask_64` — GRAMMAR-NEUTRAL.** Nested-bracket balance is the most grammar-general shape
  there is — every nesting grammar (JSON arrays/objects, CSS component blocks, BBNF group/option `()`/`[]`,
  Sheets formula parens) is a depth counter over an open-set / close-set. The primitive takes the open/close
  masks (produced by alphabet-driven `byte_class_from_eq_set_64`), so the bracket bytes are the only
  grammar datum. JSON's structural scan EMITS brackets to the index rather than suppressing them, so JSON is
  the witness that depth-tracking is a reusable mask op (JSON would consume the depth to validate nesting; CSS
  consumes it to suppress). Verdict: grammar-neutral; JSON+CSS-witnessed; the canonical Lock-14 primitive.
- **G3 `scan_components_to_index` — GRAMMAR-NEUTRAL via `select_classifier(alphabet)`.** This is the explicit
  Lock-14 neutrality vehicle (`SYNTHESIS.md §0.1` NEON gate; alphaE-C2): one assembler, the delimiter alphabet
  the only per-grammar datum, producing only a `Vec<u32>` index the tape consumes. Directly isomorphic to
  JSON `scan_structurals` (`json/scan.rs:22`). JSON is the tape-wired witness; CSS is the new rider sharing the
  kernel; this is the `simd_non_json_exercise = css_l4` the contract names (Section 2). Verdict: grammar-neutral
  by construction; the witnessed grammars are JSON + CSS (Sheets/BBNF-self deferred to SK-V18 per the Generality
  clause, `SYNTHESIS.md §0.4`).
- **G4 `parse_4_digits` — GRAMMAR-NEUTRAL but ORPHAN.** A 4-ASCII-digit→u32 decode is neutral (any grammar with
  integer literals). But it has NO benched antecedent on either CSS plane and is gated behind a typed-path
  re-profile (§2-G4). Its neutrality is moot until an antecedent exists. Verdict: neutral-but-orphan; not a
  free-standing candidate; the parse-that deliverable is its checkasm gate, sequenced behind the re-profile.
- **G5 — N/A** (non-candidate).

No candidate is JSON-overfit; none is CSS-overfit (G1/G2 are alphabet/digraph-parameterised, G3 is the
neutrality vehicle itself). P2-F should verify G1/G2's parameterisation is genuinely per-grammar (digraph for
G1, open/close-set for G2) and not silently CSS-pinned.

## §4 — Risks (REDRESS entries any candidate must NOT re-open)

- **REDRESS-89 (CSSC/CTZ next-bit bulk consumer as DEFAULT hot body) — bounded for G2 (bound promoted inline to
  §2-G2 per the V1 CHALLENGE).** G2's optional CTZ "ranges" path uses `bitmap_next_set_bit` (`lib.rs:265`) as a
  CONSUMER of the precomputed depth mask, never as the default per-byte hot body and never on the prefix-XOR
  path. REDRESS-89 rejected the CTZ consumer + B6 canary fold as a DEFAULT path with PMULL still scalar
  (`REDRESS.md:2542-2568`); G2 keeps the scalar running balance as the spec/default-shipped body and admits CTZ
  only where the block has no underflowing closes, gated by checkasm parity. This is the same conclusion as
  `p2c-arch-esoterica.md` §3.2 (CTZ bulk-iterate body forbidden): the two artefacts are reconciled — bulk-default
  CTZ REDRESS-89-blocked, parity-gated consumer-of-precomputed-mask admissible. If S-P3 shortlists G2 with CTZ as
  the unconditional body, CH3 must REVISE it back to the consumer framing.
- **REDRESS-88 (PMULL prefix-XOR as DEFAULT hot body) — not re-opened.** G1's region fill reuses the
  `overflowing_add` carry idiom (`escape_mask_64`, `lib.rs:188`), NOT PMULL. No PMULL text is introduced. The
  string-skip suppressor G3 reuses the EXISTING `prefix_xor_64` (whose backend selection is already settled by
  REDRESS-88/89 — scalar default); G1/G2/G3 add no new prefix-XOR backend.
- **REDRESS 28+33 / 82-84 (Class A tiny-string wiring, StringBlock16 probe, single-quartet unicode classifier,
  object-pair compaction) — not re-opened.** No gap proposes a tiny-string fast path, a StringBlock16 retained
  probe, or a unicode classifier. G1/G2 are region-suppressor masks, not string matchers. `match_tiny_plain_string`
  and `string_block` (extant) are NOT consumed by any gap.
- **REDRESS 50-55 / 60-72 (UTF-8 fusion, retained-parse + sidecar producers + cap-16 digest) — not re-opened.**
  G3 produces a TRANSIENT `Vec<u32>` index (a producer, not a retained sidecar — `SYNTHESIS.md §0.4` "A SIMD mask
  stream is a transient producer, not a retained sidecar"). No retained cursor, no aux density table, no second
  scan. CSS recognition does no UTF-8 decode (treats ≥0x80 as a name byte, `generated.rs:404`) so no UTF-8 fusion
  primitive is proposed.
- **Lock 1 (substrate union) — held.** G3 emits ONLY the structural index; if the offsets are retained, the
  structural projection IS the tape (`LOCKS.md:75`). No `UnionTape`, no second substrate, no parser-owned
  projection, no cross-call classifier-state retention (the carry/depth threads WITHIN a single
  `scan_components_to_index` call, reset per parse — the `SYNTHESIS.md §0.4` "carry stays within a single
  chunk-call; cross-call classifier state remains rejected").
- **AZ-IV eager-value-tree / StructRegistry indirection — not touched.** No gap materialises a value tree or
  introduces a registry; G1/G2/G3 are pre-scan masks feeding the tape index. The udot G4, if ever admitted,
  decodes ON DEMAND via `ValueRef`, not eagerly.
- **FNV / hex / fixture contrivance — explicitly excluded (G5).** No hex/FNV primitive. No per-corpus capacity
  literal (the delimiter alphabet is grammar-derived, the index capacity is `input.len()/8+8` as JSON, not a
  tailwind literal).
- **NEON-gated-behind-tape ordering (P1-E §4.3) — binding sequencing risk.** G1/G2/G3 are the NEON structural
  pre-scan; on the TYPED fact-stream plane the scan is MASKED by the String/alloc floor (~58–64%). The lever
  order is tape activation FIRST (kill `emit_fact_stream`, retire the alloc floor), THEN G1/G2/G3 on the
  surviving scan. A candidate that ships G1/G2 before tape activation has no structural index to pre-scan into
  and measures noise. S-P3 must not invert this (HARDENING-S-P1-V4 §3.4 lever order).

## §5 — Sources (every external citation — comparator source, ISA manual, prior tranche)

- S-P1 profile (locked, commit `0ae1caa52`; master HEAD-of-record `0ae1caa52`, citations re-verified this V3 cycle):
  `restart/skinny/tranches/sk-v17/research/p1/p1e-hot-leaf-attribution.md` (§2.3 recognition attribution
  56.52%+11.05%; §2.4 fact_stream String floor; §2.5 roll-up; §4.4a udot orphan, no digit antecedent);
  `restart/skinny/tranches/sk-v17/research/p1/hardening/HARDENING-S-P1-V4-CONSOLIDATED.md` (§3.3 resolved hot
  leaves + candidate primitives, §3.4 primitive antecedents + lever order).
- SK-V17 contract: `restart/skinny/tranches/sk-v17/SYNTHESIS.md` (§0.1 NEON hot-leaf union gate, §0.3 C4a/C4b
  digit split, §0.4 pre-blocks + Generality clause + transient-producer / Lock-1 carry rules, Section 2
  `simd_non_json_exercise=css_l4`); `research/alpha/alphaE-candidate-shortlist.md` (C2 NEON pre-scan, C4a/C4b
  digit split — P2-E refines C2 with the §1.3 suppression analysis showing two suppressor masks are MISSING,
  which C2 assumed the JSON kernels already cover).
- bbnf-simd source (master HEAD `0ae1caa52`, verified this V3 cycle — `lib.rs` lines `find_ascii_set_member64` :209,
  `prefix_xor_64` :170, `escape_mask_64` :175, `compact_mask` :229, `byte_class_from_eq_set_64` :282,
  `bitmap_next_set_bit` :265, `eob_pad_clamp` :275 all re-confirmed; `dispatch.rs` `select_classifier` :42,
  `PrimitiveKernels` :50, `lo6_table_admissible` :101, `& 0x3f` low-6-bit collision guard :106 re-confirmed):
  `skinny/crates/bbnf-simd/src/lib.rs` (`find_ascii_set_member64` :209, `prefix_xor_64` :170, `escape_mask_64`
  :175, `compact_mask` :229, `prim::byte_class_from_eq_set_64` :282, `bitmap_next_set_bit` :265, `eob_pad_clamp`
  :275, `StructuralAlphabet`/`StructuralIndex` :20/:72);
  `src/dispatch.rs` (`select_classifier` :42, `PrimitiveKernels` :50, `lo6_table_admissible` :101);
  `src/aarch64/classify_tbl4.rs` (`build_lo6_table` :8, `classify_block_from_table` :47);
  `src/aarch64/byte_class_from_eq_set_64.rs` (NEON :33), `src/scalar/byte_class_from_eq_set_64.rs` (scalar spec :26);
  `src/aarch64/digit_mac.rs` (`parse_4_digits` :5, `parse_4_digits_dotprod` udot :27, `dot4_i8` :51);
  `src/aarch64/{string_block.rs,match_tiny_plain_string.rs,bitmap_next_set_bit.rs,bulk_emit_positions_64.rs}`
  (extant, not consumed by any gap); `src/scalar/eob_pad_clamp.rs` :8.
- Two-layer vocabulary: `ext/x86/{x86inc.asm,x86util.asm,bbnf.asm}` (Layer-0 vendored, x86-only);
  `build.rs:1,46` (Layer-0 assembly, early-return on non-x86); `Cargo.toml` (`[build-dependencies] cc, nasm-rs`).
- checkasm discipline (Lock 16): `tests/checkasm_ascii_set_member_find_64.rs:30,159` (`find_ascii_set_member64`
  gate); `tests/checkasm_byte_class_from_eq_set_64.rs`; `tests/checkasm_structural_terminator_64.rs:9,23`
  (reference/candidate pattern); `tests/checkasm_common.rs` (harness). `ls tests/ | grep digit` = EMPTY (G4 gap).
- JSON antecedent path (the model + the generality witness): `skinny/crates/runtime/src/grammars/json/scan.rs`
  (`scan_structurals` :22, `scan_structurals_scalar` :32, NEON `neon::scan` :207, the
  `classify_structural_terminator_block_from_table` call :217-219, `prefix_xor_64` use :239,
  `escape_mask_64` use :237, `resolve_string_masks_64` :164 — JSON's ONE skip-state suppressor); JSON ships
  whitespace + string suppressors only — no comment, no bracket-depth suppressor (the §1.3 gap).
- CSS hot leaf source: `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs`
  (`find_component_delim` :288, membership :295, skip-`match` :298-307; `find_colon_before` :313;
  `consume_balanced_at` :320, recursion :330-332; `consume_comment_at` :342; `consume_string_at` :353;
  name-byte ≥0x80 :404; `emit_fact_stream` :5; `push_ascii_lower_hex` :628).
- Tape sink (the index consumer; Lock 1 single substrate): `skinny/crates/runtime/src/tape/assembler.rs`
  (`TapeBuilder` :42, `push_plain_offset` :71 branchless u32 write); `runtime/src/tape/mod.rs` (`Tape` :94,
  `ValueRef` :175, `PayloadArena` :38, `DocumentView` :227).
- REDRESS pre-blocks: `skinny/REDRESS.md` (REDRESS-88 PMULL default-body :2535-2538; REDRESS-89 CTZ
  consumer + B6 canary :2542-2568; tiny-string :325-332; StringBlock16 :2318-2320).
- Locks: `restart/locks/LOCKS.md` (Lock 1 substrate union :75; Lock 14 grammar-neutrality :349,:386-387;
  Lock 16 SIMD parity).
- External SOTA shape (the "stage-1 then suppress" structural-scan + region-masking model): Langdale & Lemire,
  *Parsing Gigabytes of JSON per Second*, VLDB Journal 28(6), 2019, §3 (structural/whitespace identification)
  + §4 (string-region masking via carry-less prefix). simdjson `src/generic/stage1/` (`json_string_scanner`
  body suppression; `find_structural_bits`) — the precedent that CSS needs TWO more suppressors (comment G1,
  bracket-depth G2) than JSON's two. ARM Architecture Reference Manual (ARM DDI 0487, ASIMD) for the
  `vqtbl4q_u8`/`vceqq_u8`/`vaddv_u8` movemask operations the extant kernels and the proposed bodies use.
