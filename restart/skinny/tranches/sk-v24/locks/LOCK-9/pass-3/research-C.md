# LOCK-9 · Family 9C — SCANNER-AS-DFA — pass-3 research

The scanner plane dissolves into the regex continuum: quote/number/ws/structural scanning become
compiled DFA/prefilter/classifier products of the bootstrapped `bootstrap/regex.bbnf` grammar, so the
hand-written scanner files cease to *originate* logic — they become emitted kernels selected by a
`RegexClass`→scheme lattice, gated by two hard ceilings and a separability test. This family's thesis
is not "one DFA per byte" — the scalar `find_at` per-byte walk is the negative it must beat. It is:
**a leaf's compiled DFA is analyzed once, and its shape picks a SIMD scan scheme.** Below: the pipeline
that already exists, the emit lattice, the census with over-ceiling denominators, and the named no-go set.

## 1. The grammar→DFA pipeline exists at scanner fidelity — but scalar (F3.Q1)

The pipeline is live end to end in `parse-that/rust`. `bootstrap/regex.bbnf` (73 LOC; leaf rules
`literal`/`class_escape`/`escape`/`quantifier` + recursive `group`/`atom`/`quantified`/`concat`/
`alternation`) is compiled by parse-that's own bootstrapped generated parser (`scripts/bootstrap-regex.sh`
→ `parse_that::regex::generated`, re-exported by the `regex-bootstrap` crate) — so the regex grammar is
self-hosted and the byte-fixpoint (regen∘regen = regen) is the bootstrap's job, banked already for the
grammar; PT-2.W3 gate 2 re-asserts on-disk idempotence.

Runtime path: `Dfa::compile(pattern)` (`rust/regex/src/automata/dfa.rs:62`) → `parse_with` → `Nfa::from_hir`
(`nfa.rs:71`) → `Dfa::from_nfa` (`dfa.rs:77`): byte-equivalence classes (`sets/equiv.rs::compute_byte_classes`)
→ subset construction → Hopcroft minimize (`dfa.rs:90`) → flat table. **The DFA already drives leaf scans**
at scanner fidelity today: `leaf.rs:74` (`dfa.find_at(bytes, 0)?`), `scanners.rs:75`, `span_parser/
constructors.rs:29`, `span_parser/leaves.rs:46` (`Dfa::compile` + `find_at`). PROVE-10 added the memchr
prefilter entry-tier (`Dfa::find`, 9.6×–29× on prefilterable leaves, 0.99× on controls). What is *missing*
is SIMD execution of the class-loop: `find_at` is a scalar `state = flat[state*num_classes + byte_classes[b]]`
inner loop (`dfa.rs:116`). SCANNER-AS-DFA is precisely the pass that replaces that scalar loop with an
emitted membership/structural kernel when the DFA shape permits — it does not invent the pipeline, it
**lowers its already-compiled artifact** to SIMD.

The emit home also exists: PT-2.W3 (`docs/tranches/sk-v24/PT-2.md:271`) names the greenfield codegen home
`crates/codegen/src/emit/regex/` with `scanner_plan.rs`, `dfa/`, `simd.rs`, `negated_class.rs` — the
scanner-plan emitter is already the codex's decomposition target (GN-1-ported from retired
`crates/core/src/generate/regex/emit/`). So the mechanism has a declared file seat on both sides.

**Name+version collision (F3.Q1).** Two crates are `bbnf-regex 0.1.0`: top-level `crates/bbnf-regex`
(legacy) and parse-that `rust/regex` (R3 §3). Neither has a crates.io version — both are path-patched
(R3 §1, `.cargo/config.toml`). XL-C settles authority: parse-that `rust/regex` is the one-regex-home and
the authoritative DFA source-of-truth; `crates/bbnf-regex` archives at GN-0. Resolution is *physical, not
registry*: archive the legacy crate, vendor parse-that-regex into skinny as the substrate, and let one
patch-table entry name one source — the collision never reaches cargo's resolver because only one live
path is patched. (Do NOT co-resolve two `bbnf-regex 0.1.0` in one graph; that is an E0464-class
duplicate-crate error, avoided only by keeping one live.)

## 2. The emit lattice: [u8;256] classifier → SIMD scheme (F3.Q3)

The `[u8;256]` a SCANNER-AS-DFA emit must lower is exactly `Dfa.byte_classes` (`dfa.rs:52`, 256 entries)
for a single-loop leaf, or the membership set of a char-class star. Three exact SIMD schemes already exist
in `skinny/crates/bbnf-simd/src/aarch64/`, with a decision already adjudicated:

1. **bit-per-member nibble shufti** — `structural_bitmap.rs::classify_16`/`classify_stripe`,
   `ws_comment.rs::classify_ws_slash_16`. Two 16-entry LUTs `lo[b&0xF]`, `hi[b>>4]`, ANDed, non-zero
   ⇒ member. `build_ws_slash_{lo,hi}` is **positional** (`lut[nibble] |= 1<<member_index`), so ≤8 members
   map to distinct bits and the AND is *exact set membership*, collision-free. One `vqtbl1q`+AND+non-zero
   per 16 bytes. Membership tier.
2. **vqtbl4q lo6-table** — `classify_tbl4.rs::classify_chunk_from_table` (64-entry `[u8;64]` indexed by
   `byte & 0x3f`, `vqtbl4q_u8`). Exact **iff** the low 6 bits uniquely identify each structural byte;
   fails on lo6 collisions.
3. **vceqq fan** — `structural_classify_64.rs::classify_stripe` (scheme α). `set_len`-many `vceqq_u8`
   broadcast-compares OR-reduced per kind bucket; exact for *any* set. Chosen for the 11-byte CSS
   structural set `' " / ( ) [ ] { } ; :` precisely because it has nibble collisions (`; [ {` ≡ lo 0xB;
   `] }` ≡ lo 0xD) **and** a lo-6 collision (`;`=0x3B ≡ `{`=0x3B), so schemes 1 and 2 cannot assign a
   distinct kind (documented `structural_classify_64.rs:11-21` — the "§8 reject the table-classifier-unify").

**Selection pass = separability test over the emitted set.** (a) ≤8 members ⇒ positional bit-per-member
shufti (scheme 1). (b) 9–16 members with lo6-distinct ⇒ vqtbl4 lo6 (scheme 2). (c) else ⇒ vceqq fan
(scheme 3); a >16-member widening upgrades to the simdjson lo×hi two-LUT AND-fold (the unbuilt upgrade
path at `structural_classify_64.rs:19`). This lattice is emittable from the DFA's per-class byte-set
signatures — `compute_byte_classes` (`equiv.rs:36`) produces, per byte, the 64-bit signature of which
ByteSets contain it; the emit reads those signatures to pick the scheme.

**Naive nibble-only emit DOES misclassify the structural set (F3.Q3, yes).** A *boolean* (non-positional)
nibble LUT — `lo[b&0xF]=1 && hi[b>>4]=1` — false-positives on the cross-product. JSON structural
`{`(7B) `}`(7D) `[`(5B) `]`(5D) `:`(3A) `,`(2C) `"`(22): hi-nibbles {7,5,3,2} × lo-nibbles {B,D,A,C,2}
admit e.g. `r`=0x72 (hi 7 ∈ set via `{}`, lo 2 ∈ set via `,"`) — a false member. The positional
bit-per-member trick fixes ≤8 (the AND of *distinct* bits is 0 for `r`); the CSS 11-byte set exceeds even
that and needs the vceqq fan. The emit must NOT default to boolean-nibble; it defaults to
positional-shufti-or-fan, exactly as the hand kernels already do.

**Refutation carry (PROVE-09 law).** All three bbnf-simd bodies pack with the double-`vaddv` movemask
(`byte_class_from_eq_set_64.rs:79`, `structural_classify_64.rs:147` — multiply-by-bit-weight, `vaddv_u8`
low/high halves). PROVE-09 refutes this idiom: the emitted membership kernel must pack via the narrow
`vshrn #4` producer (PROVE-10 §2c disassembly shows exactly `shrn.8b v0,v0,#4; fmov x8,d0` live in the
memchr NEON loop). SCANNER-AS-DFA's emitted classifier inherits the LEAN nibble-`vshrn` producer, never
the double-addv of the pre-refutation bbnf-simd bodies — a correctness-of-idiom obligation, measured
no-regression per the shim law.

## 3. Census: classes/states over the ceilings (F3.Q2)

Two hard ceilings, both cliffs:
- **255-class ceiling** — `compute_byte_classes` returns `next_class: u8` with `saturating_add`
  (`equiv.rs:57,88`). A leaf needing >255 distinct byte-equivalence classes SATURATES: distinct
  signatures silently collapse to class 255, so the DFA **misclassifies without erroring**. Denom = 255.
- **4096-state ceiling** — `DfaOptions.state_limit = 4096` (`dfa.rs:38`); subset construction returns
  `None` past it (`dfa.rs:86`), so the caller must have a fallback. Denom = 4096.

Census of the `bootstrap/regex.bbnf` leaves + two probed rich leaves (byte-mode, as classify uses
`ParseOptions::byte_mode()` at `classify.rs:158`, which sidesteps UTF-8 class explosion):

| Leaf (source) | byte classes / 255 | DFA states / 4096 | tier |
|---|---|---|---|
| `[0-9a-fA-F]+` hex (`class_escape`, HexDigits) | 2 (hex, dead) | 1 loop + dead | membership |
| `[0-9a-fA-F]{4}` (`\uXXXX`) | 2 | 5 (bounded repeat) | membership (bounded) |
| `[^\n]` (`literal`/`escape`) | 2 | 1 | membership |
| `[dDwWsS]`, `[simux]+`, `[A-Za-z_]+`, `\d+` | 2–3 | 1 loop | membership |
| CSS `url\([^)]*\)` (rich structural leaf) | ~6 (u,r,l,(,),other) | ~6 serial | structural |
| wide-unicode `\p{L}` (unicode mode) | **>255 → SATURATES** | can exceed 4096 → `None` | **NO-GO** |

Tractable-tier coverage: **~100% of `bootstrap/regex.bbnf`'s own leaves** and of the JSON/CSS scanner
leaves reach membership+structural at <3% of the class ceiling (2–6 classes) and <1% of the state ceiling
(1–6 states). The **sole over-ceiling case is the genuine `\p{...}` Unicode-property leaf** in unicode
UTF-8 mode: hundreds of codepoint-range byte classes, saturating the u8 class map, possibly blowing 4096
states. The `\u{...}` bounded-hex leaf is NOT over-ceiling (hex membership). Byte-mode classification
already routes around `\p{}` (no unicode expansion) — but the emit must *detect* the unicode-property leaf
and route it to the interval/codepoint automaton (general `find_at`), never to a saturated byte-class DFA.
This saturation-without-error is the family's single sharpest correctness risk.

**On the serial `vqtbl1q` chain.** The directive weights membership+structural over a small-multistate
serial `vqtbl1q` chain (a measured negative on the short runs these scanners see: ws runs are 1 byte 63%
of the time, ≤4 bytes 84–100%, per `ws_comment.rs:3-8`). SCANNER-AS-DFA must NOT emit a per-state
serial-`vqtbl` DFA-stepper for multi-state leaves. Multi-state leaves take: (i) PROVE-10 entry-byte memchr
prefilter + scalar `find_at` confirm (best for rare-entry structural like `url(`, `0x`, `#hex`); (ii) the
vceqq structural classifier when the leaf is a membership-of-a-kind-set with a separate delimiter (the
JSON/CSS structural bitmap); (iii) scalar `find_at` when neither wins. The membership tier (single-loop
char-class star) is where the SIMD membership scan is an unconditional win.

## 4. Scanner-file mapping census: 1:1-kernel | vector-tier | no-go (F3, F1-owed)

The 8 portable_simd scanner files (R3 §2a) trichotomize by whether their logic is byte-class-regular:

- **1:1-kernel (emit target IS the file).** `structural_bitmap.rs` — already a *parameterized* nibble-LUT
  membership kernel (`classify_stripe(lo_lut, hi_lut)`); it is literally the emission substrate,
  parameterized by a byte set. `ws_comment.rs` tier-1/tier-2 — WhitespaceWithBlockComment membership (the
  `\s` half) is single-loop membership, emittable from the DFA ws-class set (bit-per-member, set
  {' ','\t','\n','\r','\f','/'} ≤8, separable). The block-comment `/*…*/` tail is a 2-state sub-automaton
  in the cold path (`scan_ws_bitmap_cold`) — structural, not membership.
- **vector-tier (membership scan, value work stays scalar).** The digit *run* in `number*.rs` is a
  membership scan (advance past `[0-9]`), DFA-emittable. But **SPEC §0's no-digit-scan-SIMD refutation
  binds**: the digit-*value* accumulation (`number_simd.rs` SSE/NEON `SimdDigitResult`) is REFUTED and
  must be purged (PT-2.W2). SCANNER-AS-DFA emits the membership *boundary* scan, never SIMD value
  accumulation — the family must state this boundary explicitly or it re-imports the refuted lever.
- **no-go (beyond byte-class-regular at SIMD width).** `quote_parity.rs`/`quoted_simd.rs` — the in-string
  bitmap is a **carry-dependent prefix-XOR over backslash parity** (`prefix_xor` via clmul / `vmull_p64`,
  `compute_in_string_bitmap`). Escaped-quote parity is not regular byte-class membership; it is a
  parallel-prefix (carryless-multiply) computation, so NOT expressible as a DFA membership scan and stays
  a hand kernel. `decode.rs` — JSON string decode-to-arena has escape-transition states (`\uXXXX`, `\n`)
  writing an output arena; multi-state + output-producing, not a boundary scan. `state.rs`/`scanners.rs` —
  PaddedView machinery + shared inline scanners, infrastructure not leaves.

Split: **2 files are 1:1-kernel** (the classifier substrate), **~2 vector-tier** (membership boundary
under the digit-refutation), **~4 no-go** (prefix-XOR parity, decode transitions, infrastructure). So
SCANNER-AS-DFA legitimately absorbs the membership+structural half of the scanner plane; the quote-parity
and decode kernels survive as hand-written primitives (constitution: combinators/primitives are lawful
hand-written surface) — SCANNER-AS-DFA is a *partial*, honestly bounded dissolution, not a total one.

## 5. The classification bridge already exists

The DFA-shape→scheme selection does not start from zero: `rust/regex/src/classify.rs` (1148 LOC) walks HIR
into a `RegexClass` taxonomy that IS the emit-plan input — `HexDigits`,
`CharClassQuantified(ClassRangeInfo{chars: CharSet128, negated, min, max})`, `WhitespaceWithBlockComment`,
`Identifier{allows_leading_dash, allows_double_dash_prefix}`, `QuotedString{quote_char, allows_escapes,
allows_u_escapes}`, `PrefixThenClass{prefix, tail}`, `AccelDriven(u8)`, `Numeric{…}`, `Unknown`
(`classify.rs:55-121`). Each variant maps 1:1 to a §4 tier: `CharClassQuantified`/`HexDigits`→membership
shufti; `PrefixThenClass`/`AccelDriven`→memchr prefilter (PROVE-10); `WhitespaceWithBlockComment`→
membership+cold structural; `QuotedString`→quote-parity no-go; `Numeric`→scalar under the digit
refutation; `Unknown`/unicode-property→general `find_at`. `ClassRangeInfo.chars: CharSet128` (128-bit
ASCII membership) is the exact input to the §2 separability test. **This is the family's strongest asset:
the analysis half is built and stable; only the SIMD-emit half (RegexClass → bbnf-simd kernel selection,
vshrn-packed) is the new work.**

## 6. Prior art

- **regex-automata** (Gallant): byte-equivalence classes + `[u8;256]` class map (`equiv.rs` mirrors its
  `ByteClasses`), the memchr/rare-byte prefilter ladder (PROVE-10's basis), Teddy/shufti multi-byte
  prefilter (the unbuilt upgrade path). Its `dense::DFA` scalar step is the exact negative SCANNER-AS-DFA
  beats via membership emission.
- **simdjson** (Langdale/Lemire, "Parsing gigabytes of JSON per second", VLDBJ 2019): structural-bitmap +
  quote-parity prefix-XOR — the *reason* `quote_parity.rs` is no-go, and the source of the lo×hi two-LUT
  AND-fold upgrade.
- **Muła/Langdale shufti/teddy** — the nibble-LUT membership classifier; the positional-bit trick that
  makes ≤8-member membership exact.
- **tree-sitter** — LR + external scanners: precedent that not all scanning is regular (the quote/decode
  no-go set mirrors its external-scanner escape hatch).
- **Thompson/Hopcroft** — the NFA→DFA→minimize spine already in `automata/`.

## 7. What a prototype must test to move this family

1. **Membership-emit parity + speed.** Emit the shufti membership kernel for `[0-9a-fA-F]+` and the ws set
   from their DFA `byte_classes`, vshrn-packed; differential bit-parity vs scalar `find_at` over black-box
   inputs + alignment sweep + INJECT_BUG; divan cold paired vs the scalar DFA loop on JSON/CSS corpora.
   Gate: ≥0.98× otool reduction-insn vs hand `structural_bitmap`, measured win vs scalar `find_at`, base-pinned.
2. **Naive-nibble misclassification proof.** Emit both boolean-nibble and positional-shufti for the JSON
   structural set; assert the boolean form false-positives `r`=0x72 and the positional form is exact — the
   counterexample that justifies the selection lattice.
3. **Ceiling census, built.** Run `Dfa::compile` on all `bootstrap/regex.bbnf` leaves + `url(...)` +
   `\p{L}`; report `class_count()`/255 and `state_count()`/4096; confirm `\p{L}` saturates the u8 class
   map (or returns `None` at 4096) — the over-ceiling denominator, measured not asserted.
4. **Serial-vqtbl negative, reproduced.** Emit a per-state serial-`vqtbl1q` DFA stepper for a 6-state
   `url(` leaf and show it loses to memchr-prefilter+scalar-confirm on the short-run distribution —
   confirming the weighting toward membership+structural over multistate vqtbl chains.
5. **RegexClass→scheme table.** Wire `classify_regex_from_hir` → scanner-plan scheme for the 8 scanner
   leaves; assert the §4 trichotomy (which route to kernel, which to prefilter, which stay scalar/hand).
