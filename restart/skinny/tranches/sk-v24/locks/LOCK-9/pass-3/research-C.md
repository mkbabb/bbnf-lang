# LOCK-9 · Family 9C — SCANNER-AS-DFA (research-C, pass 3)

The scanner layer dissolves into the regex continuum: quote/number/ws/ident/url-char scanning
become compiled DFA + prefilter products of the bootstrapped grammar, so the hand-written
*character-class* scanners cease to exist. This report makes the mechanism concrete, names the
already-present substrate, and draws the honest boundary between what a DFA can lawfully
replace and what it structurally cannot.

## 1. The mechanism, made concrete

A grammar leaf carrying a regex (`literal`, `char_class`, `escape` in
`parse-that/rust/bootstrap/regex.bbnf:10-37`) lowers on the generator path through the existing
`bbnf-regex` automata tower and terminates in **one byte-class table** that is already the
input type of the shared SIMD kernel:

1. leaf regex text → `Hir` (`parse-that/rust/regex/src/hir/parser/mod.rs:153`)
2. `Hir` → Thompson NFA (`automata/nfa.rs:71 from_hir`)
3. NFA → DFA via subset construction + Hopcroft minimization
   (`automata/dfa.rs:67 compile_with` → `:86 subset_construction` → `:90 hopcroft_minimize`)
4. the DFA emits **`byte_classes: [u8; 256]`** (`automata/dfa.rs:52`), computed by
   `compute_byte_classes` (`sets/equiv.rs`, called at `dfa.rs:80`), plus `num_classes: u16`.

The load-bearing observation for 9C: **that `[u8;256]` byte-equivalence table is bit-for-bit
the same shape as the table consumed by the shared classifier kernel**
`bbnf_simd::prim::byte_class_from_table_64(&[u8;64], &[u8;256]) -> u64`
(`skinny/crates/bbnf-simd/src/lib.rs:256`, NEON body
`aarch64/byte_class_from_table_64.rs`). The DFA already *produces* the representation the SIMD
plane already *consumes*. Scanner-as-DFA is therefore not a new engine — it is a **one-pass
emit that routes the DFA's start-state row and byte-class table into three existing kernel
tiers**, selected by separability:

- **membership / self-loop leaf** (`[set]+`, `[set]*` — the ws, digit, hex, ident, url-char
  runs): the DFA is 1–2 states with 2 byte-classes (member / non-member). It reduces to a
  single-block membership scan. If `|set| ≤ 8` → `byte_class_from_eq_set_64`
  (`lib.rs:283`, memchr-tier). Otherwise → `byte_class_from_table_64` (any `[u8;256]`,
  `lib.rs:256`). This is where "hand-written scanners cease to exist" is literally true.
- **anchored literal-prefix leaf** (`https?://…`, `0x…`, `#…`): the start-state entry set is
  1–3 bytes → the **prefilter tier** (`memchr`/`memchr2`/`memchr3`) skips runs that cannot
  begin a match, then an anchored DFA confirm. This is the PROVE-10 mechanism verbatim
  (`Prefilter::from_dfa` reads the start-state transition row; `Dfa::find` branches once on
  the prefilter; CONFIRMED 8.8–29.5×, 0.99× controls, compile-neutral, 44/44 tests).
- **multi-kind structural leaf** (the JSON `{}[]:,"\` set, CSS delimiter set): NOT a single
  membership test — needs a *distinct kind per byte*. Routes to
  `structural_classify_64(&[u8;64], &KindPartition)` (`lib.rs:301`), the `vceqq`-fan
  (scheme α, `aarch64/structural_classify_64.rs`), grammar-neutral, caller supplies the
  partition.

The emit pass is thus: *derive `byte_classes`/entry-set/kind-partition from the leaf DFA →
select tier by a separability predicate → emit the block loop over `PaddedView` 64-byte
stripes.* No hand scanner survives for the membership and prefix tiers.

## 2. The substrate is already physically wired (not a proposal)

The skinny greenfield resolves the **name+version collision** the directive flags (two
`bbnf-regex 0.1.0`) by *renaming, not deduping*:

- `skinny/crates/parse-that-regex/` (v0.1.0) — **the vendored engine, the DFA source of
  truth** (`Cargo.toml` deps: `bbnf-simd`, `memchr`, `thiserror`; 3022 src LOC incl.
  `unicode/`, `number/eisel_lemire/`, `integration/`). This is the substrate.
- `skinny/crates/bbnf-regex/` (v0.1.0) — repurposed as the **facts/analysis** crate:
  `RegexFacts { nullable, first: FirstSet, byte_classes: Vec<ByteClass>, hir, string }`
  (`src/lib.rs:1`), the codegen-decision surface (first-sets, byte-classes, string facts).
  This is *not* the engine; it is the grammar-tier `Regex(StringId)`-opaque analysis face.
- The legacy top-level `crates/bbnf-regex` (6742 LOC, the original engine) is **already
  absent** from `bbnf-lang/crates/` — it archives at GN-0; the `parse-that/rust/regex` tree
  is its live twin and the vendoring source.

The DFA↔kernel seam is a shipped trait, not a design:
`skinny/crates/parse-that-regex/src/integration/simd_scan_hook.rs:3` —
`trait SimdScannerHook { fn classify_chunk(&self,&[u8;64])->ClassifyResult; fn alphabet(&self)->&'static [u8;64]; }`
with a blanket impl over `bbnf_simd::SimdClassifier`. The regex engine already calls
`bbnf_simd::aarch64::string_block::scan_string_special_block`
(`parse-that-regex/src/lib.rs:611`) and `memchr::memchr2` (`:687`). **The physical
isomorphism the constitution asks for exists at the crate-dependency level today.**

### 2a. The separability selector already exists — and is the crux of F3

`bbnf_simd::dispatch::select_classifier(alphabet)` (`dispatch.rs:42`) picks the backend via
`select_backend` → **`lo6_table_admissible(alphabet)`** (`dispatch.rs:101`): it walks the
alphabet and rejects (falls to scalar) if any two members collide in their low 6 bits
(`slot = byte & 0x3f; if seen[slot] return false`). This is exactly the "partition /
separability selection" the directive asks a scanner-DFA emit pass to perform — it is
**already implemented for the single-table `vqtbl4q` tier**. An emit pass would call the same
predicate on the DFA-derived alphabet.

## 3. F3 answer — "does a naive nibble-only emit misclassify the structural set?" YES, provably.

The tree documents the counterexample in-source. `classify_tbl4.rs:8 build_lo6_table` maps
`byte & 0x3f → byte`; `structural_classify_64.rs:14` records the defect verbatim:
**`;`=0x3B ≡ `{`=0x3B under low-6 mask — "neither a single `vqtbl1q_u8` low-nibble LUT NOR the
`vqtbl4q_u8` lo6-table can assign a DISTINCT kind per byte."** The JSON structural set itself
is low-6-separable (`{[]}:,"\` map to distinct low-6 slots), so the *single-membership*
`vqtbl4q` tier is admissible for it — but the moment a CSS/other grammar's delimiter set
includes any low-6-colliding pair (e.g. `;` with `{`), a naive nibble/low6 emit silently drops
one member. The `classify_tbl4` guard `vandq(vceqq(class,chunk), vcgtq(class,0))`
(`classify_tbl4.rs:32`) rescues only *member-vs-non-member* collisions (a non-member byte whose
slot holds a member fails the equality); it CANNOT rescue *two colliding members*. Therefore:

- a scanner-DFA emit **must** run `lo6_table_admissible` and **fall back** to either
  `byte_class_from_table_64` (full `[u8;256]`, collision-free by construction) or the
  `vceqq`-fan `structural_classify_64` (exact equality, collision-free, caps ~5 kinds).
- The `vceqq`-fan is the safe multi-kind default; the `vqtbl4q` lo6-table is a *fast path*
  gated on separability, not the general emit target.

So a `[u8;256]→SIMD classifier` **can** be emitted matching the hand classifier — the runtime
kernel (`byte_class_from_table_64`) already IS grammar-neutral and takes the DFA's exact
`[u8;256]` — and the naive nibble-only variant **does** misclassify unless the separability
gate routes around it.

## 4. F3 census — classes/states over the tractable ceilings

Denominators: 255 byte-classes (a `[u8]` alphabet cannot exceed 256 classes; DFA
`num_classes: u16`), and a scanner-tractable state ceiling (the `vqtbl1q` serial-chain tier is
a measured negative on the short runs these scanners see, so the tractable state budget is
≈ a handful, not thousands). Analytic census from `compute_byte_classes` semantics (bytes with
identical transition behaviour merge); §10.3 owes the measured replacement:

| Leaf | byte-classes / 255 | DFA states (tractable ≲ 8) | Tier | Coverage |
|---|---|---|---|---|
| ws `[ \t\n\r]+` | 2 (0.8%) | 2 | eq_set (≤8) | membership ✅ |
| digit `[0-9]+` | 2 (0.8%) | 2 | eq_set/prefilter | membership ✅ |
| hex `[0-9a-fA-F]+` | 2 (0.8%) | 2 | table_64 (>8) | membership ✅ |
| ident `[A-Za-z_][A-Za-z0-9_]*` | 3 (1.2%) | 2–3 | table_64 | membership ✅ |
| **url() `[a-zA-Z0-9./-]+`** | **2 (0.8%)** | **2** | table_64 | membership ✅ |
| `https?://[…]+` (prefix leaf) | ~8 (3%) | ~9 | prefilter(`h`)+confirm | prefix ✅ |
| JSON structural `{}[]:,"\` | 9 (3.5%) | 1 (per-byte) | vceqq-fan | structural ✅ |
| **wide-unicode `\p{L}` / `[-￿]`** | **30–60+ (12–24%)** | **hundreds** (UTF-8 range automaton, `regex/src/utf8.rs`) | **NO-GO** | scalar Höhrmann DFA ❌ |

The tractable tiers (membership + prefix + structural) reach **~100% of the JSON/CSS-relevant
leaf surface** — every character-class run and delimiter set the shipped grammars use collapses
to ≤3 classes / ≤3 states. The **wide-unicode leaf is the sole NO-GO**: `\p{L}` over UTF-8
bytes explodes to dozens of byte-classes (length-tagged lead bytes + `0x80–0xBF` continuations)
and hundreds of states — over the tractable state ceiling; the serial `vqtbl1q` multistate
chain is a measured loss on short runs. It stays on the scalar UTF-8 DFA already vendored:
`parse-that-regex/src/unicode/utf8_hoehrmann.rs` (Höhrmann state machine) +
`unicode/utf8_block.rs`. The census weighting the directive hints is confirmed: **weight
membership + structural; the multistate `vqtbl1q` chain earns no leaf.**

## 5. The honest boundary — what a DFA CANNOT dissolve

"Hand-written scanners cease to exist" is TRUE for the membership/character-class/prefix leaves.
It is **FALSE** for the *carry-stateful transduction* kernels, which are not per-byte DFAs over
a byte alphabet:

- **quote-parity prefix-XOR** (`prefix_xor_64`, `lib.rs:169`) — a carry-propagating
  prefix-scan, not a membership test.
- **escape odd-parity** (`escape_mask_64`, `lib.rs:174`; the backslash-run parity) — inter-block
  boolean carry.
- **bracket-depth interior** (`bracket_depth_mask_64`, `lib.rs:336`) — a running i32 balance
  (REDRESS-89: scalar running balance, NOT CTZ).
- **comment-body** (`comment_body_mask_64`, `lib.rs:316`) — `open…close` carry state.

These are the simdjson stage-1 style bitmap transductions; they carry state *across* 64-byte
blocks and are structurally outside a single-alphabet DFA. **9C's lawful scope is the
character-class/membership/prefix leaves; the four carry-stateful kernels remain hand-authored
and belong to LOCK-9's kernel plane (9A/9B), consumed by 9C via the same `byte_class`/mask
vocabulary, never re-derived.** Claiming a DFA dissolves them would be a stub — forbidden.

Scanner-file census (the ~8 files, R3 §2a), classified DFA-lowerable | kernel-tier | no-go:
`ws_comment.rs`(16) → **DFA-lowerable** (ws run) + kernel (`comment_body_mask_64`);
`decode.rs`(17) → kernel (`string_block`/`unescape_uxxxx`); `structural_bitmap.rs`(13) →
kernel (`structural_classify_64`); `quoted_simd.rs`(7) → kernel (escape parity);
`quote_parity.rs`(4) → kernel (prefix-XOR); `number_simd.rs` → **DFA-lowerable** digit run
(scan only; float materialize is SPEC §0/02 NO_GO, stays scalar Eisel–Lemire);
`scanners.rs`(22)/`span_parser/scan.rs`(7) → **DFA-lowerable** membership scans; `state.rs`(2)
→ no-go (PaddedView machinery, not a scanner). **~3 files fully dissolve; ~4 stay as
carry-stateful kernels; 1 is infrastructure.**

## 6. The mask-contract seam (a live inconsistency the DFA emit must respect)

The tree carries **two movemask idioms** and the DFA-lowered classifier must not conflate them
(the refutation law: vshrn narrow, never double-addv):
- **LEAN nibble-`vshrn`**: `match_tiny_plain_string.rs:100` and `structural_classify_64.rs:147`
  use `vshrn_n_u16::<4>` → a 4-bit-per-lane (nibble-strided) 64-bit mask; consumers bit-scan at
  `tz >> 2`. This is the CONFIRMED idiom (SPEC §0/03).
- **DENSE double-`vaddv`**: `byte_class_from_eq_set_64.rs:79-84` still uses
  `vaddv_u8(lo)+vaddv_u8(hi)` → a dense 1-bit-per-lane u16 mask. This is the shape PROVE-09's
  refutation flagged (2271 `addv.8b` vs 509). It is *legitimate here* only because eq_set
  returns a 16-bit dense mask whose sole use is `trailing_zeros` for first-member (`lib.rs:217`)
  — but a DFA emit that fed a `vshrn` producer into a dense consumer, or vice-versa, would
  corrupt index arithmetic. **The emit pass must stamp the mask contract (LEAN vs DENSE) per
  kernel and keep the bit-scan stride matched.** This is the concrete parity trap for the
  differential oracle.

## 7. F4 seam (secondary to 9C, noted for completeness)

The rewind surface exists: `parse-that/rust/parse_that/src/state.rs:38,211 furthest_offset`;
restore logic at `:386-446`. A scanner-DFA is *forward-only within a leaf* (each `find`/`scan`
is bounded by the leaf's DFA, no unbounded backtrack), so a windowed 64-byte stripe feed over
`PaddedView` (trailing-64-NUL guard, `INPUT_PAD_BYTES=64`) is the natural streaming surface for
the scanner tier — but whole-document tree ownership upstream means this does not reduce peak
RSS. The rewind histogram (P50/P99/max of `furthest_offset − offset`) is the measurable artifact
bounding whether a ring buffer suffices. 9C itself introduces no backtrack — its leaves are
total DFAs.

## 8. Prior art (citations)

- **regex-automata (Rust, BurntSushi)** — the canonical byte-class DFA design: `ByteClasses`
  (`[u8;256]` equivalence map) shrinks the transition alphabet; dense vs sparse DFA; the
  memchr/rare-byte **Prefilter** ladder (PROVE-10 lifts this verbatim). Confirms the
  `byte_classes` → shared-table route is SOTA, not novel risk.
- **simdjson (Langdale & Lemire, VLDB 2019)** — stage-1 structural classification by NEON/AVX
  `vceqq`/shuffle fans + prefix-XOR quote parity + odd-backslash parity. The four carry-stateful
  kernels (§5) are exactly its transductions — evidence they are *not* single-DFA products.
- **Hyperscan / Teddy & shufti/truffle (Wang et al.)** — multi-byte literal + small-charset
  SIMD acceleration; the nibble-shuffle "shufti" is precisely the low-nibble LUT whose collision
  hazard §3 documents. Teddy is the multi-byte tier beyond the 1/2/3-byte memchr prefilter
  (PROVE-10 §5 names it future work).
- **RE2 / Thompson NFA→lazy-DFA (Cox)** — subset construction + Hopcroft minimization
  (`dfa.rs:291`) is textbook; lazy determinization is the alternative PROVE-10 deliberately did
  NOT take (correctness-simpler prefilter chosen).
- **Höhrmann UTF-8 DFA (2010) / ICU** — the branchless 9-state UTF-8 validator; vendored at
  `parse-that-regex/src/unicode/utf8_hoehrmann.rs`; the correct home for the wide-unicode NO-GO
  leaf (§4).
- **tree-sitter lexer** — grammar-derived scanner precedent, but interpretive tables; bbnf's
  differentiator is emitting *specialized SIMD* per leaf behind the separability gate, not an
  interpretive automaton.

## 9. Named risks & counterexamples

- **R1 — nibble misclassification.** A `vqtbl4q`/`vqtbl1q` low-bits emit drops a colliding
  structural member. *Counterexample:* `;`≡`{` at low6 0x3B (`structural_classify_64.rs:14`).
  *Guard:* `lo6_table_admissible` gate → `vceqq`-fan or `byte_class_from_table_64` fallback.
- **R2 — mask-contract mismatch.** A DFA emit pairs a `vshrn` producer (nibble stride) with a
  dense-mask consumer (`tz` stride). *Counterexample:* PROVE-09's 2271-vs-509 addv census.
  *Guard:* per-kernel LEAN/DENSE contract tag + differential parity oracle.
- **R3 — wide-unicode state explosion.** Emitting a `\p{L}` DFA into a serial `vqtbl1q` chain
  regresses on short runs (the measured negative). *Guard:* state-count ceiling → route to
  scalar Höhrmann DFA; never SIMD-lower a >~8-state leaf.
- **R4 — false "scanners cease to exist."** Claiming the DFA dissolves the carry-stateful
  quote-parity/escape/bracket-depth/comment kernels. *Counterexample:* prefix-XOR is a scan, not
  a membership test. *Guard:* §5 boundary — 9C scopes to character-class/prefix leaves only.
- **R5 — DFA source ambiguity.** Two `bbnf-regex 0.1.0` crates. *Resolution:*
  `parse-that-regex` is the engine/source-of-truth; `bbnf-regex` (skinny) is the facts face;
  legacy top-level archives at GN-0. Version collision is resolved by *name*, not version bump.

## 10. What a prototype must test to move 9C

1. **Emit-parity oracle.** For each shipped grammar leaf (ws, digit, hex, ident, url-char,
   JSON structural set), derive the DFA `byte_classes`/entry-set/kind-partition, feed the
   existing kernel (`byte_class_from_table_64` / `byte_class_from_eq_set_64` /
   `structural_classify_64` / PROVE-10 `Dfa::find`), and assert **bit-identical scan output vs
   the hand scanner** over black-boxed random inputs + an alignment sweep, with an INJECT_BUG
   rate confirming the oracle bites. Include the `;`≡`{` low6-collision case explicitly.
2. **Separability-gate correctness.** Enumerate the grammar alphabets, run
   `lo6_table_admissible`, and confirm every non-separable set routes off the `vqtbl4q` tier —
   a fuzz that plants a colliding member and asserts NO silent drop.
3. **Census reproduction.** Actually compile the eight leaves through
   `parse-that/rust/regex Dfa::compile_with` and record real `num_classes` + state counts
   against the 255/tractable ceilings (replace §4's analytics with measured numbers); confirm
   `\p{L}` over-ceils and is excluded.
4. **No-regression on the DFA scan path.** Paired cold divan (N≥200, two windows, opposite arm
   order) of the emitted membership scan vs the hand scanner on the JSON/CSS corpora; `otool -tv`
   reduction-insn count ≤ hand baseline; paired median ≥ 0.98×; base-pinned to a stated HEAD.
   PROVE-10's controls (`0.99×`) are the floor.
5. **Mask-contract audit.** `otool` the emitted kernels; assert every mask producer is the
   `vshrn` narrow where its consumer bit-scans at nibble stride, and no double-`vaddv` feeds a
   nibble consumer.
