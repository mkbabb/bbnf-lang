# LOCK-9 · PASS-3 · FAMILY 9C — SCANNER-AS-DFA

Family charter (9C): the hand-written scanner layer dissolves into the regex continuum —
quote, number, and ws scanning become compiled DFA/prefilter products of the bootstrapped
grammar; hand scanners cease to exist. This report develops that family alone to its
strongest form, answering the F3 census and the F3/F4 seam questions.

---

## 1. THE MECHANISM MADE CONCRETE — the DFA→SIMD-classifier substrate already exists, split

The SCANNER-AS-DFA transposition is not a green-field invention: its two halves are already
built and shipping in the two repos, wanting only a seam that joins them. The mechanism is a
three-tier lowering keyed off the DFA's own byte-equivalence partition.

**Producer half (bbnf-lang, the generator).** The DFA already emits its partition as codegen.
`crates/core/src/generate/regex/emit/dfa/mod.rs:128` writes `static __BYTE_CLASSES: [u8; 256]
= [...]` verbatim from the compiled DFA. The dispatcher emitter
`crates/core/src/generate/regex/byte_class.rs:106 emit_byte_class_lut` builds a first-byte
prefilter LUT; `regex_scan_adapter.rs:598` gates it (`is_dispatchable(&pattern_first_bytes)
&& len >= 4`, `MAX_DISPATCHED = 32`, `first_bytes: Option<CharSet128>` at `byte_class.rs:50`)
and reads `dfa.byte_classes` at `:293`,`:465`. So the generator ALREADY produces the exact
`[u8;256]` table F3 asks whether a classifier can be emitted from.

**Consumer half (bbnf-simd, the runtime kernels).** The runtime already has the classifiers
that consume that table:
- `aarch64/byte_class_from_table_64.rs byte_class_from_table_64_neon(src:&[u8;64],
  table:&[u8;256]) -> u64` — the general 256-entry TBL classifier; its input signature IS the
  DFA `byte_classes`.
- `aarch64/byte_class_from_eq_set_64.rs byte_class_from_eq_set_64_neon` — membership over a
  set `S` of ≤8 byte values → 64-bit mask (shufti-shape, four 16-byte stripes packed via the
  project movemask convention, `aarch64/movemask.rs`).
- `aarch64/classify_tbl4.rs build_lo6_table(alphabet:&[u8]) -> [u8;64]` + `load_lo6_table`
  (`vld1q_u8_x4`) + `classify_block_from_table` — a lo-6-bit (`byte & 0x3f`) `vqtbl` classifier
  for alphabets separable on the low six bits; wired at `dispatch.rs:24-26`.

**The join (the family's actual deliverable).** The lowering is: compile the grammar leaf to a
DFA (`Nfa::from_hir` → `Dfa::compile`), read its `byte_classes: [u8;256]` (`dfa.rs:52`) and
its non-dead entry-byte set (PROVE-10's `Prefilter::from_dfa`), then SELECT the runtime kernel
by the partition's shape — the "partition/separability selection" F3 names:
1. entry-set ≤3 bytes → PROVE-10 `Dfa::find` memchr1/2/3 prefilter (the skip tier; 8.8–29×).
2. STOP/membership alphabet ≤8 bytes → `byte_class_from_eq_set_64` (shufti membership).
3. low-6-bit-separable alphabet → `classify_tbl4` lo6 `vqtbl`.
4. general (≤255 classes) → `byte_class_from_table_64` over the emitted `__BYTE_CLASSES`.
The selection input is `compute_byte_classes` (`sets/equiv.rs:17`, returns `([u8;256], u16
num_classes`) — the byte-equivalence fixpoint that already runs inside `Dfa::compile`. No new
analysis is invented; the DFA's own partition IS the classifier plan.

**Live seam proof.** `parse_that/src/scanners.rs:55 pub fn cached_dfa(pattern:&str) ->
Arc<Dfa>` shows the scanner layer already caches a DFA per pattern — the hand nibble scanners
(`structural_bitmap.rs`, `ws_comment.rs`) and the DFA cache coexist at HEAD; 9C replaces the
former's call sites with the latter's `find`/classify, deleting `build_nibble_luts` &co.

---

## 2. THE CENSUS (F3) — classes/states over the 255 / 4096 ceilings

**Ceilings, exact.** `DfaOptions.state_limit = 4096` (`dfa.rs:36`). `byte_classes: [u8;256]`
(`dfa.rs:52`) — the class ID is `u8`, so the usable ceiling is **255 classes** (a 256th class
overflows the ID type; `compute_byte_classes` returns `u16 num_classes` but stores `u8`). The
flat table is `flat_transitions[state*num_classes + class]` (`dfa.rs:50`) — so cost is
`states × classes`, both bounded.

**Rich-CSS `url()` leaf** (`grammar/css/l4/func-body.bbnf:55`, `values.bbnf:69`):
`urlFunction = "url" , "(" >> (string | /[^)"'\s]+/) << ")"`. The scanning core is the wide
NEGATED class `/[^)"'\s]+/`. `compute_byte_classes` over this leaf collapses the 256-byte
alphabet to **~2 classes** — {member bytes} vs the ~5-byte STOP set {`)`,`"`,`'`,` `,`\t`…} —
and the automaton is **~3 states** (enter → self-loop-on-member → accept-on-STOP). Coverage:
**2/255 classes, 3/4096 states → deeply tractable, TIER-2 (eq_set_64 membership over the
≤8-byte STOP set), the ideal 9C case.** A negated class is the paradigm scanner leaf: broad
alphabet, tiny partition, tiny state count.

**Wide-unicode leaf** (from `bootstrap/regex.bbnf` — `escape = "\\" >> /[pP]/ >> "{" >>
/[A-Za-z_]+/ << "}"` at `:32`, and `\u{...}` at `:33`): the automata layer builds over BYTES.
`regex/src/utf8.rs` (`Utf8Sequences`, `Utf8Sequence{ranges:[Utf8Range;4]}` at `:20`) splits
each codepoint range into 1–4 byte-range chains. A full `\p{L}`-scale or `[\u{0}-\u{10FFFF}]`
class fans into MANY byte-level branch states — each UTF-8 continuation-byte range becomes its
own equivalence class and its own linear chain. This is the **OVER-CEILING case**: byte
classes can exceed 255 and states can blow past 4096; `Dfa::compile` returns
`Err(state_limit)`. Coverage at scanner fidelity: **0% — falls back to scalar/`char`-decode.**
This is the honest boundary of the family: membership + structural tiers are fully DFA-able;
wide-unicode is NOT, and 9C must not pretend it is.

**The tractability verdict (weighting membership + structural per F3):** the leaves the
scanners actually see — JSON structural `{}[]:,"`; quote `"`+`\`; ws `[ \t\n\r\f]`; CSS
url/ident STOP sets; number `[0-9]` (which is a REFUTED SIMD site, SPEC §0/02 — DFA emits the
*membership/skip*, never in-vector digit accumulation) — every one lands in TIER-1/2/3 at
≤~4 classes and ≤~5 states. **The F3 warning is binding: a small-multistate serial `vqtbl1q`
chain is a measured negative on the short runs these scanners see.** Therefore the DFA's value
to the scanner is its PARTITION + ENTRY-SET (a single classify/prefilter pass), NEVER a
per-byte DFA-walk. 9C lowers the DFA to a stateless classifier, not to an interpreted state
machine.

---

## 3. THE NAIVE-NIBBLE MISCLASSIFICATION COUNTEREXAMPLE (F3, confirmed by construction)

F3: "does a naive nibble-only emit misclassify the structural set?" **Yes — provably.** The
hand classifier is a 4-way nibble scheme: `structural_bitmap.rs:37 classify_16` computes
`lo_r = lo.swizzle_dyn(lo_nibble)`, `hi_r = hi.swizzle_dyn(hi_nibble)`, `matched = lo_r & hi_r`
— shufti's TWO-table AND. `scanners.rs:238` caps `build_nibble_luts` at **8 targets** for a
reason. A naive LO-nibble-ONLY emit collapses any two bytes sharing a low nibble: the JSON
structural set has `{`=0x7B and `[`=0x5B (both low-nibble 0xB) and `}`=0x7D and `]`=0x5D (both
0xD). A lo-only classifier maps `{`↔`[` and `}`↔`]` into one class — it CANNOT distinguish
brace from bracket, corrupting depth tracking. The emit is only correct with BOTH nibble
tables (shufti's `lo_r & hi_r`) OR the full 256-entry `byte_class_from_table_64`. The DFA
`byte_classes[256]` path is IMMUNE by construction (full table); the ≤8 `eq_set_64` path is
immune (exact membership); only a hand-rolled single-nibble shortcut misclassifies. **9C's
emit-correctness gate: differential parity of the emitted classifier's 64-bit mask vs the hand
classifier over the structural set, with an INJECT_BUG that flips to lo-only and MUST fail.**

---

## 4. THE PIPELINE & THE NAME-COLLISION (F3)

**Byte-fixpoint-gated grammar→DFA pipeline — exists, partial, LOCK-5-owned.** The chain is
live: `.bbnf` regex leaf → HIR (grammar tier keeps `Regex(StringId)` opaque per XL-C; engine
tier keeps rich `Hir`, `hir/mod.rs:216`) → `Nfa::from_hir` (`automata/nfa.rs:71`, Thompson) →
`Dfa::compile_with` (`dfa.rs:67`): `compute_byte_classes` (`equiv.rs`, the BYTE-equivalence
fixpoint — two bytes share a class iff identical membership across all NFA ByteSets) →
`class_representatives` → `subset_construction` (`dfa.rs:218`) → `hopcroft_minimize`
(`dfa.rs:291`, partition-refinement to a FIXPOINT — this is the "byte-fixpoint gate"). Two
fixpoints compose: byte-class refinement (alphabet) and Hopcroft (states). Scanner-fidelity
gate = `state_limit=4096`; over-ceiling leaves error out (§2's wide-unicode). Buildable now;
the missing piece is only the emit-select-and-consume seam of §1.

**Name+version collision (`bbnf-regex 0.1.0` twice).** Two crates carry it: (a) the
parse-that-vendored `rust/regex/` (6,742 LOC, the engine of record) and (b) the legacy
top-level `crates/bbnf-regex` in bbnf-lang. **Authoritative DFA source-of-truth = the
parse-that-vendored `bbnf-regex` (`rust/regex/src/automata/dfa.rs`)** per XL-C's one-regex-home
and SPEC §1 ("parse-that = the one regex home"). Resolution: GN-0 ARCHIVES the top-level
`crates/bbnf-regex`; `.cargo/config.toml` `[patch]` `bbnf-regex -> regex` (in-tree) makes the
vendored copy win at resolution (R3 §1). One name, one home, collision dissolved by archival —
no version bump needed.

---

## 5. THE STREAMING/WINDOW SEAM (F4, as it touches 9C)

9C is stateless-classify, so it is streaming-NEUTRAL by construction: a DFA classifier over a
64-byte block needs no cross-block state (the DFA-walk would; the classifier does not). The
`PaddedView` invariant (`state.rs:57 INPUT_PAD_BYTES=64`, trailing NUL) lets every stripe load
skip a tail guard — the emitted classifier inherits it free. Rewind: the classify pass is
forward-only; backtrack lives in the COMBINATOR layer (LOCK-4), not the scanner. So the F4
rewind-histogram question is orthogonal to 9C — the DFA classifier is invoked at a fixed
offset and returns a mask; it never rewinds. Whole-document windows do not reduce peak tree
RSS (SPEC/F4 law); 9C makes no streaming claim.

---

## 6. PRIOR ART (citations)

- **regex-automata** (BurntSushi): dense/sparse DFA with byte-equivalence classes (the exact
  `[u8;256]` reduction, `equiv.rs` mirrors it), memchr prefilter ladder (PROVE-10's tier),
  Aho-Corasick, Teddy multi-substring. The classical pipeline 5C measures against.
- **simdjson / asmjson** (Langdale & Lemire, "Parsing Gigabytes of JSON per Second"): the
  `classify_chunk` four-16-byte-stripe → 64-bit structural mask that `byte_class_from_eq_set_64`
  is a NEON translation of (cited in-tree at `scalar/byte_class_from_eq_set_64.rs:12`).
- **Hyperscan shufti/truffle** (Intel): the two-nibble-table membership classifier — the
  correctness floor §3's counterexample defends (lo AND hi, never lo alone).
- **Validark, "Adventures with simdjson"** (2021): NEON nibble decode via `vqtbl` (cited at
  `aarch64/unescape_uxxxx.rs:8`), the lo6-table lineage of `classify_tbl4`.
- **Thompson NFA + Hopcroft minimization**: `nfa.rs` / `dfa.rs:291` — Hopcroft as
  partition-refinement-to-fixpoint (5C's "minimization as rewrite-to-normal-form").
- **tree-sitter**: compiles scanners from grammar but emits an interpreted table-walk, NOT a
  SIMD classifier — the anti-pattern §2's F3-warning forbids (per-byte state walk).

---

## 7. NAMED RISKS & COUNTEREXAMPLES

1. **Wide-unicode over-ceiling** (§2): utf8 byte-splitting blows 4096 states / 255 classes;
   9C MUST fall back to scalar `char`-decode for `\p{}`/wide `\u{}`. Not a defect — a named
   boundary. Counterexample: `[\u{0}-\u{10FFFF}]` → `Err(state_limit)`.
2. **Serial multi-state `vqtbl1q` chain is a measured negative on short scanner runs** (F3):
   the DFA must lower to a STATELESS classifier (partition + entry-set), never an
   interpreted per-byte walk. Counterexample: a 5-state DFA walked per byte over a 40-byte
   url() loses to the `eq_set_64` membership one-shot.
3. **Naive nibble-only misclassifies the structural set** (§3): `{`↔`[` collision on lo-nibble
   0xB. Guard: two-table shufti or full 256-table; INJECT_BUG parity test.
4. **The PROVE-09 refutation is LAW**: any emitted classifier's mask-narrow MUST use the
   `vshrn` nibble-stride idiom (SPEC §0/03 CONFIRMED), NEVER the double-`vaddv` movemask
   (2271 vs 509 `addv.8b`, VERIFY attack #2). `eq_set_64` already uses the project
   `movemask_u8x16` convention — the emit inherits it; it must not hand-roll a reduction tree.
   Stable NEON on `core::arch::aarch64`, measured no-regression, is the gate.
5. **Digit-scan is REFUTED** (SPEC §0/02): the number scanner's DFA emits membership/skip
   (`[0-9]+` boundary), NEVER in-vector Mula/Lemire accumulation (CPI 0.13→0.30 inversion).
6. **Generated-size budget**: a classifier per grammar leaf must stay within the per-tranche
   line budget; the `__BYTE_CLASSES[256]` + a kernel call is ~+3 lines/leaf — cheap, but the
   dispatcher (`MAX_DISPATCHED=32`, `is_dispatchable ≥4 patterns`) must gate emission so small
   grammars don't pay the cold dispatch tax (`regex_scan_adapter.rs:598`,`:664`).

---

## 8. WHAT A PROTOTYPE MUST TEST (to move 9C)

1. **Emit-and-consume**: from one grammar leaf's DFA `byte_classes`, emit a
   `byte_class_from_table_64` (general) OR `byte_class_from_eq_set_64` (≤8) call, replacing the
   hand `classify_stripe`/`build_nibble_luts` at a real scan site (JSON structural, CSS url
   STOP set, ws). Consumer in-wave: the JSON/CSS parser the ~400 tests drive.
2. **Differential parity** (§3): emitted classifier's 64-bit mask == hand classifier's over
   the structural set + url STOP set, 10k random 16-/64-byte vectors, alignment sweep,
   black_box inputs. INJECT_BUG: flip to lo-nibble-only → MUST fail on `{`/`[`.
3. **Census table**: for each of the ~8 scanner leaves, report `(num_byte_classes,
   num_dfa_states)` vs `(255, 4096)`; classify TIER-1(prefilter) | TIER-2(eq_set) |
   TIER-3(lo6) | over-ceiling. Denominators explicit per F3.
4. **No-regression, base-pinned** (per the PROVE-09 law): `otool -tv` reduction-insn count of
   the emitted classifier ≤ hand baseline (guard the double-`vaddv`); paired cold divan ≥0.98×
   on ws/quote/structural sites, two windows opposite arm order, N≥200, HEAD named. Confirm NO
   serial multi-state `vqtbl1q` chain in the disasm (single classify pass only).
5. **Over-ceiling fallback**: feed a wide-unicode leaf; assert `Dfa::compile` returns
   `Err(state_limit)` and the emit selects the scalar path (no crash, no silent misclassify).

**The family's strongest form**: the DFA is the scanner's PLANNER, not its RUNTIME. It
contributes exactly two artifacts to each scan site — the byte-equivalence PARTITION
(`[u8;256]` → `byte_class_from_table_64`/`eq_set_64`) and the ENTRY-BYTE SET (→ PROVE-10
`Dfa::find` prefilter). Both are stateless one-shot SIMD passes already resident in bbnf-simd;
9C is the emit pass that selects among them by partition shape and the deletion of the
hand nibble-LUT scanners they subsume. Wide-unicode is the honest floor; the digit scan is
refuted; the classify pass never walks state.
