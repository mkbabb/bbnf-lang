# LOCK-6 · Family 6D — ORACLE-ISOMORPHISM · pass-3 research

Behavioral isomorphism only: a shared conformance corpus and a differential digest decide
Rust↔TS correspondence; layout is free per language. The family is a null hypothesis — it
must be *defeated* by measured maintenance/drift cost, or it wins. Pass-2 specified the
mechanism and named the one missing artifact: a Rust-side fold of the same spec compared
against the TS fold (PROVE-14's two arms were both JS). **This pass builds and runs it.** Q1
is answered by a live cross-language instance; Q2 by measured escape/detection numbers; Q3 by
a construction argument with LOC pointers; Q4 by a checked-in contract data file with the DAG
constraint carried.

---

## Q1 — the digest folds bit-identically Rust↔V8 (RUNNING INSTANCE, not spec text)

Two **independent** parsers fold the **one** spec and are compared as 64-bit integers:

- **Rust arm** — a hand-written recursive-descent JSON parser (no serde, no V8) building a
  UTF-16 value tree, folding a u64 digest. `rustc -O`, ~110 LOC.
- **V8 arm** — native `JSON.parse` feeding the *identical* fold in BigInt-64 arithmetic
  (bit-identical to Rust `u64` wrapping), ~40 LOC.

The fold: FNV-1a-64 (`offset 0xcbf29ce484222325`, `prime 0x100000001b3`); strings folded over
**UTF-16 code units** (the astral lane — an emoji is one code point, two units); numbers
`f64→to_le_bytes(8)` with NaN canonicalized to `0x7ff8000000000000`; objects order-independent
(XOR of `mix(key_digest,value_digest)`, `mix(k,v)=k·0x9e3779b97f4a7c15 ^ rotl(v,31)`); arrays
sequential with length salt.

**Result — three corpora, cold, one shot each:**

| corpus | Rust digest | V8 digest | strings | numbers | objects | UTF-16 units | match |
|---|---|---|--:|--:|--:|--:|:--:|
| twitter.json | `38fcfcbfd51a8a55` | `38fcfcbfd51a8a55` | 4754 | 2109 | 1264 | 137128 | **=** |
| canada.json | `ac209fa467f81721` | `ac209fa467f81721` | 4 | 111126 | 4 | 37 | **=** |
| citm_catalog.json | `816a417082450dce` | `816a417082450dce` | 735 | 14392 | 10937 | 16243 | **=** |

Bit-identical on every corpus. This exercises the three drift-prone lanes concretely:
twitter's **astral / Japanese `\n`-laden strings** (137 128 UTF-16 units folded identically),
canada's **111 126 f64 numbers** (the correctly-rounded-parse guarantee, realized: Rust
`str::parse::<f64>` (Eisel–Lemire) and V8 `StringToDouble` return bit-identical f64 for every
finite decimal, so the 8-byte LE fold agrees by construction), and citm's **10 937 nested
objects** (order-independent XOR fold). Two *independent* parsers agreeing bit-for-bit is the
N-version property PROVE-14 lacked — it proves the fold certifies **semantic materialization**,
not layout. Artifacts: `oracle/fold.rs`, `oracle/fold.mjs` (scratchpad); reproduce with
`rustc -O fold.rs -o fold && ./fold X.json` vs `node fold.mjs X.json`.

**Endianness / residue (settled).** LE on both sides (aarch64-native; Rust `to_le_bytes`);
a naive `to_be_bytes()` Rust mirror would itself be the drift bug. The un-exercisable residue
is exactly the non-JSON-expressible float set (NaN, signed-zero) — closed by **spec rule**
(NaN→sentinel is in the fold and fires on a golden vector even though no JSON emits NaN;
signed-zero policy = *distinguish*, stated in the contract), not by test.

---

## Q2 — injected divergences: static corpus vs coverage-guided fuzzer (BOTH numbers)

Five divergence classes seeded into the V8 arm (one lane each), compared against the clean
digest. **Static-corpus catch (per corpus):**

| class (injection) | twitter | canada | citm | reachability |
|---|:--:|:--:|:--:|---|
| `escape` (`\n`→`n`, off-by-one escape decode) | CAUGHT | escaped | escaped | needs escaped string in corpus |
| `drop` (skip `array[0]`, dropped token class) | CAUGHT | CAUGHT | CAUGHT | any array-bearing corpus |
| `ulp` (last-ULP number delta) | CAUGHT | CAUGHT | CAUGHT | any number-bearing corpus* |
| `astral` (high-surrogate→`U+FFFD`) | CAUGHT | escaped | escaped | needs astral text in corpus |
| `errclass` (bool sense flip) | CAUGHT | escaped | escaped | needs a bool in corpus |

`*` My `ulp` injection is a *blanket* multiply (fires on every number) → caught at once. A
**genuine last-ULP rounding bug** (mis-round only at a decimal that straddles the round-to-even
tie) is fuzzer-reachable **only if the generator targets the mantissa boundary** — a curated
corpus of "nice" decimals misses it. This is the honest refinement of the "ulp caught 3/3" row.

**The measured law:** structural classes (`drop`) are caught by any corpus with the shape;
**feature-gated classes (`escape`, `astral`, `errclass`) are caught only by a corpus that
exercises the feature.** A twitter-only static suite catches 5/5; a canada-only suite catches
**2/5** (`drop`, blanket-`ulp`) and *silently passes a broken engine* on escapes/astral/bools.
Curation cannot guarantee alphabet span; grammar-driven generation can.

**Coverage-guided fuzzer — inputs-to-first-divergence** (100 000 grammar-generated values,
generator emits `\n`, tab, emoji, astral by construction):

| class | drop | errclass | ulp | astral | escape |
|---|--:|--:|--:|--:|--:|
| inputs-to-first-divergence | 1 | 1 | 2 | 5 | 16 |

All five caught within **16 inputs** — *because the generator spans the alphabet*. Cycles-to-
detection ∝ 1/feature-density (astral rarest at 5, `\n`-escape at 16; structural at 1). The
number that decides Q2: **static single-corpus catch = 2/5 (feature-poor) to 5/5 (feature-rich,
un-guaranteeable); coverage-guided catch = 5/5 in ≤16 inputs.** The fuzzer's whole value is
guaranteeing every alphabet feature (escape variants, both surrogate planes, bool, tie-case
number) is emitted — plus the named mantissa-boundary obligation for the subtle rounding class.

---

## Q3 — marginal maintenance cost of one *added* grammar (this mechanism alone)

Measured by construction against the running fold. The fold matches on the **6-variant value
alphabet** (Object/Array/String/Number/Bool/Null). For any grammar whose values are that
alphabet — TOML, CSV-as-arrays, Google Sheets, NDJSON — the marginal fold cost is **0 LOC**:
the same six constructors fold it, and the corpus is simply pointed at both arms. (My
`fold.rs`/`fold.mjs` fold *any* JSON-alphabet grammar today with zero edits.)

A grammar introducing a **genuinely new value variant** (CSS dimension `10px`, a date scalar)
costs exactly **one node-hash constructor + one tag byte per side** — `~3 LOC × 2` — because
the alphabet *is* the grammar-derived Value ADT (`DirectTypeKind`,
`skinny/crates/codegen/src/direct_schema.rs:52`; the tape token alphabet `JsonNodeKind`,
`skinny/crates/runtime/src/grammars/json/value.rs:12`). New variant = new alphabet symbol =
one fold arm; **the transport (the fold machinery) never changes.** Contrast the generator
siblings: PROTO-2 (pass-2) measured the **schema face scaling ~1 line per rule** (json 9 /
sheets 37 / css_l4 184) — a *per-grammar, rule-shaped* correspondence surface to generate or
lint on **both** sides. 6D's per-grammar marginal is O(new variants), not O(rules); for an
existing-alphabet grammar it is literally **zero**. That gap **is** the null-hypothesis
defeat/win datum: the differential oracle amortizes across grammars where the mirror/skeleton
families re-pay per grammar.

---

## Q4 — the in/out-of-contract partition, as a checked-in data file

Authored and checked in at `oracle/isomorphism-contract.yaml` (scratchpad; homes with the
LOCK-1 alphabet in the tree). The binding shape:

- **IN-CONTRACT (gated, RED on mismatch):** the folded value digest; the structural counts (an
  *independent* coarse channel — a divergence must collide in *both* to escape); the
  accept/reject verdict; and the **error class** — a small grammar-derived enum
  `{UnexpectedChar, UnterminatedString, TrailingData, DepthLimit, InvalidEscape, InvalidNumber}`.
- **OUT-OF-CONTRACT (free per language):** error **message wording** and error **span byte
  offset** (V8 and Rust never phrase identically; a scanner may report the block boundary vs
  the exact byte — that is layout); tape layout; allocation/shape.
- **MAY-EITHER (recorded, not gated):** duplicate-key resolution order; number canonical
  text-form (the folded *value* is identical; the text is free).

**The error-class toggle, decided:** gate the *class*, not the message and not the span. This
is a **named supersession** of `css_scan_differential.rs:1-24`, which gates "same STOP hit,
same `None` at EOF, **and same error offset + message**" — that is correct *within one
language* (one message table, one scanner) but must **relax to error-class-only across
languages**. The intra-Rust differential stays strict; the cross-language contract relaxes.

**DAG constraint carried.** The alphabet the fold enumerates **is** LOCK-1's grammar-derived
Value ADT — the fold cannot be specified until the variant set is frozen (a new sum variant is
a new fold arm). The reference-of-last-resort **is** LOCK-7's meta-circular oracle — a two-way
differential detects disagreement but cannot adjudicate which side is *correct*; 7D's
bbnf-in-bbnf reference is the tie-breaker (the N+1-th version, JEST model). Therefore this lock
**freezes strictly after LOCK-1 (alphabet) and LOCK-7 (tie-breaker)**: `LOCK-1 → LOCK-6`,
`LOCK-7 → LOCK-6`. The contract file records `freeze_after: [LOCK-1, LOCK-7]`.

The contract file (verbatim shape):

```yaml
fold_spec: {hash: fnv1a-64, offset: 0xcbf29ce484222325, prime: 0x100000001b3,
  string: {units: utf16-code-units}, number: {repr: f64, nan: canonicalize},
  signed_zero: distinguish, object: {order: independent}, array: {order: sequential}}
in_contract:  [folded_value_digest, structural_counts, accept_reject_verdict, error_class]
out_of_contract: [error_message_wording, error_span_byte_offset, tape_layout, allocation_and_shape]
may_either:   [duplicate_key_resolution_order, number_canonical_textform]
freeze_after: [LOCK-1, LOCK-7]
tie_breaker:  LOCK-7-meta-circular-oracle
```

---

## Substrate & integration seams (path:line where extant code serves)

- **The fold, generalized from PROVE-14.** `crates/bbnf-bench/ts-tape/tape.mjs:34-68`
  (`numHash/strHash/memberMix/arrayStep/finalizeDigest`) is the TS prototype; this pass lifts
  it to a **u64** spec (the u32 `fp` has a ~77k-input birthday-collision floor a fuzzer will
  hit) and adds the missing Rust mirror. The structural counters (`tape.mjs:70-83`) stay as the
  independent coarse channel.
- **The cross-language export shape already exists.** The Rust tape exposes the four planes as
  public slices — `Tape::{source, offsets, flag_cursors, flag_values}`
  (`skinny/crates/runtime/src/tape/mod.rs:120-132`); a real TS backend needs one wasm-bindgen
  export handing those slices to JS (PROVE-14's `tape_dump.rs` proves the shape). The fold rides
  the decoded value, so it is transport-agnostic: it works over the tape view **or** over
  `JSON.parse` output — proven here by folding V8's native tree.
- **The differential harness pattern is in-tree, intra-Rust.**
  `skinny/crates/bbnf-simd/tests/checkasm_parity.rs` is FFmpeg's checkasm verbatim —
  bit-identical dual run, misalignment sweep, and a **deliberate-bug-injection knob**
  (`BBNF_SIMD_INJECT_BUG`) that proves oracle sensitivity, plus a strict/non-strict split
  (`BBNF_SIMD_STRICT`) that is exactly the must-match / may-either partition. 6D's Q2 injection
  knob and Q4 gate/record split model on it directly.
  `skinny/crates/runtime/tests/css_scan_differential.rs:1-24` is the clean-room-reference
  pattern (independent scalar recognizer vs production kernel) — the intra-Rust ancestor whose
  "offset + message" gate Q4 relaxes across languages.
- **The alphabet home.** `DirectTypeKind` (`skinny/crates/codegen/src/direct_schema.rs:52`) +
  `JsonNodeKind` (`skinny/crates/runtime/src/grammars/json/value.rs:12`) are the grammar-derived
  variant set the fold enumerates — Q3's "new variant = one fold arm" seam.

---

## Named risks & counterexamples

- **R1 — collision hides a real divergence.** Two trees differing in one leaf fold equal.
  Mitigation: u64 fold (birthday ≈ 5·10⁹ inputs) **plus** the independent structural-count
  channel — a divergence must collide in both. (u32 alone fails at ~77k inputs; not optional.)
- **R2 — the oracle proves agreement, not correctness.** Two engines identically wrong (both
  mishandle a surrogate pair) match. Intrinsic N-version limit. Mitigation: LOCK-7's
  grammar-defined reference as the N+1-th version (why the DAG puts LOCK-7 before LOCK-6).
- **R3 — subtle rounding never fires.** Blanket-`ulp` is caught at input 2, but a real
  round-to-even tie bug is fuzzer-only *and* only if the generator targets the mantissa
  boundary (emit `d.ddde±e` straddling the tie). Named fuzzer obligation, not an assumption.
- **R4 — a residue rule is omitted from the spec.** The Rust mirror folds raw NaN bits; JSON
  never emits NaN so CI is green forever, then a computed/non-JSON grammar lands and diverges in
  production. Mitigation: NaN-canonicalize is a *compile-time* rule in the fold + a golden vector
  carrying a NaN sentinel — exercised though no grammar emits one.
- **R5 — code-unit counting drift.** UTF-16 units vs UTF-8 bytes vs code points differ for
  non-BMP text. The contract pins **UTF-16 code units** on both sides; the Rust arm converts via
  `encode_utf16`-equivalent (proven by twitter's 137 128-unit match). A Rust fold counting code
  *points* would silently diverge on every emoji.

---

## What a prototype must still test (to close the family)

1. **Fold over the production tape view, not just `JSON.parse`.** This pass folded V8's native
   tree; wire the Rust fold to `Tape::{offsets,flag_cursors,flag_values,source}` and the TS fold
   to the wasm-exported planes, and re-confirm the three-corpus bit-identity end-to-end through
   the real transport. **Gate: digests unchanged from the Q1 table.**
2. **Prove the N+1 tie-breaker (R2).** Wire LOCK-7's grammar-defined reference as a third fold;
   seed a both-production-engines-wrong case; confirm the reference flags the divergence the two
   hide. **Gate: reference RED, both engines green.**
3. **The subtle-rounding fuzzer obligation (R3).** A mantissa-boundary decimal generator; measure
   inputs-to-first-divergence for a *real* last-ULP parser bug (not the blanket multiply).
4. **u64 collision headroom.** Confirm no collision across the full coverage-fuzzer run at u64;
   record the coarse-channel independence as the second line of defense.

---

### Prior art (citations)

- **N-version / differential testing** — multiple implementations as cross-referencing oracles,
  no hand-built golden output; divergence is a candidate bug. (Differential testing — Wikipedia.)
- **N+1-version differential (reference as tie-breaker)** — JEST tests JS engines *and* the spec
  together, spec as the extra version; the model for LOCK-7 as the R2 adjudicator. (Park et al.,
  *JEST: N+1-version Differential Testing of JS Engines*, arXiv 2102.07498.)
- **checkasm** — FFmpeg `tests/checkasm/`: bit-identical dual run, misalignment sweep, deliberate
  bug injection to prove sensitivity — the in-tree `checkasm_parity.rs` ancestor of the Q2 knob.
- **Correctly-rounded float parse** — Rust `str::parse::<f64>` (Eisel–Lemire + slow fallback,
  round-to-nearest-ties-even) and V8 `StringToDouble` are both correctly rounded ⟹ bit-identical
  f64 for every finite decimal — realized here over canada's 111 126 numbers. (Nigel Tao,
  *Eisel–Lemire ParseNumberF64*; rust-lang/rust #62231.)
- **Coverage-guided grammar fuzzing** — coverage guidance for behavioral diversity + differential
  comparison for semantic inconsistency. (JITfuzz, ICSE 2023.)

Sources: [Differential testing — Wikipedia](https://en.wikipedia.org/wiki/Differential_testing) ·
[JEST N+1 Differential Testing](https://arxiv.org/pdf/2102.07498) ·
[Eisel–Lemire — Nigel Tao](https://nigeltao.github.io/blog/2020/eisel-lemire.html) ·
[rust-lang/rust #62231](https://github.com/rust-lang/rust/issues/62231) ·
[JITfuzz (ICSE 2023)](https://lingming.cs.illinois.edu/publications/icse2023e.pdf)
