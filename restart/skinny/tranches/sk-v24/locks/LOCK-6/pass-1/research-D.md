# LOCK-6 / family 6D — ORACLE-ISOMORPHISM (behavioral isomorphism only)

**Charter.** The rust/ts split is held equivalent by a shared conformance corpus and
differential tests alone — not by mirrored module trees (6A), not by a spec-generated
skeleton (6B), not by a single wasm core (6C). Layout is free per language: LLVM-shaped
Rust (SoA offset-tape, NEON kernels), V8-shaped TS (monomorphic hidden-class decoders,
typed arrays). This is the **null hypothesis of the plane**: the isomorphism boundary is
*data* (corpus + a canonical, language-neutral digest), never *code*. It wins by default;
it is defeated only if measured drift/maintenance cost exceeds a mirror-tree linter's or a
skeleton generator's. This report makes the mechanism concrete, shows it is already realized
three times in-tree, and specifies the experiment that decides the null hypothesis.

---

## 1. The mechanism made concrete

Three coupled pieces: a **canonical form** (the behavioral spec), a **differential runner**
(the harness), a **shared corpus** (the substrate). A fourth — grammar-directed fuzzing —
converts the plane's one weakness (silent drift in un-exercised behavior) into an offensive
detector.

### 1.1 The canonical form — a language-neutral value digest

Behavioral isomorphism needs a total, deterministic encoding of a parse result that both
languages emit identically. Three realized forms exist in the tree, in ascending generality:

- **(a) Canonical re-emission.** `crates/core/tests/json_canonical_parity.rs:105`
  (`canonical_parity`) renders both the bbnf `JsonDocument` and `sonic_rs::to_string` through
  one shared normalizer (`common::json_normalize::strip_insignificant_ws`) and asserts
  byte-equality across twitter/citm/canada/data_xl. This is the differential oracle in its
  simplest form. Limit: "re-emit as JSON" does not generalize to css/csv/toml/sheets.
- **(b) The offset-tape bytes.** PROVE-14 proves the SoA tape (`Tape::{offsets, flag_cursors,
  flag_values, source}`, `crates/runtime/src/tape/mod.rs:126-137`) decodes **node-for-node
  identically** in TS and in `JSON.parse` — the tape *is* a language-neutral canonical artifact
  that crosses the boundary byte-exactly (`tape_dump.rs:78` emits it, `ts-tape/verify.mjs`
  checks it).
- **(c) A structural event digest.** PROVE-14's `verify.mjs` folds the walk into 6 structural
  counts + code-unit total + a 32-bit fingerprint (`fp 986912547` identical tape-vs-JSON.parse
  over 320 cold children). This is the general form and the recommended canonical spec: a
  **SAX-like typed event stream** over the Value-fabric alphabet (ObjectOpen/Key(bytes)/
  String(bytes)/Number(canonical)/Bool/Null/…, from GN-3.W1 sum types) hashed by a fixed
  endianness-neutral rolling hash. The alphabet is grammar-neutral, so one digest law spans
  json/css/csv/toml/sheets; the hash is defined over the event stream, never over native
  memory layout (so Rust `u32` planes and V8 typed arrays digest alike).

The canonical serialization can itself be grammar-defined (constitution i) — the Value enum's
shape is grammar-derived, so its neutral encoding is a bbnf artifact, not a hand spec on each
side.

### 1.2 The differential runner — protobuf's conformance-runner shape

One thin *conformance runner* per language: read a corpus input, parse via that language's
idiomatic path, emit the canonical digest to stdout. The harness feeds every corpus case to
every runner and diffs digests across the implementation set:

- Rust eager tape · Rust stop-early `get` · Rust `parse_into::<T>` · TS `TapeDoc` decoder ·
  a **third-party reference** (`serde_json`/`sonic_rs`/`JSON.parse`) so no impl is privileged
  — spec-as-truth, WPT-style.

This is not speculative: `crates/core/tests/ts_node_execute.rs` already spawns real `node`
against the TS-emitted parser, walks `["statuses",0,"text"]`, and compares the leaf
**byte-for-byte** against the eager Rust path, the lazy Rust path, and `JSON.parse`
(`:303` the reference, `:329` the walk) — with a *node-required, no-silent-skip* gate
(`:249`) that is precisely the drift-detection discipline. GN-4.W5's planned four-way oracle
(`crates/runtime/tests/value_parity.rs` + `ts/tape/test/cross-lane.mjs`, GN-4.md:407,424)
is this runner already specified in the extant baseline for four lanes; 6D's contribution is
to *generalize it into the standing law of the plane* — one corpus, one digest, all grammars,
both languages — rather than a per-tranche parity assert.

### 1.3 The shared corpus — the substrate

Stored once, language-agnostic (`input bytes` + either a golden digest or a named third-party
reference). Extant material to fold in: value corpora `data/json/{twitter,citm_catalog,
canada}.json`, the css corpora (tailwind/material/bootstrap/animate under `skinny/corpora`),
csv RFC4180 fixtures, `crates/core/tests/fixtures`. Missing and required: a **JSONTestSuite-
style accept/reject corpus** (y_/n_/i_ classes) so the differential covers the *accept/reject*
axis, not only the *value* axis — and it must partition **must-accept / must-reject /
may-either** so implementation-defined cases (deep nesting, duplicate keys, number precision)
do not make the differential flap. The corpus's consumer is the CI differential gate
(consumer-less-substrate forbidden, constitution viii); it is a colocated data home, the
runner a directory-module (colocation edict).

### 1.4 Grammar-directed differential fuzzing — the teeth

The corpus catches only what it exercises; that is the plane's one real weakness. But the
grammar *is* the source of truth (constitution i), so sample the bbnf grammar to generate
valid inputs (and near-miss invalid ones), feed both impls, diff digests. A mismatch is a
bug in exactly one side. This is Csmith-for-grammars: it makes drift *offensively detectable*
and is nearly free because the generator is the grammar already in hand. It is the argument
that turns the null hypothesis from "wins unless we forget a test" into "wins because drift
cannot hide."

---

## 2. Prior art (the pattern is settled SOTA elsewhere)

- **Protobuf conformance suite** — the strongest analogue: ~10 idiomatic language
  implementations held behaviorally isomorphic by ONE conformance runner
  (ConformanceRequest→ConformanceResponse over a pipe) + one test corpus, zero shared code,
  each impl fully native. Exactly 6D at production scale.
- **Web Platform Tests (WPT)** — one shared corpus, many independent browser engines, the
  *spec* is truth, not any engine. The canonical "many idiomatic impls, one behavioral spec,
  differential via shared corpus" pattern.
- **sqllogictest (SQLite)** — hashes query result sets and compares across database engines;
  the digest-differential model verbatim (§1.1c).
- **JSONTestSuite** (Seriot, 2016) — the y_/n_/i_ acceptance corpus; the standard JSON
  conformance substrate (§1.3).
- **CommonMark** `spec.txt` — one data file drives every implementation's conformance.
- **Unicode conformance data** (`NormalizationTest.txt`, `BidiTest.txt`) — data-driven,
  language-neutral conformance tables.
- **Differential-testing lineage** — McKeeman, "Differential Testing for Software" (Digital
  Technical Journal, 1998); Csmith (Yang/Chen/Eide/Regehr, PLDI 2011) for C compilers;
  Crockford's `jsonchecker`. **Golden/snapshot** kin: `insta` (Rust), Jest snapshots (TS).
- **Kaitai Struct KST** — a shared cross-target test corpus over parsers generated into many
  languages; and the **WASM `.wast` testsuite** run across every engine.

Every one of these holds N idiomatic implementations equivalent by *data + differential*,
never by mirrored source. 6D is the established discipline; 6A/6B are the exotic bets.

---

## 3. Integration seams (path:line — extant code already serves)

| seam | path | role in 6D |
|---|---|---|
| canonical-form differential oracle | `crates/core/tests/json_canonical_parity.rs:105` | §1.1a realized; number-canon hazard handled at `:74-77` |
| shared normalizer | `crates/core/tests/common/json_normalize.rs` (`strip_insignificant_ws`) | the canonical-form spec, one home |
| real cross-language runner | `crates/core/tests/ts_node_execute.rs` (`:249` no-skip gate, `:303` ref, `:329` walk) | §1.2 realized Rust↔node↔JSON.parse, byte-for-byte |
| language-neutral tape artifact | `crates/runtime/src/tape/mod.rs:126-137` | §1.1b the bytes that cross |
| corpus-fixture generator | `crates/bbnf-bench/src/bin/tape_dump.rs:78` | emits golden tape for the corpus |
| structural-digest oracle | PROVE-14 `crates/bbnf-bench/ts-tape/verify.mjs` | §1.1c fingerprint + counts, 320 cold children |
| four-way oracle (planned) | GN-4.md:407,424 → `value_parity.rs` + `ts/tape/test/cross-lane.mjs` | 6D pre-specified for four lanes; 6D generalizes it |
| sibling-grammar parity | `css_l4_canonical_parity.rs`, `sheets_expr_parity.rs`, `bbnf_self_parity.rs`, `serialize_roundtrip.rs` | the pattern is already pervasive; 6D names it the plane's law |

The mechanism needs no invention — it needs **consolidation**: lift these scattered parity
asserts into one corpus + one digest + one runner set, and add the accept/reject corpus and
the grammar fuzzer.

---

## 4. Named risks / counterexamples (where the null hypothesis can LOSE)

1. **Coverage gap = silent drift** (the decisive weakness). Behavioral iso catches only what
   the corpus/fuzzer exercises. *Counterexample:* Rust adds astral-plane surrogate-pair
   decoding; corpus has no U+1xxxx string; TS stays wrong and green. This is exactly the drift
   6A's mirror-tree linter and 6B's skeleton exist to make structurally impossible — 6D must
   *measure* that fuzzing closes this gap, or concede the class.
2. **Number canonicalization** — Rust `f64` Display vs V8 `Number.toString` differ on `.0`,
   `-0.0`, exponent form, 17th-digit precision. The canonical form MUST pin a number
   normalization or byte-equality falsely fails/passes; `json_canonical_parity.rs:74-77` proves
   the hazard is real and solvable, but it is per-grammar work.
3. **Diagnostics are behavior too.** If error messages/spans are in-contract (PT-E1 per-parse
   diagnostics, `utils.ts:6`), byte-identical diagnostics across idiomatic Rust/TS is expensive
   and probably the wrong goal. The behavioral spec MUST declare which behaviors are in-contract
   (values, accept/reject) vs out (message wording, span encoding). Under-specify → false
   confidence; over-specify → the very maintenance cost that defeats 6D.
4. **The accept/reject / may-either axis.** Without a must-accept/must-reject/may-either
   partition (§1.3), differential tests flap on implementation-defined inputs (duplicate keys,
   deep nesting, leading zeros).
5. **Digest determinism across languages.** The hash must fold a canonical *event stream*, not
   native memory (endianness, `u32` stride) — a naive memory hash breaks the crossing.
   PROVE-14's fp is event-defined and endianness-safe; keep it so.
6. **Corpus is itself a shared artifact** with drift/maintenance — but far cheaper than mirrored
   code, and grammar-derivable via the fuzzer, so this cost is bounded, not open-ended.

---

## 5. What a prototype MUST test to move this family

Because 6D is the null hypothesis, the prototype does not merely *show the oracle works*
(PROVE-14 and `ts_node_execute.rs` already do) — it must **measure drift cost** and name the
behavior classes 6D cannot cheaply cover.

1. **Standing cross-language differential harness.** Extend `ts_node_execute.rs` +
   `verify.mjs` into one runner emitting the §1.1c event-digest, over the full extant corpus
   (twitter/citm/canada + css + csv), for Rust eager/stop-early/typed and the TS `TapeDoc`,
   plus a third-party reference. Gate: 100% digest agreement (reproduces PROVE-14's parity as a
   standing law, not a one-shot).
2. **Injected-divergence experiment (the decider).** Introduce N deliberate behavioral
   divergences — off-by-one TS escape decode, a dropped CSS token class, a number-format delta,
   an astral-string mishandle — and MEASURE: (a) fraction caught by the static corpus, (b)
   fraction caught by the grammar-directed fuzzer, (c) CI cycles-to-detection. If corpus+fuzz
   catches ≈100% within one cycle, drift cost → ~0 and 6D wins outright; any class that
   systematically escapes (diagnostics, astral strings) is precisely where 6A/6B earn their
   keep — report it as the honest boundary.
3. **Maintenance-cost measurement (the null-hypothesis test).** Add ONE new grammar (TOML) and
   reach behavioral parity under 6D (corpus + digest only); record LOC + author-effort. 6D wins
   iff this marginal cost is strictly below the hypothetical 6A mirror-tree / 6B skeleton cost
   AND the escaped-drift rate from (2) is bounded.
4. **Number/escape/accept-reject conformance.** Run a JSONTestSuite y_/n_/i_ + float
   round-trip corpus through both impls; measure divergence specifically on the known hazard
   axes (§4.2, §4.4). This quantifies how much of the canonical-form spec (§1.1) must be pinned
   per grammar — the true recurring cost of the plane.

The verdict 6D owes the lock: a number for **drift escape rate** and one for **marginal
maintenance cost per grammar**. If both are low, behavioral isomorphism is not merely adequate
— it is the correct boundary, and layout mirroring is cost without a measured buyer.
