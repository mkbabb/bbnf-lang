# LOCK-6 / family 6D — ORACLE-ISOMORPHISM (behavioral isomorphism only)

**Charter.** The rust/ts split is held equivalent by a shared conformance corpus and
differential tests ALONE — not by mirrored module trees (6A), not by a spec-generated skeleton
(6B), not by a single wasm core (6C). Layout is free per language: LLVM-shaped Rust (SoA
offset-tape, NEON kernels), V8-shaped TS (monomorphic hidden-class decoders, typed arrays). This
is the **null hypothesis of the plane**: the isomorphism boundary is *data* (a corpus + a
canonical, language-neutral digest), never *code*. It wins by default and is defeated only if
measured drift/maintenance cost exceeds a mirror-tree linter's (6A) or a skeleton generator's
(6B). This report makes the mechanism concrete, shows it is already realized several times
in-tree, incorporates the one LIVE counterexample that bounds it, and specifies the two measured
numbers 6D owes the lock.

The constitution's clause (iii) — "isomorphism to a reasonable point, *measured, not asserted*" —
is 6D's entire thesis: for a large surface the reasonable point is *nowhere in the code*, and the
proof is a drift-escape number near zero. 6D is also FLOOR, not merely a family: behavioral parity
is already the plan-of-record close gate (GN-4.W5 `value_parity` + `cross-lane.mjs`), so every
rival family is "6D **plus** a structural enforcement tax." The lock's question is whether that tax
buys enough drift-avoidance to justify itself, and on which surface.

---

## 1. The mechanism made concrete

Four coupled pieces: a **canonical digest** (the behavioral spec), a **differential runner** (the
harness, a wire protocol), a **shared corpus** (the substrate), and **grammar-directed fuzzing**
(the teeth) that converts the plane's one weakness — silent drift in un-exercised behavior — into
an offensive detector.

### 1.1 The canonical digest — a language-neutral value fold

Behavioral isomorphism needs a total, deterministic encoding of a parse result that both languages
emit identically. Three realized forms exist in the tree, ascending in generality:

- **(a) Canonical re-emission.** `crates/core/tests/json_canonical_parity.rs:105` renders the bbnf
  `JsonDocument` and `sonic_rs::to_string` through one shared normalizer
  (`common/json_normalize.rs::strip_insignificant_ws`) and asserts byte-equality across
  twitter/citm/canada/data_xl. The simplest differential oracle. Limit: "re-emit as JSON" does not
  generalize to css/csv/toml/sheets, and it couples the contract to a *textual* number rendering
  (§4.2).
- **(b) The offset-tape bytes.** PROVE-14 proves the SoA tape (`Tape::{offsets, flag_cursors,
  flag_values, source}`, accessors at `crates/core/src/runtime/tape/mod.rs` ~121-137) decodes
  **node-for-node identically** in TS and in `JSON.parse` — the tape is itself a language-neutral
  canonical artifact crossing byte-exactly (`tape_dump.rs:78` emits it, `ts-tape/verify.mjs`
  checks it). Limit: tape-shaped, so it is the *value* plane only, not accept/reject or diagnostics.
- **(c) A structural EVENT digest — the recommended general form.** PROVE-14's `verify.mjs` folds
  the walk into 6 structural counts + a code-unit total + a 32-bit fingerprint (`fp 986912547`,
  identical tape-vs-`JSON.parse` over 320 cold children). Generalize this to a **SAX-like typed
  event stream** over the Value-fabric alphabet — `ObjectOpen · Key(bytes) · String(bytes) ·
  Number(canonical) · Bool · Null · ArrayOpen · Close · …`, the variant set drawn from GN-3.W1's
  `DirectTypeKind` sum types (LOCK-1's value ADT) — folded by a fixed, endianness-neutral rolling
  hash. Two properties make it correct: the alphabet is grammar-*neutral*, so ONE digest law spans
  json/css/csv/toml/sheets; and the hash folds the *event stream*, never native memory, so Rust
  `u32` planes and V8 typed arrays digest alike. The canonical serialization can itself be
  grammar-defined (constitution i): the Value enum is grammar-derived, so its neutral encoding is a
  bbnf artifact, not a hand spec per side.

**The decisive digest-design insight — encode numbers by IEEE-754 bits, not by decimal text.** The
digest lane must NOT render a `f64` as decimal, because Rust `ryu`/`f64::Display` and V8
`Number.prototype.toString` produce the *same shortest round-tripping value* but *differently
formatted text* — `1e21` vs `1000000000000000000000`, the exponent-threshold at 1e-7/1e21, the
trailing-`.0`, `-0` vs `0`. Fold the **8-byte big-endian IEEE-754 bit pattern** (with a canonical
NaN and canonical `-0.0`→`+0.0` iff the grammar says signed-zero is out-of-contract) as the
`Number` token. This dissolves the entire §4.2 number-canonicalization hazard for the digest,
because bit-equality is language-agnostic and total. Textual re-emission parity (form a) still owes
a number-text contract — but that is a *separate, narrower* obligation confined to the grammars
that ship a canonical printer, not a tax on the general digest.

### 1.2 The differential runner — a conformance WIRE PROTOCOL, not a spawn

Promote today's ad-hoc `spawn node` (`ts_node_execute.rs`) into protobuf-conformance's shape: one
thin *conformance runner* per implementation, speaking a language-agnostic line protocol over
stdin/stdout — `{corpus_id, grammar, input_bytes} → {accept: bool, digest: u64, error_class?}`.
The harness (Rust host) feeds every corpus case to every runner and diffs across the impl set:

- Rust eager tape · Rust stop-early `get` · Rust `parse_into::<T>` · TS `TapeDoc` decoder · a
  **third-party reference** per grammar so no impl is privileged (spec-as-truth, WPT-style).

This is already realized narrowly and LIVE: `ts_node_execute.rs` spawns real `node` against the
W1-emitted TS JSON parser, walks a path, and compares the leaf **byte-for-byte** against Rust eager,
Rust lazy, and `JSON.parse`, with a **node-required, no-silent-skip** gate (`:17-21`, audit written
regardless of assertion outcome) — precisely the drift-detection discipline, un-generalized. The
wire-protocol form buys three things the spawn form lacks: (i) any impl plugs in without host
edits — including a third-party binary or the LOCK-7 meta-circular grammar oracle; (ii) **two-tier
CI** — committed golden-digest files as the fast in-repo lane, live cross-language spawn as the
gated slow lane, so per-commit cost is a file diff and node runs only when TS changes; (iii)
**attribution** — with a reference present, a mismatch is majority-voted to the wrong side; without
one, metamorphic invariants + the grammar oracle break the tie (§1.4, §4.3).

### 1.3 The shared corpus — the substrate

Stored once, language-agnostic (`input bytes` + either a golden digest or a named reference).
Fold in the extant material: `data/json/{twitter,citm_catalog,canada}.json`, the CSS corpora
(tailwind/material/bootstrap/animate under `skinny/corpora`), the CSV RFC4180 fixtures,
`crates/core/tests/fixtures`. **Missing and required — the accept/reject axis.** A
JSONTestSuite-style corpus (Seriot, "Parsing JSON is a Minefield", 2016) partitioned three ways —
**must-accept · must-reject · may-either** — so implementation-defined inputs (deep nesting,
duplicate keys, number precision, leading zeros, astral surrogates) DO NOT flap the differential.
The three-way partition is load-bearing: a plain accept/reject binary would make every impl-defined
case a false RED. The corpus's consumer is the CI differential gate (consumer-less substrate
forbidden, constitution viii); it is a colocated data home feeding a directory-module runner
(colocation edict).

### 1.4 Grammar-directed differential fuzzing — the teeth

The corpus catches only what it exercises; that is the plane's one real weakness. But the grammar
IS the source of truth (constitution i), so sample it to synthesize valid inputs AND near-miss
invalid ones (single-edit mutations of valid strings — the reject corpus, near-free), feed both
impls, diff digests. A mismatch is a bug in exactly one side. This is **Csmith-for-grammars**, and
it is nearly free because the generator is the grammar already in hand. Make it *coverage-guided* —
track grammar-rule/edge coverage of generated inputs (the Zest/RLCheck idea) so the fuzzer drives
toward un-exercised productions, and **shrink** every counterexample by delta-debugging to a minimal
input before reporting. This converts the null hypothesis from "wins unless we forget a test" into
"wins because drift cannot hide": the measurable proxy is *grammar coverage reached × drift-escape
rate*, and 6D's claim is that this product bounds silent drift near zero.

---

## 2. Prior art (the pattern is settled SOTA; the exotic bets are 6A/6B)

**Differential / conformance-by-corpus (the core pattern).**
- **Protobuf conformance suite** — the strongest analogue: ~10 idiomatic language impls held
  behaviorally isomorphic by ONE conformance runner (`ConformanceRequest → ConformanceResponse`
  over a pipe) + one corpus, zero shared code, each impl fully native. Exactly 6D at production
  scale, and the source of §1.2's wire protocol.
- **Web Platform Tests** — one shared corpus, many independent browser engines; the *spec* is
  truth, not any engine. The canonical "many idiomatic impls, one behavioral spec, shared corpus."
- **sqllogictest (SQLite)** — hashes query result sets (MD5 of the normalized rows) and compares
  across database engines: the digest-differential model verbatim (§1.1c).
- **JSONTestSuite** (Seriot 2016) — the y_/n_/i_ acceptance corpus and the impl-defined taxonomy;
  the standard JSON conformance substrate and the source of §1.3's three-way partition.
- **CommonMark `spec.txt`; Unicode `NormalizationTest.txt`/`BidiTest.txt`; Kaitai Struct KST; the
  WebAssembly `.wast` testsuite** — data-driven, language-neutral conformance tables run across
  every implementation.

**Canonical-digest design (§1.1c).**
- **RFC 8785 JSON Canonicalization Scheme** and **DAG-CBOR / IPLD content addressing** — total
  canonical serializations that are language-neutral by construction; the precedent for "one digest
  both languages emit identically," and independent confirmation that number canonicalization is the
  hard corner (JCS mandates ECMAScript `Number.toString`, exactly the cross-language format hazard
  §1.1's bit-pattern trick sidesteps for the digest lane).

**Grammar-directed fuzzing (§1.4).**
- **Csmith** (Yang/Chen/Eide/Regehr, PLDI 2011) — grammar-driven differential testing that found
  hundreds of GCC/LLVM bugs; the model. **grammarinator, Nautilus, Superion** — grammar/coverage
  fuzzers. **Zest & RLCheck** (Padhye et al.) — validity-/coverage-guided property generation.
  **Hypothesis** (Python), **proptest/quickcheck/`arbitrary`** (Rust) — generator + automatic
  shrinking; **swarm testing** (Groce et al.) for feature-subset diversity. **Delta-debugging**
  (Zeller) — the minimal-counterexample shrinker §1.4 mandates.

**Oracle-free complement (for grammars with NO third-party reference, e.g. sheets).**
- **Metamorphic testing** (Chen/Cheung/Yiu 1998) — relations that must hold without a golden
  oracle: `digest(parse(serialize(parse(x)))) == digest(parse(x))` (idempotent round-trip),
  `parse(x)` accepts ⇒ `serialize` output re-parses to the same digest. This is 6D's answer where
  no reference exists, and it is already prefigured LIVE by `serialize_roundtrip.rs`.

Every one of these holds N idiomatic implementations equivalent by *data + differential*, never by
mirrored source. 6D is the established discipline; 6A/6B are the exotic bets that owe a measured
buyer.

---

## 3. Integration seams (path:line — extant code already serves)

| seam | path | role in 6D |
|---|---|---|
| canonical-form differential oracle (form a) | `crates/core/tests/json_canonical_parity.rs:105` | §1.1a realized; number-text hazard documented at `:68-80` |
| shared normalizer (one home) | `crates/core/tests/common/json_normalize.rs`; `common/css_normalize.rs`; `common/mod.rs` | the canonical-form spec, colocated |
| real cross-language runner (LIVE) | `crates/core/tests/ts_node_execute.rs` (`:17-21` node-required no-skip gate, `:41-53` audit sink) | §1.2 realized Rust↔node↔`JSON.parse` byte-for-byte; the spawn to promote to a wire protocol |
| language-neutral tape artifact | `crates/core/src/runtime/tape/mod.rs` ~121-137 | §1.1b the bytes that cross |
| corpus/golden generator | `crates/bbnf-bench/src/bin/tape_dump.rs:78` | emits golden tape planes for the corpus |
| structural-digest oracle | PROVE-14 `crates/bbnf-bench/ts-tape/verify.mjs` | §1.1c fingerprint + 6 counts, 320 cold children |
| third-party reference oracles (LIVE) | `crates/core/tests/{sonic_rs_parity,lightningcss_parity}.rs`; `json_value_parity.rs`, `json_parity_struct.rs` | the "no impl privileged" reference lane is already realized for JSON and CSS |
| sibling-grammar parity (pervasive) | `css_l4_canonical_parity.rs`, `css_color_parity.rs`, `sheets_expr_parity.rs`, `bbnf_self_parity.rs`, `serialize_roundtrip.rs` | 6D names this scattered pattern the plane's LAW; `serialize_roundtrip.rs` is the metamorphic seed (§2) |
| four-way oracle (PLANNED) | GN-4.W5 `crates/runtime/tests/value_parity.rs` + `ts/tape/test/cross-lane.mjs` | 6D pre-specified for four lanes; 6D generalizes it to one corpus/one digest/all grammars |
| the counterexample (LIVE) | `crates/core/tests/isomorphic_path_error.rs:129-150` | the bounding case, §4.1 |

The mechanism needs no invention — it needs **consolidation**: lift these scattered parity asserts
into ONE corpus + ONE digest + ONE wire-protocol runner set, add the three-way accept/reject
corpus, and add the coverage-guided grammar fuzzer.

---

## 4. Named risks / counterexamples (where the null hypothesis can LOSE)

**4.1 Coverage gap = silent drift (the decisive weakness) — and the LIVE proof that cuts both
ways.** Behavioral iso catches only what the corpus/fuzzer exercises. The tree already contains the
proof: `isomorphic_path_error.rs:129-150` documents that `$.statuses[0].nope` surfaces
`UnregisteredRule` on the Rust offline frontend but `UnknownField` through the cdylib — **identical
value shapes, divergent error semantics**. This is double-edged. (i) It refutes 6A/6B *alone*: a
mirror-tree linter and a spec skeleton both pass GREEN here (the surfaces are structurally
identical), so structure is necessary-not-sufficient. (ii) It also names 6D's own blind spot: 6D
catches this only if the corpus contains that input AND the contract counts error-classification as
in-contract. The response is the mechanism of §4.3 — 6D must *declare* whether error class is
in-contract, and if so add an `error_class` field to the digest (§1.2) and a near-miss corpus entry
(§1.4). The astral-surrogate analogue (Rust adds U+1xxxx decoding, TS stays wrong-and-green with no
astral string in the corpus) is the same failure and the same cure. This class is exactly where the
grammar fuzzer's coverage guidance must be MEASURED to close the gap, or 6D concedes the class to
6A/6B and the answer becomes a *layering*.

**4.2 Number canonicalization** — Rust `f64` Display vs V8 `Number.toString` differ on `.0`, `-0`,
exponent form, the 1e21/1e-7 thresholds, 17th-digit precision. **Solved for the digest** by folding
the IEEE-754 bit pattern (§1.1c), which is the single most important sharpening this family owes.
Textual re-emission (form a) still needs a per-grammar number-text contract, proven real and
solvable at `json_canonical_parity.rs:68-80`.

**4.3 Diagnostics are behavior too — the contract must be DECLARED.** If error messages/spans are
in-contract (PT-E1 per-parse diagnostics, `utils.ts:6`), byte-identical diagnostics across idiomatic
Rust/TS is expensive and probably wrong. The behavioral spec MUST partition: **values +
accept/reject = in-contract** (digested, gated); **message wording, span byte-encoding = out**;
**error *class* = a declared toggle** (the `nope` case §4.1). Under-specify → false confidence;
over-specify → the maintenance cost that itself defeats 6D. This partition IS the "reasonable point"
the constitution demands, expressed as data rather than code.

**4.4 The accept/reject / may-either axis** — without the three-way partition (§1.3), differential
tests flap on impl-defined inputs. This is not optional; it is the difference between a stable gate
and a flaky one.

**4.5 Digest determinism across languages** — the hash must fold the canonical *event stream*, not
native memory (endianness, `u32` stride, hashmap iteration order). Key-order and duplicate-key
handling must be pinned in the contract (document-order preserved vs canonicalized; last-wins vs
reject). PROVE-14's fp is event-defined and endianness-safe; keep it so.

**4.6 The corpus is itself a shared artifact** with drift/maintenance — but far cheaper than
mirrored code, grammar-derivable via the fuzzer, and the ONLY shared artifact 6D introduces (vs
6A's name map + linter, 6B's spec + two generators). This cost is bounded, and bounding it *is* the
family's measured claim.

---

## 5. What a prototype MUST test to move this family

Because 6D is the null hypothesis, the prototype does not merely *show the oracle works* (PROVE-14
and `ts_node_execute.rs` already do) — it must **measure drift cost** and name the behavior classes
6D cannot cheaply cover. Two numbers decide the family; both are MEASURED, neither assertable.

1. **Standing cross-language differential harness (the substrate).** Promote `ts_node_execute.rs` +
   `verify.mjs` into one wire-protocol runner (§1.2) emitting the §1.1c event-digest with numbers
   folded as IEEE-754 bits, over the full extant corpus (twitter/citm/canada + css + csv), across
   Rust eager/stop-early/typed + TS `TapeDoc` + a third-party reference (`sonic_rs`/`lightningcss`,
   already LIVE). Gate: 100% digest agreement — reproduces PROVE-14's parity as a STANDING LAW, not
   a one-shot.

2. **Injected-divergence experiment (THE decider → drift-escape rate).** Introduce N deliberate
   behavioral divergences — off-by-one TS escape decode, a dropped CSS token class, a number-format
   delta, an astral-string mishandle, the `nope` error-class flip (§4.1) — and MEASURE: (a) fraction
   caught by the static corpus, (b) fraction caught by the coverage-guided grammar fuzzer, (c)
   CI cycles-to-detection, (d) grammar-rule coverage reached. If corpus+fuzz catches ≈100% within
   one cycle, drift cost → ~0 and 6D wins outright; any class that systematically escapes
   (diagnostics, astral strings) is precisely where 6A/6B earn their keep — report it as the honest
   boundary, which converts the verdict from "6D-as-center" to a named layering.

3. **Marginal-maintenance-cost experiment (the null-hypothesis test).** Add ONE new grammar (TOML —
   none exists anywhere in-tree, per the codex) and reach behavioral parity under 6D alone
   (corpus + digest, no mirror map, no skeleton); record LOC + author-effort. 6D wins iff this
   marginal cost is strictly below the hypothetical 6A mirror-tree / 6B skeleton cost for the same
   grammar AND the escaped-drift rate from (2) is bounded. Note sheets has no third-party reference:
   test the metamorphic fallback (§2) there and record that a reference-less grammar costs only the
   metamorphic relations, not a golden oracle.

4. **Number/escape/accept-reject conformance.** Run a JSONTestSuite y_/n_/i_ + float round-trip
   corpus through both impls; confirm the IEEE-754-bit digest eliminates number flaps and quantify
   how much per-grammar contract (§4.2/§4.3) remains — the true recurring cost of the plane.

The verdict 6D owes the lock: a number for **drift-escape rate** (under corpus+coverage-guided fuzz)
and one for **marginal maintenance cost per grammar**. If both are low, behavioral isomorphism is
the correct boundary and layout mirroring is cost without a measured buyer; if a class systematically
escapes, that class — and only that class — is where a linter (6A) or a generator (6B) earns its
tax, and the plane's answer is 6D-as-floor beneath a thin structural cap.

---

## 6. Cross-lock dependencies (what 6D consumes and feeds)

6D is deliberately thin because it borrows, not builds. It **consumes**: LOCK-1's grammar-derived
Value ADT as the digest alphabet (§1.1c); LOCK-5's regex behavioral-parity verdict (Rust bespoke
DFA vs TS native `RegExp` are NOT mirrored — 6D digests their *acceptance behavior*, unaffected by
their non-isomorphic internals); LOCK-7's 7D **meta-circular grammar oracle** as the reference for
grammars lacking a third-party one (sheets), which is also 6D's attribution tie-breaker (§1.2). It
**feeds**: the GN-4.W5 four-way parity close gate (already the plan of record), the parse-that
consumer differential (value.js/latex-paper/keyframes conformance), and every gestalt as the
mandatory behavioral FLOOR — the one family that is installed regardless of which structural cap, if
any, the lock crowns.
