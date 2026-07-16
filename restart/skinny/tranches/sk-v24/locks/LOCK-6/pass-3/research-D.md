# LOCK-6 · 6D — ORACLE-ISOMORPHISM · Research (pass 3)

Behavioral isomorphism only: one shared conformance corpus, one shared digest fold,
differential Rust↔V8 tests; layout free per language. The null hypothesis under test is
that this behavioral contract beats a mirrored/generated-layout regime on measured
maintenance and drift cost — or that it is defeated by a number/span canonicalization tax
that a layout regime would not pay. This pass runs the fold cross-language for real,
injects divergences, prices one added grammar, and pins the in/out-of-contract partition
as a data file. The running instance is decisive and it reshapes the mechanism.

---

## The mechanism, made concrete

The oracle is three homomorphic parts, each a total function into a `u32`:

1. **The corpus** — a checked-in set of source documents plus a per-document expectation
   record (accept/reject, and for accepted inputs the canonical value stream). Layout of
   the parser output is free per language; only the value stream is contracted.
2. **The fold** — FNV-1a over `u32` (`prime = 0x01000193`, offset basis `0x811c9dc5`),
   mixing via `Math.imul(h ^ x, prime) >>> 0` on V8 and `(h ^ x).wrapping_mul(prime)` in
   Rust. This is already the PROVE-14 digest arithmetic (`crates/bbnf-bench/ts-tape/tape.mjs`
   shared node-hash primitives; the `mix` at PROVE-14/patch.diff:289/357/387). It folds
   structural counts, UTF-16 code units, and per-leaf value hashes into one fingerprint.
3. **The differential harness** — Rust folds one side, node folds the other; the two
   fingerprints must be `==`. On mismatch the harness bisects the corpus to the first
   diverging document (the fold is a left chain, so a per-document reset gives O(log N)
   bisection to the culprit).

**Substrate that already exists.** The fold primitives, the UTF-16-unit string hash, the
structural-count digest, and the `digestsEqual` comparator are the PROVE-14 code verbatim
(`tape.mjs`, `verify.mjs` at PROVE-14/patch.diff:394/699-731). The Rust plane accessors that
feed a Rust-side fold are live: `Tape::{source,offsets,flag_cursors,flag_values}`
(`crates/runtime/src/tape/mod.rs` ~121-137). GN-4.W5 already specifies a four-lane parity
oracle (`crates/runtime/tests/value_parity.rs`, `ts/tape/test/cross-lane.mjs`,
docs/tranches/sk-v24/GN-4.md:407/420-428) whose close gate is *exactly* node-for-node
agreement across the language boundary — 6D is the generalization of that gate from JSON to
every grammar, and from four in-repo lanes to a two-language contract.

---

## Q1 — the fold RAN cross-language; it does NOT fold bit-identically for numbers

A running instance (not spec text): a Rust fold (rustc -O) and a V8 fold (node v26) each
read a 27-value f64 corpus and fold it three ways — over each value's **native shortest
decimal text**, over the **raw IEEE-754 u64 bits** (split lo/hi `u32`), and over a
**span-length** stream derived from the native text. Result:

| fold lane | Rust digest | V8 digest | verdict |
|---|---|---|---|
| native decimal text | `cce194de` | `c8d6e5e1` | **DIVERGES** |
| raw IEEE-754 bits | `b99874e8` | `b99874e8` | **bit-identical** |
| span-length (from text) | `22ec9009` | `fe9db4c4` | **DIVERGES** (inherits text) |

The bits lane proves the fold arithmetic itself is genuinely isomorphic across Rust and V8
— identical `u32` FNV-1a by construction. The divergence is **not** in the machinery; it is
in the *canonical-decimal spec*. Rust `core::fmt` float Display and V8
`Number.prototype.toString` (ECMA-262 §7.1.12.1) are BOTH shortest-round-trip in the
significand, but disagree on three enumerable classes:

- **Exponential threshold.** Rust Display is always positional; V8 emits exponential for
  exponent ≥ 21 or magnitude < 1e-6. Concrete: `1e21` → Rust `1000000000000000000000`
  vs V8 `1e+21`; `1e-7` → Rust `0.0000001` vs V8 `1e-7`; `6.022e23` → Rust
  `602200000000000000000000` vs V8 `6.022e+23`; subnormal `5e-324` → Rust a 300-digit
  positional expansion vs V8 `5e-324`.
- **Sign of zero.** `-0.0` → Rust `-0`, V8 `String(-0)` → `0`. A divergence *outside* the
  exponential range entirely.
- **Agreement elsewhere is real.** Every normal-magnitude value agreed exactly —
  `0.30000000000000004`, `123456789.12345679`, `9007199254740992`, `0.1/0.2/0.3` — so the
  divergence set is bounded and nameable, not pervasive.

**The verdict for the family.** Behavioral isomorphism is sound *only if the contract owns
number canonicalization* rather than inheriting either language's native formatter. Two
lawful canonical forms exist and both fold bit-identically: (a) fold the **IEEE-754 bits**
(proven above — zero implementation cost, but loses "did the two parsers agree on the same
DECIMAL" and cannot catch a formatter bug); (b) a **contract-defined normalized decimal**
(shortest round-trip, fixed exponential rule, `+0` for zero) implemented once per language
and cross-checked. The native-text fold is a **latent false-RED generator** and must be
forbidden in the corpus spec. This is the pass-3 discriminator: 6D's maintenance cost is
dominated by whichever canonicalizers the contract must carry, not by module mirroring.

Reproduce: a Rust `fold.rs` + a node `fold.mjs` over a boundary-vector `corpus.txt`
(`rustc -O fold.rs -o fold_rs && ./fold_rs < corpus.txt` vs `node fold.mjs corpus.txt`) —
built and run this pass; digests above are the live output.

---

## Q2 — divergence injection: static corpus vs coverage-guided fuzzer

Two numbers, measured and reasoned on the running fold:

- **Static corpus, hand-picked (27 values):** already surfaces **3 distinct divergence
  classes** (exp-high ≥1e21, exp-low <1e-6, sign-of-zero) with zero fuzzing — because the
  vectors were chosen to straddle the ECMA-262 thresholds. A static corpus catches exactly
  the classes its authors foresaw; its blind spot is the class nobody thought to encode
  (e.g. the `1e-6`/`1e-7` boundary is easy to miss by one exponent).
- **Cycles-to-detection, once a divergent value is in the fold: 1.** A single injected
  last-ULP delta (`0.3` → `0.3 + 1 ULP` via bit increment) flipped the bits digest
  `69b08416` → `a9ae2539` (run this pass). Because the fold is a left chain, ANY single-value
  divergence changes the terminal fingerprint deterministically; detection at fold-end is
  guaranteed and bisection localizes it in O(log N). The injected classes and their catchers:

  | injected divergence | native lane | bits lane | who catches it |
  |---|---|---|---|
  | last-ULP number delta | catches (text differs) | **catches** (bits differ) | both; bits is canonicalization-proof |
  | off-by-one escape decode | catches (UTF-16 units differ) | n/a (string) | string-unit fold |
  | dropped token class | catches (structural count differs) | n/a | count digest |
  | astral-string mishandle | catches (`a😀b` = 4 UTF-16 units) | n/a | UTF-16-unit fold (NOT scalar fold) |
  | error-class flip (accept↔reject) | catches (accept/reject field) | n/a | expectation record |

  The astral case is a **design constraint, not a free win**: the fold MUST iterate UTF-16
  code units (`charCodeAt` / `encode_utf16`), never Unicode scalars, or a surrogate-pair
  mishandle folds identically and escapes. Probed this pass: `a\u{1F600}b` has UTF-16 length
  4 in V8; the digest counts *codeUnits*, matching V8's `String.length` and Rust's
  `.encode_utf16().count()`.

- **The coverage-guided fuzzer's marginal value** is precisely the *unforeseen* class: it
  would have discovered the `-0` and the `1e-6` boundary without an author encoding them.
  Recommendation: a small hand-corpus for the named classes (fast CI gate, deterministic)
  PLUS a periodic coverage-guided fuzzer seeded from the corpus that mutates numbers toward
  formatter thresholds and strings toward surrogate/escape boundaries — the fuzzer's finds
  get *promoted into* the static corpus as regression vectors. Prior art:
  `cargo-fuzz`/libFuzzer differential targets (the pattern rustc uses against Miri, simdjson
  against RapidJSON); the "fold two impls, assert equal, minimize on mismatch" shape is
  exactly a libFuzzer differential target.

---

## Q3 — marginal maintenance cost for one ADDED grammar

Under this mechanism *alone*, adding a grammar not yet in-tree (say TOML — GN-5.W2 notes no
TOML exists anywhere, R1) costs, in order:

1. **Corpus documents + expectation records** — N source files plus their canonical value
   streams. This is the irreducible cost and it is **grammar-authored, language-neutral,
   written once**. It is NOT duplicated per language.
2. **One canonical-value emitter per language** — but only for value *classes the grammar
   introduces that are not already contracted*. TOML introduces datetimes (RFC 3339) and
   integers with widths; strings/numbers/bools reuse the existing canonicalizers. So the
   marginal per-language code is a datetime canonicalizer (one function each side) — and
   Q1 proves it must be contract-defined (Rust `chrono` Display vs JS `Date.toISOString`
   would diverge on fractional-second trailing zeros exactly as f64 text did).
3. **Zero new fold code, zero new harness code** — the FNV-1a fold and the differential
   comparator are grammar-agnostic. This is the mechanism's central economy and the core of
   its case against 6B (spec-generated skeleton): the differential *harness* does not grow
   per grammar; only the *value contract* does, and that contract is data + a bounded
   canonicalizer set shared across all grammars.

**Net marginal cost per added grammar ≈ (corpus authoring) + (0..K new canonicalizer pairs,
where K = value classes not already in the shared set).** For grammars over the existing
value universe (CSV, sheets — strings/numbers only) K = 0: pure corpus authoring, no code.
This is the number that must beat the mirror/skeleton regimes' per-grammar layout-spec +
generator cost. The differential harness's fixed cost amortizes to zero across the fleet;
the layout-generation regimes re-pay per grammar. **This is 6D's affirmative case.**

---

## Q4 — the in/out-of-contract partition, as a checked-in data file

The partition is a committed `oracle-contract.toml` (grammar-agnostic; each grammar
references it). Drawn from the running evidence:

```toml
# oracle-contract.toml — the behavioral-isomorphism contract (checked in)
# DAG: this file freezes strictly AFTER LOCK-1 (alphabet) and LOCK-7 (tie-breaker).
[contract.in]
values      = true    # every accepted leaf's canonical value MUST fold identically
accept      = true    # accept/reject decision IS the contract
reject      = true

[contract.out]
wording     = true    # diagnostic MESSAGE text is free per language (V8 vs Rust phrasing)
span        = true    # byte/char span of a node is layout — free per language
layout      = true    # AoS/SoA, tape vs tree, field order — free per language

[contract.error_class]
# the ONE toggle: is the error CLASS (not its wording/span) in-contract?
in_contract = true    # e.g. "trailing-comma" vs "unexpected-eof" must agree; text need not

[canonical.number]
form        = "contract-normalized"   # NOT native f64 Display / Number.toString (Q1: DIVERGE)
significand = "shortest-round-trip"    # both langs agree here (proven)
exponent    = "never"                  # positional always; forbids V8's >=1e21 / <1e-6 switch
zero_sign   = "positive"               # -0 folds as +0  (Q1: Rust "-0" vs V8 "0")
alt_lane    = "ieee754-bits"           # bit fold; identical by construction; formatter-blind

[canonical.string]
units       = "utf16"                  # fold code units, not scalars (Q2 astral guard)
normalize   = "none"                   # NFC/NFD is a grammar decision, not the oracle's

[canonical.datetime]                    # example added-grammar class (Q3)
form        = "rfc3339-utc"
frac_zeros  = "trimmed"                 # contract-defined; native Displays diverge
```

The single genuinely open knob is `[canonical.number].zero_sign`: `JSON.parse("-0")` yields
`-0` with `Object.is(-0,0) === false` (probed, running) — so `-0` is *observable* in V8.
The contract chooses to fold it as `+0` (values-agree over bit-identity) UNLESS a downstream
consumer distinguishes them; that election is a LOCK-1 alphabet / LOCK-7 tie-breaker input,
which is why the DAG places this freeze strictly after both. The `error_class in_contract`
toggle is the second election: turning it on adds an accept/reject-plus-class column to every
expectation record (cheap, data-only) and is what lets the oracle catch a "dropped token
class" that still rejects for the wrong reason.

---

## Prior art (citations)

- **ECMA-262 §7.1.12.1 `Number::toString`** vs **Rust `core::fmt::float` (Grisu/Ryū
  positional)** — the exact source of the Q1 exponential/zero-sign divergence; both are
  shortest-round-trip (Steele & White / Ryū, Ulf Adams PLDI 2018) yet textually disagree.
- **Differential fuzzing**: libFuzzer/`cargo-fuzz` differential targets; simdjson vs
  RapidJSON conformance; rustc-vs-Miri; the "fold both, assert equal, minimize" shape.
- **JSONTestSuite (Nicolas Seriot)** — the canonical accept/reject corpus model; its
  `y_`/`n_`/`i_` (must-accept / must-reject / implementation-defined) partition maps directly
  onto `[contract]` accept/reject/error_class here.
- **WASM/Wasmtime spec-test `.wast` model** and **WPT (web-platform-tests)** — the
  checked-in-corpus-as-contract precedent; behavior contracted, implementation free.
- **PROVE-14** (in-tree) — the FNV-1a `u32` fold that already crosses Rust↔V8 byte-exactly
  on the JSON tape (320 cold children), proving the fold arithmetic is isomorphic; 6D lifts
  it from one grammar to a fleet contract.

---

## Named risks & counterexamples

- **R1 — canonicalization tax defeats the null hypothesis.** If most fleet grammars each
  introduce ≥1 new value class needing a contract-defined canonicalizer per language, Q3's
  K>0 per grammar and the "harness cost amortizes to zero" case weakens toward the
  skeleton-generation regimes. Counterexample to watch: a grammar whose value class is a
  bignum or a decimal-with-precision where Rust and JS have *no* common native type —
  forcing a bespoke canonicalizer pair. Mitigation: the `alt_lane = ieee754-bits` fold is
  the escape hatch for any IEEE-754 number; only non-float classes cost real code.
- **R2 — native-formatter regression re-enters silently.** A future author folds
  `format!("{}", x)` / `String(x)` for convenience; CI is green on the normal-range corpus
  and RED only when a ≥1e21 vector lands. Mitigation: the corpus MUST carry the boundary
  vectors (this pass's `corpus.txt` is the seed) so a false-formatter fails on day one;
  a grep-gate forbidding native float Display in oracle code is cheap.
- **R3 — astral/surrogate escape.** A scalar-iterating fold folds a surrogate-pair
  mishandle identically to correct handling. Counterexample: mishandled `😀` collapsing to
  a replacement char still yields the same *scalar count*. Mitigation: contract fixes
  `units = utf16` (proven catcher above).
- **R4 — order-sensitivity hides compensating errors only if resets are omitted.** The
  left-chain fold catches any single divergence; two divergences could theoretically cancel
  in a commutative hash. FNV-1a is non-commutative (order-dependent), so this is guarded —
  but the per-document reset for bisection must be preserved or localization degrades to O(N).
- **R5 — DAG inversion.** Freezing the number/zero-sign election before LOCK-1/LOCK-7 decide
  the alphabet/tie-breaker would bake a wrong `zero_sign`. The contract file header states
  the ordering; the freeze gate must assert LOCK-1 and LOCK-7 closed first.

---

## What a prototype must test to MOVE this family

1. **Contract-normalized decimal, both languages, folded equal.** Implement the
   `[canonical.number]` rule (shortest round-trip, positional-always, `+0`) in Rust and TS;
   fold the boundary corpus and assert the *native* lanes now AGREE — closing the Q1
   divergence by contract rather than by bit-fold. This is the one experiment that converts
   the negative Q1 result into a shipped mechanism.
2. **Fleet economy proof.** Wire the differential harness over TWO grammars (JSON in-tree +
   one added — CSV, K=0) and show the harness/fold code is byte-identical between them —
   the Q3 "zero marginal harness" claim made concrete.
3. **Fuzzer-promotes-to-corpus loop.** A `cargo-fuzz` differential target that mutates
   numbers toward the ECMA thresholds and strings toward surrogate boundaries, with the
   minimizer output auto-appended to the static corpus — measuring how many novel divergence
   classes a fuzzer finds beyond the hand corpus in a fixed budget (the Q2 second number,
   made empirical rather than reasoned).
4. **The `error_class` toggle, priced.** Add the accept/reject/class column to one grammar's
   expectation records and inject a "rejects for the wrong reason" mutant; confirm the
   class-on oracle catches it and the class-off oracle does not — pricing the toggle's
   real defect-catch delta against its data cost.

The running fold already settles the load-bearing question: **the fold is isomorphic, the
canonicalizers are not free, and the contract — not the native formatter — must own value
normalization.** That is 6D's strongest form: a grammar-agnostic differential harness whose
fixed cost amortizes to zero across the fleet, gated by a small checked-in canonicalizer set
whose sole hard case is IEEE-754 number text, for which a bit-fold escape hatch exists.
