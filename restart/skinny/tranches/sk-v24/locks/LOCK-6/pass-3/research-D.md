# LOCK-6 · Family 6D — ORACLE-ISOMORPHISM · Research (pass 3)

**The family's claim.** Rust and TS need share NO layout, NO module tree, NO name map — only
*behaviour*. A single checked-in conformance corpus plus a differential harness that folds each
side to a comparable digest is the entire isomorphism obligation; everything else (tape layout,
struct shapes, hidden-class discipline) stays idiomatic per language. The constitution admits
this only if it survives its own null hypothesis: it must be DEFEATED by measured
maintenance/drift cost, or it wins. This report makes the mechanism concrete, finds the one
genuine substrate gap, and states exactly what a prototype must fold.

---

## 1. The mechanism made concrete — a two-plane digest, not a byte mirror

Behavioural isomorphism reduces to: **for every input in a corpus C, fold both implementations
to a value-digest D and an outcome-tag O; assert D_rust == D_v8 and O_rust == O_v8.** Two planes:

- **Value plane** — a structural fold over the parsed document: order-sensitive FNV-1a chain
  over child hashes, XOR-accumulated members (order-insensitive), leaf hashes from typed
  primitives. This already exists on the TS side (below); it does not exist on the Rust side.
- **Outcome plane** — accept/reject and (contract-gated) error-*class*. Wording and span are
  explicitly OUT of contract (Q4).

The digest is the isomorphism boundary. Two implementations are "isomorphic to a reasonable
point" (Constitution iii) iff their digests coincide over C — nothing about their internal
representation is asserted. This is strictly weaker than 6A's name-map or 6B's generated
skeleton, and that weakness is the whole thesis: the maintenance object is a `corpus + one fold
per side`, not a mirrored tree.

---

## 2. The substrate — what exists, and the ONE thing that does not

The TS half of the value-plane fold already exists and is production-grade:

- Shared node-hash primitives — `nodeString/nodeNumber/nodeBool/nodeNull`, `memberMix`,
  `arrayStart/arrayStep/finishArray`, `finalizeDigest`, `digestsEqual` — all 32-bit
  `Math.imul(h ^ x, 0x01000193) >>> 0` FNV-1a
  (in-tree at `.claude/worktrees/wf_386900ed-462-14/skinny/crates/bbnf-bench/ts-tape/tape.mjs:351-403`).
- `walkTape` (over the SoA offset-tape) and `walkJs` (over `JSON.parse`) BOTH call those
  primitives, so identical structure ⇒ identical `fp` **by construction**. Measured:
  `fp 986912547` identical across 320 cold children, all 6 structural counts + codeUnits
  byte-exact (prover REPORT §"Byte-exact parity"; VERIFY CONFIRMED).

**The gap that defines this family's work: the tape-crossing proof was TS↔TS, not Rust↔V8.** The
Rust dumper computes **counts only** — `tape_dump.rs:44-46` emits a `meta.json` of
`offset_count/flag_count/payload_bytes` and NO fold. The 32-bit FNV prime `0x01000193 / 16777619`
appears **nowhere in skinny Rust** (grep: zero hits across `skinny/crates`). So the celebrated
`fp=986912547` is a V8-only artifact; the "Rust view" it was compared against is the
same-language `JSON.parse` lane. **6D's substrate is the missing Rust fold**: a ~40-LOC walk over
`runtime::Tape` (public planes at `skinny/crates/runtime/src/tape/mod.rs:126-140`:
`offsets/flag_cursors/flag_values/source` + `offset_at`) that reproduces those exact primitives
in Rust (`u32` `wrapping_mul(0x01000193)`, `f64::to_bits().to_le_bytes()` for numbers). Q1 is:
build it, run it, check `fp_rust == fp_v8` for a real corpus — a running instance, not spec text.

---

## 3. Q1 — the cross-language fold and its ONE load-bearing invariant

**The number-fold invariant is the crux.** The TS `numHash` folds the parsed IEEE-754 double's
raw 8 bytes: `_numDV.setFloat64(0, n, true)` where `n = Number(src.toString("latin1", start,
end))` (tape.mjs:43-44, 181, 226). It does NOT fold the decimal *text*. This is the correct design
and it yields a clean invariant:

> A float64-bit fold is bit-identical Rust↔V8 **iff both sides decimal→double convert with
> correct round-to-nearest-even.** Rust `f64::from_str` uses Eisel-Lemire with a slow-path
> fallback (rust-lang/rust#86761), correctly rounded for 100% of inputs; V8's `Number()` /
> `strtod` is correctly rounded (David-Gay-derived). "C's strtod, Lemire's, Wuffs', and Go's
> strconv.ParseFloat all agree on the f64 form over millions of unique cases." So VALID JSON
> numbers fold identically; a divergence can ONLY arise from a non-correctly-rounded parser —
> which is exactly the last-ULP bug Q2 wants to catch.

Critical corollary — **do NOT fold canonical decimal text**. Rust's canonical serialization keeps
the *raw source substring* for numbers: `JsonValue::Number(number) => number.raw()`
(`skinny/crates/runtime/src/grammars/json/value.rs:96`; `to_canonical_string` at :90). V8's
`Number.prototype.toString` re-emits shortest-round-trip (Grisu/Ryū-class). So `1.0`, `1e3`,
`10.00` fold IDENTICALLY as doubles but DIVERGE as canonical text (`"1.0"` vs `"1"`, `"1e3"` vs
`"1000"`). The contract must fold **the double**, not the string — otherwise every trailing zero
is a false positive. A concrete, checked counterexample, not a hypothetical.

**String fold — the astral hazard (real, present in the current primitives).** The TS clean-path
string fold runs `Math.imul(h ^ s.charCodeAt(i), 0x01000193)` over UTF-16 code units
(tape.mjs:37, 357), and escaped `\uXXXX` decodes via `String.fromCharCode` to UTF-16 units,
matching `JSON.parse`. A naïve Rust fold over `source` **UTF-8 bytes** would diverge on every
non-ASCII string: an astral codepoint is 4 UTF-8 bytes in Rust but 2 UTF-16 surrogate units in
V8. The tape-crossing proof dodged this by folding *codeUnits* and only proving the ASCII/escaped
minority through the flag channel; a genuine Rust fold MUST iterate `char::encode_utf16` (fold
`u16` units). **This is the single most likely place the Rust fold breaks on first run** — name
it, test it first.

**Q1 prototype (~1 hr):** add `tape_digest.rs` beside `tape_dump.rs` (autodiscovered
`src/bin/*.rs`, no Cargo edit — the established precedent) that walks `runtime::Tape` with the
mirror primitives and prints `{fp, objects, ..., codeUnits}`; extend `verify.mjs` to read it
alongside the V8 `fp`. Gate: `fp_rust == fp_v8` over twitter.json + ≥1 non-ASCII-heavy corpus
(to arm the astral path). Expected: structure/bools/nulls PASS immediately; numbers PASS iff
folding doubles; strings PASS iff folding UTF-16 units. Any FAIL is a *found divergence* — the
harness working.

---

## 4. Q2 — divergence injection: static corpus vs coverage-guided fuzzer

**Setup.** Take the passing Rust↔V8 fold (§3) and inject five seeded divergences, one at a time,
into ONE side:

| # | divergence | plane tripped | where it hides |
|---|---|---|---|
| a | off-by-one escape decode (`\n`→wrong unit) | value (string fp) | only escaped strings (312/18099 in twitter = 1.7%) |
| b | dropped token class (skip `null`) | value (count + fp) | any doc with that class |
| c | last-ULP number delta (non-round-nearest) | value (number fp) | only a decimal near a tie |
| d | astral mishandle (surrogate split) | value (string fp) | only astral-bearing strings |
| e | error-class flip (reject→wrong class) | outcome tag | only malformed inputs |

**The honest prediction the prototype must confirm.** A *static* corpus (JSONTestSuite's ~318
`y_/n_/i_` files — Seriot, "Parsing JSON is a Minefield") catches **b and e near-instantly**
(every file exercises token classes + the accept/reject boundary) but is **structurally blind to
a, c, d unless a file happens to contain the triggering escape/decimal/astral** — JSONTestSuite
has such files, so coverage is *incidental*, not guaranteed, and yields no *cycles-to-detection*
number (a fixed one-pass sweep, ~ms). A *coverage-guided differential fuzzer* (cargo-fuzz/
libFuzzer feeding both folds, mutation-guided on Rust-side coverage) catches **all five** and
yields a real cycles-to-detection distribution: b/e in the first few hundred execs (dense
triggers); a/d within ~10³–10⁴ execs once the mutator discovers `\` / high bytes; **c (last-ULP)
is the hard case** — a blind mutator rarely lands a tie-breaking decimal, so a *grammar-aware*
number generator (decimals near representable ties, à la Nigel Tao's float vectors / Hrvoje
Abraham `strtod_tests`) is required, else c runs 10⁶+ execs undetected. This is exactly the SOTA
finding: without coverage guidance differential testing "repeatedly exercises the same paths while
missing bugs in less-frequent regions" (AsiaCCS 2024, *Cross-Language Differential Testing of JSON
Parsers*).

**Deliverable both numbers require:** a `fuzz/` target (cargo-fuzz `fuzz_target!` folding Rust
`parse_tape` and a persistent-node V8 fold) + the static JSONTestSuite sweep, run to catch each
seeded bug, reporting {caught?, execs-to-first-diff} per divergence per method. Prediction to
falsify: static catches 2/5 deterministically + ~1–2/5 incidentally; fuzzer catches 5/5 but needs
a number-vector seed for c.

---

## 5. Q3 — marginal maintenance cost per ADDED grammar

Under 6D *alone*, adding a grammar not yet in-tree (TOML — "no TOML exists anywhere"; authored at
GN-5.W2) costs, on the isomorphism plane, exactly:

1. **One value-fold specialization per side** IF the grammar's value shape is NOT already covered
   by the JSON-family primitives. TOML's document is an object-of-tables → the SAME object/array/
   string/number/bool primitives + a *datetime* leaf. Marginal fold cost: **one new leaf hash
   (`nodeDateTime`) each side** (~5 LOC Rust + 5 LOC TS); object/array/scalar machinery reused
   verbatim. Grammars whose values are all JSON-scalars (CSV rows, sheets cells) add **zero** new
   primitives.
2. **Corpus rows** — N conformance inputs (accept + reject + error-class cases). Data, not code;
   the dominant recurring cost, linear in grammar *surface*, not in implementation.
3. **Zero** module-tree/name-map maintenance (that is 6A's cost, explicitly not incurred here) —
   the decisive economy and the answer to the null hypothesis. Per-grammar cost is
   `max(1 new leaf primitive, 0) + corpus rows`, versus a mirror-tree's per-name drift on every
   rename. **Break-even vs 6A: 6D wins whenever grammars are added faster than the leaf-primitive
   set grows** — and the leaf set is bounded by the value fabric's one `Value` enum (the four
   proven lanes share ONE surface, GN-4.W5), so it saturates.

Named risk: if a grammar's *outcome* semantics are context-dependent (error class depends on prior
state), the error-class contract data grows non-trivially — but into the corpus, still not into a
mirrored tree.

---

## 6. Q4 — the in/out-of-contract partition as a checked-in data file

The contract is a checked-in artifact, not prose. Proposed `oracle/contract.toml` (colocated
beside the corpus it governs):

```toml
# LOCK-6 · 6D oracle contract — what the differential harness asserts.
# Freezes strictly AFTER LOCK-1 (alphabet) and LOCK-7 (tie-breaker): see [dag].
[value]                 # IN CONTRACT — must fold bit-identically Rust↔V8
in  = ["structure",     # object/array nesting, member order-insensitivity
       "accept_reject", # the boolean outcome
       "number_f64",    # the parsed IEEE-754 double's bits — NOT the decimal text
       "string_utf16",  # decoded UTF-16 code units — NOT source UTF-8 bytes
       "bool", "null"]
[wording]               # OUT OF CONTRACT — free per language
out = ["error_message", # human wording diverges by design
       "span",          # byte offsets / line-col are layout, not behaviour
       "canonical_text",# "1.0" vs "1" — fold the double, never the serialization
       "tape_layout",   # SoA vs AoS vs V8 heap object — invisible to the fold
       "diagnostics_suggestions"]
[error_class]           # TOGGLE — in-contract only when both sides opt in
enabled = false         # default OFF; a grammar sets true to assert class parity
classes = ["unexpected_token", "unexpected_eof", "invalid_number",
           "invalid_escape", "trailing_content"]  # shared taxonomy when ON
[dag]
freezes_after = ["LOCK-1", "LOCK-7"]
# LOCK-1 fixes the ALPHABET (which byte classes exist) — the fold's token taxonomy is
#   meaningless until the alphabet is frozen; a class added later is a corpus edit.
# LOCK-7 fixes the TIE-BREAKER (ambiguity / accept-reject boundary) — the accept_reject
#   bit is only well-defined once the tie-breaker is law; injecting the error-class flip
#   (Q2.e) before LOCK-7 would measure the tie-breaker, not a bug.
```

The DAG carry is load-bearing: `accept_reject` and the `error_class` taxonomy are ILL-DEFINED
until LOCK-1's alphabet and LOCK-7's tie-breaker freeze, so 6D's corpus cannot finalize before
those close — the contract file records that dependency as data. Wording/span being OUT is what
makes the corpus portable: it encodes no implementation's byte geometry. (This is also why the
tape-crossing *full-walk* full-materialization is a non-goal — native parse won it, measured
0.91×; the contract asserts the *digest*, which stop-early get delivers at 4.4×, so the fold is
priced on the lane that wins.)

---

## 7. Integration seams (path:line)

- **Rust fold home** — `skinny/crates/runtime/src/tape/mod.rs:126-140` (`offsets`, `flag_cursors`,
  `flag_values`, `source`, `offset_at`) are already public; the fold is a pure consumer, zero
  runtime change (export shape proven by `tape_dump.rs`).
- **Number-raw seam** — `skinny/crates/runtime/src/grammars/json/value.rs:96` (`number.raw()`):
  the fold must NOT route through this; it must `f64::from_str(raw)` then `to_bits()`.
- **Existing 64-bit FNV in-tree** — `.../grammars/css_l4_declaration_values/generated.rs:767-771`
  folds `input.as_bytes()` with `0x100000001b3` for a *source* fingerprint header — NOT the value
  fold, and a different (64-bit) prime. The 6D value fold is a distinct, additive primitive set;
  do not conflate. (Proof, too, that codegen already emits FNV folds — a grammar-derived digest
  emitter is within reach for a later gestalt, but is NOT required for 6D.)
- **TS fold home** — `ts-tape/tape.mjs:351-403` verbatim; Rust mirrors these constants.
- **Corpus/contract home** — new `oracle/` directory-module colocated with the bench crate that
  feeds it (the `ts-tape/` sibling precedent).
- **Harness** — extend `ts-tape/verify.mjs` to read the Rust `fp`.

---

## 8. Prior art (citations)

- **Cross-language differential JSON testing** — *Cross-Language Differential Testing of JSON
  Parsers*, ACM AsiaCCS 2024 (dl.acm.org/doi/10.1145/3634737.3657003): the exact discipline;
  documents that un-guided differential testing misses low-frequency regions → coverage guidance
  needed (Q2 anchor).
- **Static conformance corpus** — N. Seriot, *Parsing JSON is a Minefield* / JSONTestSuite
  (`y_/n_/i_` files): the canonical static differential corpus; the "catches classes+boundary,
  incidental on escapes/ULP/astral" prediction is calibrated to its known coverage.
- **Correctly-rounded decimal→double** — Rust Eisel-Lemire adoption (rust-lang/rust#86761,
  Alexhuszagh, from fast-float-rust); Nigel Tao, *The Eisel-Lemire ParseNumberF64 Algorithm*
  (nigeltao.github.io/blog/2020/eisel-lemire.html): "strtod, Lemire, Wuffs, Go's ParseFloat all
  agree on the f64 form over millions of cases" — basis for the §3 number-fold invariant. ULP
  fuzzer seed: Nigel Tao's float tests + Hrvoje Abraham `strtod_tests`.
- **Coverage-guided differential fuzzing** — cargo-fuzz/libFuzzer; USENIX Sec 2024, *Data Coverage
  for Guided Fuzzing*; GraphFuzz (arXiv 2502.15160) as the differential-fuzz-with-lightweight-
  feedback template. tree-sitter corpus tests and rustc/GHC differential-oracle practice are the
  compiler-side analogues.

---

## 9. Named risks & counterexamples

1. **Canonical-text false positives** — folding `to_canonical_string()` instead of the double
   makes `1.0`/`1e0`/`10e-1` all fire divergences. MUST fold the double. (§3, checked.)
2. **Astral UTF-8-vs-UTF-16 divergence** — a Rust byte-fold vs V8 unit-fold diverges on every
   non-ASCII string. MUST fold UTF-16 units both sides. (§3 — the first-run breakage.)
3. **Array vs object order-sensitivity** — objects XOR-accumulate (order-insensitive, `memberMix`)
   but arrays FNV-chain (order-SENSITIVE, `arrayStep`). Object member-order divergence is
   correctly invisible (JSON objects unordered); array-order divergence IS caught — verify the
   Rust walk emits array children in source order.
4. **`-0.0`** — `to_bits(-0.0) != to_bits(0.0)`; NaN/Inf are JSON-illegal so never appear, but
   `-0` is legal; both sides parse it to `0x8000000000000000` (they do). Add a corpus row.
5. **The null hypothesis is real** — if a future grammar needs error-class parity ON *and* the
   class taxonomy is context-dependent, 6D's corpus cost approaches 6B's generation cost; the
   `error_class.enabled=false` default is the release valve. Measure before asserting 6D wins
   universally.

---

## 10. What a prototype must test to move this family

1. **Build the missing Rust fold** (`tape_digest.rs`, ~40 LOC, autodiscovered) and prove
   `fp_rust == fp_v8` on twitter.json + one astral-heavy corpus. This is Q1 and the whole
   substrate — until it runs, 6D has only a TS↔TS proof, not a Rust↔V8 one.
2. **Inject a–e, measure catch-fraction and execs-to-detection** for the JSONTestSuite static
   sweep vs a cargo-fuzz differential target with a grammar-aware number-vector seed. Both numbers,
   per divergence. (Q2.)
3. **Add TOML** end-to-end through the fold; record the marginal delta (predicted: one
   `nodeDateTime` leaf per side + corpus rows, zero tree maintenance). (Q3.)
4. **Check in `oracle/contract.toml`** and wire the harness to READ it (in/out/toggle honored),
   with the LOCK-1/LOCK-7 DAG dependency asserted. (Q4.)
5. **Falsification target:** if the Rust fold cannot be made bit-identical to V8 without
   per-grammar special-casing beyond the shared leaf set, 6D's economy claim is defeated and the
   family loses to 6B — that is the measured verdict the null hypothesis demands.
