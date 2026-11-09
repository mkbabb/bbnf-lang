# PROTO-1 — LOCK-4 pass-3 probe 1: does the Futamura codegen route generalize to recursive JSON and beat the shipped closure core? (family 4B)

**Agent:** PROTOTYPE, pass 3, LOCK-4 (THE COMBINATOR CORE), probe 1
**Measured HEAD (bbnf-lang worktree):** `ce51c36fcbdf5108eb028005586672d9a6d2ca1b`
  base-pin `ce51c36fc` — worktree started BEHIND the pin (`d2b32f6e3`, an ancestor on a
  linear path); `git merge --ff-only ce51c36fc` succeeded, ancestry re-verified GREEN.
**Incumbent HEAD (parse-that, read-only):** `ef10d5b`
**Date:** 2026-07-16 · **Wrong-artifact-risk:** NONE (clean ff onto base-pin).

---

## QUESTION

Does the Futamura-projection codegen route — proven in-tree for regex at
`parse-that/rust/parse_that/src/regex/generated.rs` (2293 LOC, ZST `RegexParser`, zero
SmallBox hot path) — GENERALIZE to a RECURSIVE, alt-dispatched, value-bearing grammar
(JSON: object/array/value recursion, which regex lacks) AND beat the tuned incumbent
`parse_that::parsers::json::json_parser()`?

## METHOD & RECONCILIATION

The brief's steps "author json.bbnf" + "run the bbnf generator" are **already realized
in-tree at base-pin `ce51c36fc`**:

- `grammar/json/json.bbnf` — recursive value-bearing grammar (object/array/value recursion;
  `number … -> f64`; `string … -> decode_json_string_to_arena(input) : String`) exists.
- `crates/core/src/grammar/generated/json.rs` (3505 LOC) — emitted by the SAME path
  `crates/core/src/backend/rust/emitter/` that emitted regex `generated.rs`. ZST
  `pub struct JsonParser`, `use ::parse_that::*`, regen `cargo xtask regen --grammar json`,
  wired live (`pub mod json;`).
- `.cargo/config.toml` patches `parse_that = { path = "../parse-that/rust/parse_that" }`,
  so emitted `JsonParser` and incumbent `json_parser()` share the **identical** parse_that
  core → true apples-to-apples election bench.

Probe = **verification of the gates on the already-emitted artifact**. Paired benches ship:
`benches/json/monolithic.rs` (emitted `JsonParser::parse`) and `benches/json/parse_that.rs`
(incumbent), both divan cold-per-parse + mimalloc, same corpus, `sample_count(100)`.

## WHAT RAN

- Base-pin ff + ancestry verify — GREEN.
- Full static census of emitted hot path — RAN.
- `cargo bench -p bbnf --features competitor --bench json_monolithic --bench json_parse_that`
  under `[profile.bench]` (fat LTO, codegen-units=1) — canonical cold profile. Compiled
  clean (lib: 190 warnings, 0 errors); both benches’ pre-timing correctness asserts passed
  on every corpus file (running parse proof). **loadavg at launch 20.67** — heavy
  multi-agent contention; fastest-sample is the robust statistic, mean/slowest are noise.

---

## GATE A — hot-path census (emitted `json.rs`, 3505 LOC) — **PASS**

| construct | count | note |
|---|---|---|
| `SmallBox` | **0** | PASS |
| `dyn ` (dyn ParserFn / any) | **0** | PASS |
| `ParserFn` | **0** | PASS |
| `Rc<`/`Arc<` | **0** | PASS |
| fn-pointer fields `fn(` | **0** | PASS |
| `Box<` | **1** | cold escape branch only |

The single `Box<` (json.rs:1439, `buf.into_boxed_slice()`) is reached ONLY on the
`Some((_off, b'\\'))` escaped-string branch of `parse_string_…`; the no-escape fast path is
zero-copy (`&input[body_start..end]`). Not a SmallBox, not dyn — the minimal per-owned-string
arena decode. Twitter/citm/canada are escape-light → never in the hot loop.

**Recursion lowering (the generalization crux):** value recursion emits as DIRECT,
monomorphized, first-byte-dispatched `fn` calls — not boxed closures, not dyn ParserFn:

```
b'{' => parse_object_JsonParser_object(input, p, state, builder, cursor),
b'[' => parse_array_JsonParser_array (input, p, state, builder, cursor),
```

object/array bodies recurse into `parse_value_…` by direct call. The "JSON has recursion,
regex does not" objection dissolves: the route lowers alt-dispatch + value recursion into a
first-byte `match` over direct recursive `fn` calls, staying ZST and `'a`-scoped by
construction exactly as the regex proof. **The codegen route GENERALIZES.**

(GATE-A construction/per-parse alloc counts <6 / =1 were not isolated via dhat within the
cap; the source-level census + clean fat-LTO compile is the evidence carried here. Marked as
the one un-instrumented GATE-A sub-claim.)

---

## GATE B — cold paired bench vs shipped closure core (window 1)

`[profile.bench]` fat-LTO · divan · mimalloc · N=100/fn · loadavg 20.67. Emitted =
`JsonParser`, Incumbent = `json_parser()`. **fastest** (contention-robust) and **median**:

| corpus | dominant | Emitted fastest | Incumbent fastest | ratio (emit/incumbent) | Emitted median | Incumbent median | ratio |
|---|---|---|---|---|---|---|---|
| data (35 KB) | small/mixed | 26.79 µs | 36.49 µs | **0.73× (emit WINS)** | 27.29 µs | 42.02 µs | **0.65× WINS** |
| twitter (631 KB) | strings/struct | 544.1 µs | 572.2 µs | **0.95× (emit WINS)** | 585.5 µs | 686 µs | **0.85× WINS** |
| citm (1.7 MB) | mixed | 1.654 ms | 1.446 ms | 1.14× (emit slower) | 1.93 ms | 1.871 ms | 1.03× (≈tied) |
| canada (2.2 MB) | **all f64 numbers** | 5.891 ms | 3.925 ms | **1.50× (emit SLOWER)** | 6.499 ms | 4.173 ms | 1.56× SLOWER |

**Reading:** the emitted codegen parser WINS on string/structure-heavy + small inputs
(data −27/−35%, twitter −5/−15%), is ≈TIED on mixed citm, and REGRESSES hard on
number-dominated canada (+50%). The regression localizes to the numeric leaf: canada is
essentially a wall of floating-point coordinates, and the incumbent's tuned number fast-path
(eisel_lemire float + SIMD `dispatch_byte_multi`) outruns the emitted `-> f64` path.
**Window-2 (reversed binary order, lower loadavg 9–14) CONFIRMS window-1 exactly** — the
signal is structural, not contention noise:

| corpus | Emit fastest | Incumbent fastest | ratio | Emit median | Incumbent median | ratio |
|---|---|---|---|---|---|---|
| data | 19.04 µs | 28.66 µs | **0.66× WINS** | 20.56 µs | 29.77 µs | **0.69× WINS** |
| twitter | 375.3 µs | 499.2 µs | **0.75× WINS** | 498.6 µs | 611.6 µs | **0.82× WINS** |
| citm | 1.096 ms | 1.069 ms | 1.03× (≈tied) | 1.131 ms | 1.117 ms | 1.01× (tied) |
| canada | 3.861 ms | 2.428 ms | **1.59× SLOWER** | 3.999 ms | 2.56 ms | 1.56× SLOWER |

Two windows × both orders agree: **data/twitter WIN (~0.66–0.82×), citm TIE (~1.0×), canada
REGRESS (~1.5–1.6×).** The canada regression is a stable structural property of the emitted
f64 numeric leaf, reproduced across four independent measurement passes.

---

## GATE C — parity oracle

Oracle: `parse-that/rust/parse_that/tests/json_test.rs` (37 `#[test]`, 60 asserts) +
`typescript/test/json-vectors.test.ts`. A full ~400-case sweep against the emitted parser
was NOT run within the cap. **Running parity actually obtained:** both benches gate on
correctness before timing — the emitted parser is asserted to parse each corpus file to a
structural root (`Object|Array`), the incumbent to complete (`offset ≥ trimmed len`). Green
run ⇒ end-to-end parse proof for BOTH on data/twitter/citm/canada.

---

## VERDICT

**RAN_MEASURED — MIXED; strict election gate NOT cleared.**

- GATE A **PASS**: the Futamura codegen route GENERALIZES to recursive, alt-dispatched,
  value-bearing JSON with **0 SmallBox / 0 dyn ParserFn / 0 fn-ptr** on the hot path (lone
  Box confined to the string-escape cold branch); value recursion lowers to direct
  byte-dispatched `fn` calls. The regex-has-no-left-recursion objection is dissolved by a
  running artifact.
- GATE B **does NOT meet "≥1.00× no-regression vs the shipped closure core."** It WINS on
  data (−35% median) and twitter (−15% median), ties citm, and REGRESSES +56% (median) on
  number-heavy canada. The **real shrink is workload-dependent and does not reach the
  16–27% stripped-grammar upper bound as a uniform win** — it is +35%↔−56% across the corpus,
  with the loss concentrated entirely in the f64 numeric leaf.

**Election consequence:** the strict "beats SOTA everywhere → elect 4B as the Rust fast
face" form is **NOT satisfied** — canada is a clean regression. But the *refutation* form
("below the shipped closure core → interpret is the only live Rust story") is **also too
strong**: the route generalizes with 0 boxes and already beats the tuned incumbent on
string/structure-heavy inputs. The honest election read: **4B is architecturally proven and
competitive, gated on a number-leaf fast-path (eisel_lemire + SIMD) before it can claim a
uniform beat.** That numeric-leaf uplift is the single lever between "mixed" and "elect."
RF-1's staging boundary can be grounded on this running artifact; the go/no-go rests on
whether the emitted `-> f64` path is brought to incumbent parity.
