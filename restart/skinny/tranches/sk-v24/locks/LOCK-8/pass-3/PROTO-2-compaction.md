# LOCK-8 · pass-3 · PROTO probe 2 — Cross-language undefined-compaction (8B route-1 vs 8C ledger)

**Plane:** THE TS BACKBONE PLANE (parse-that/typescript) · families **8B + 8C** · probe 2
**Verdict line:** **RAN_MEASURED — DIVERGE. Rust seq keeps a FIXED-ARITY tuple (arity 3, empty slot present); TS `all` COMPACTS undefined-yielding arms (length 2). Lawful non-mirror.**

> File named `PROTO-2-compaction.md` (not `PROTO-2.md`) to avoid clobbering the pre-existing
> `PROTO-2.md` (8C priority-DFA necessity, a different probe). Follows this dir's family-tagged
> convention (cf. `PROTO-8BD.patch`).

---

## Question (exact)

TS `fuseAll` compacts undefined-yielding arms
(`if (state.value !== undefined) out[w++] = state.value; … if (w !== n) out.length = w;`,
`leaf.ts:209/248/270`): `all(string('a'), whitespace, string('b'))` **TYPES** as
`Parser<[string,string,string]>` but **YIELDS** `['a','b']` (length 2) when `whitespace`
(`regex(/\s*/)`) empty-matches. **Does the Rust core compact identically, or produce a FIXED-ARITY
tuple keeping the empty/unit slot?**

This decides:
- **(a)** whether 8B route-1's compaction caveat is a *private within-TS note* or a *Rust↔TS
  mirror-DRIFT row* the 8C ledger + 6D corpus must carry; and
- **(b)** closes the incompatibility "**8B undefined-compaction vs 8C fixed-arity mirror**".

## Method (both engines, base-pin bbnf-lang HEAD `1a0d3091c`; parse-that `rust`/`regex` read-only)

parse-that untouched. Rust half built on a scratch copy of `parse-that/rust/` per
`research/R3-parse-that-rust.md` §8 (rsync; the 3 external patch prefixes absolutized in
`.cargo/config.toml`; `CARGO_TARGET_DIR` inside the copy). A one-line `probe8` workspace member was
added so **only `parse_that`'s library deps compile** — its heavy dev-deps (lightningcss, simd-json,
…) are skipped. The **only** edit to the copied tree is that `+probe8` members line.

**Source (dispositive) — both flat-tuple builders the brief named:**
- `seq!` (`combinators/macros.rs:14`) expands the 3-ary arm to `Some((v1, v2, v3))` — a
  statically-sized 3-tuple. Arity = the count of `$pN` arms; no runtime path drops a slot.
- `impl_parser_flat!` (`span_trait.rs:184-220`) `then` destructures `($($T,)*)` and appends `last`
  → `($($T,)* Last)` — the tuple grows by exactly one statically-typed slot per composition; a
  successful empty/unit arm still fills its slot.

Rust mirror of TS `whitespace` (which *succeeds* on zero whitespace, yielding `undefined`):
`string(" ").opt()` — on `"ab"` the middle arm empty-matches, consumes nothing, and yields
`None` (a successful unit/empty arm; cf. `combinator_test.rs:185`
`string("x").opt().parse("abc") == Some(None)`).

**(1) Rust — RAN.** `probe8/src/main.rs`:
`ParserFlat::then(string("a").then(string(" ").opt()), string("b")).parse("ab")`.
Arity read off the **concrete runtime type** via `type_name_of_val` (reflected, not asserted); the
value is also destructured into exactly three bindings (compiles only at arity 3).

**(2) TS — RAN.** `ts-probe8.mjs`, importing the built `dist/core.js`:
`all(string('a'), whitespace, string('b')).parse('ab')`; assert `.length`.

### seq! footnote (does not affect the answer)
The `seq!` macro's inline `move |state| …` closure fails rustc higher-ranked-lifetime **inference**
at *every external callsite* — `implementation of \`Fn\` is not general enough` — independent of
owned-vs-borrowed arm outputs, an explicit `Parser<…>` type ascription, or wrapping in a
`'static`-returning fn (all three tried; all rejected). It is likewise never instantiated in
parse_that's own tests/benches (zero uses outside its definition). This is orthogonal to arity:
`seq!`'s 3-arity is manifest in source as `Some((v1,v2,v3))`. Runtime confirmation therefore uses
`impl_parser_flat!`/`ParserFlat::then` — the sibling flat-tuple builder, which is the *same*
static-product construction and compiles cleanly.

## What ran — the numbers

Rust (`evidence-compaction-rust.out`):
```
RUST_OUTPUT_TYPE   = (&str, core::option::Option<&str>, &str)
RUST_SEQ_RESULT    = ("a", None, "b")
RUST_MIDDLE_ARM    = None            (whitespace empty-matched; slot KEPT)
RUST_TUPLE_ARITY   = 3
```

TS (`evidence-compaction-ts.out`):
```
TS_STATIC_TYPE  = Parser<[string, string, string]>   (declared arity 3)
TS_VALUE        = ["a","b"]
TS_VALUE_LENGTH = 2
```

| Engine | Static/declared arity | **Runtime yield** | empty middle arm |
|---|---|---|---|
| Rust (`seq!` / `impl_parser_flat!`) | 3 | **3** — `("a", None, "b")` | **KEPT** as `None` |
| TS (`all` / fuseAll) | `[string,string,string]` = 3 | **2** — `["a","b"]` | **COMPACTED OUT** |

(No perf claim is made — this is a pure arity/semantics determination; no timing measured.
loadavg at Rust build was `3.32 6.74 7.34`.)

## Verdict (numeric PASS/FAIL)

Rust tuple arity ∈ {2,3}. Measured **Rust == 3** (fixed, slot kept) while **TS == 2** (compacted)
⟹ **DIVERGE.**

Consequences (per the brief's fork):
- The **8C `LEDGER.tsv` gains a `LAWFUL-NON-MIRROR(compaction)` / DRIFT row**: the two engines' seq
  *type* agrees (arity 3) but their *runtime container* does not — Rust's tuple is a fixed static
  product (no element can be absent; an empty arm occupies its slot with `()`/`""`/`None`), whereas
  TS's `all` yields a **dynamic array** that fuseAll compacts, dropping every `undefined`-yielding
  arm. This is structural, not incidental: a statically-sized tuple has no representation for a
  "missing" slot, so compaction is impossible in Rust by construction; TS chose length-mutating
  compaction as a hot-path shape optimization (`out.length = w`).
- **8B route-1's compaction caveat is PUBLIC**, not a private within-TS note: it is a genuine
  Rust↔TS mirror divergence. The **6D corpus must carry `all(a, ε, b)`** (a sequence with an
  empty/undefined-yielding middle arm) as a differential fixture.
- The incompatibility "**8B undefined-compaction vs 8C fixed-arity mirror**" **does not close as a
  non-issue** — it closes as a *documented lawful non-mirror* the ledger and corpus must encode.

**STRUCTURED verdict:** `RAN_MEASURED` · numbers `Rust_arity=3`, `TS_length=2` · DIVERGE.

## Deliverables
- `PROTO-2-compaction.md` (this file)
- `PROTO-2-compaction.patch` — probe sources: `probe8/Cargo.toml`, `probe8/src/main.rs`, the
  `+probe8` workspace-member line, `ts-probe8.mjs`
- `ts-probe8.mjs` — TS half (runnable)
- `evidence-compaction-rust.out`, `evidence-compaction-ts.out` — raw runtime output
