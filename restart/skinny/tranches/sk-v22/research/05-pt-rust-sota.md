# SK-V22 R4a — parse-that Rust SOTA audit

Scope (charter correction 4): the Rust side of parse-that is IN scope for uplift — a full SOTA
audit of the combinator crate and the bespoke regex engine, plus a legacy/deprecated census, with
consumer-safety gates. Repo: `~/Programming/parse-that`, `rust/` frozen at `051a6d6` (2026-05-02).
No builds run. Evidence is file:line. Web-SOTA anchors cited inline.

Workspace: `rust/Cargo.toml` — four members: `src` (a thin bin), `parse_that` (0.4.0, combinators
+ scanners, 2,371 LOC src), `bootstrap` (62 LOC thin re-export), `regex` (bbnf-regex 0.1.0, the
6,742-LOC HIR→NFA→DFA engine).

---

## 0. The headline: nightly is FOUR gates deep, not one

The charter frames `smallbox[nightly]` (`rust/parse_that/Cargo.toml:97`) as the stable blocker
(E0554). It is not the only one, nor the deepest. `rust/parse_that/src/lib.rs:1-3` pins three
compiler features directly:

```
#![feature(portable_simd)]          // std::simd — used in scanners.rs + 9 scan/*.rs files
#![feature(fmt_helpers_for_derive)] // pprint derive internal
#![feature(stmt_expr_attributes)]   // #[cfg] on expressions in SIMD loops
```

`smallbox[nightly]` is a fourth. **The crate cannot compile on stable regardless of smallbox** —
E0554 fires from `portable_simd` first. Any "de-nightly" that only touches smallbox ships a crate
that still requires a nightly toolchain. The real anchor is `portable_simd`: 12 files load
`std::simd` (`scanners.rs`, `parsers/scan/{structural_bitmap,quoted_simd,decode,balanced,
ws_comment,quote_parity}.rs`, `span_parser/{mod,scan}.rs`). Per web-SOTA (Feb 2026), portable_simd
is *still* nightly-only; stabilization is progressing but unshipped
([pythonspeed](https://pythonspeed.com/articles/simd-stable-rust/),
[std::simd docs](https://doc.rust-lang.org/std/simd/index.html)).

**The transposition already exists in the skinny tree.** skinny replaced `std::simd` with two
crates that use stable `core::arch::aarch64` NEON intrinsics (stable since Rust 1.61):
`skinny/crates/bbnf-simd` (2,324 LOC; `aarch64/{movemask,bracket_depth_mask_64,string_block,
match_tiny_plain_string,unescape_uxxxx}.rs` + a `utf8/validate_block.rs`) and `crates/simd-scan`.
The stable-portable alternatives for the non-NEON path are `wide` and `pulp`
([SIMD-in-Rust-2025](https://shnatsel.medium.com/the-state-of-simd-in-rust-in-2025-32c263e5f53d)).
The de-nightly is therefore a *transposition to the better design already prototyped*, not a strip:
adopt `bbnf-simd` as the SIMD façade, delete `#![feature(portable_simd)]`. `fmt_helpers_for_derive`
falls out with a pprint-derive bump; `stmt_expr_attributes` is cosmetic (hoist the `#[cfg]` off the
expression). smallbox falls out with §2's arena transposition.

---

## 1. The combinator crate: allocation census + smallbox adjudication

**Construction-time, per combinator node.** Every combinator — `then`/`skip`/`next`/`wrap`/`trim`
(`combinators/methods/sequence.rs`), `or`/`chain` (`alternation.rs`), `map` (`map.rs:10`),
`many`/`sep_by` (`repetition.rs`, `sep_by.rs`), `opt` (`methods/mod.rs:18`) — closes over its
child `Parser`s and wraps the closure in `Parser::new` → `SmallBox::new(...)` →
`SmallBox<dyn ParserFn, S32>` (`parse.rs:62,73`). Each node is one boxed closure.

**The smallbox S32 inline optimization is defeated for combinator nodes.** A `Parser` is
`SmallBox<dyn…, S32>` + `u8` flags ≈ 40 bytes. A `then` closure captures *two* Parsers ≈ 80 bytes,
which exceeds the S32 (32-byte) inline budget, so SmallBox spills to the heap — identical to a plain
`Box`. S32 inlining only ever helps *leaf* closures that capture a couple of pointers
(`string`/`regex` in `leaf.rs`). So the nightly `smallbox` dependency buys inline storage for
leaves while combinator trees — the bulk of any grammar — heap-allocate one box per node anyway.
**Adjudication: smallbox is a net-negative — it costs a nightly gate for a win that evaporates on
the hot shape.**

The idiomatic stable transpositions, ranked:

- **(A) Arena the parser tree into the existing `BumpSlab`.** `bump_slab.rs` already implements a
  polymorphic byte bump allocator (`alloc<T>`, `alloc_slice_copy`). Parsers are built once and
  reused; a whole grammar's parser tree can bump-allocate into one arena and hand out
  `&'arena dyn ParserFn`. Zero per-node heap allocs, stable, DRY with an existing subsystem. This
  is the elegant answer and it eliminates smallbox outright.
- **(B) Plain `Box<dyn ParserFn>`.** Stable, one alloc/node — behaviourally identical to the S32
  spill path already taken, minus a nightly feature. The zero-cost fallback if (A) is scoped out.
- **(C) enum-dispatch `Parser`.** A `Parser` enum (`String`/`Regex`/`Seq`/`Alt`/`Many`/…)
  monomorphizes and removes boxing entirely, but forecloses arbitrary user closures — a semver
  break for any consumer passing `Parser::new(|s| …)`. Reject for the public combinator surface;
  viable only inside a codegen backend.

**Other allocation sites.** `one_of`/`dispatch_byte`/`dispatch_byte_multi` allocate a
`Vec<Parser>` at construction (`leaf.rs:298,315,340`); `many`/`sep_by` allocate `Vec<Output>`
per-parse; `take_until_any_span` boxes a `[bool;256]` LUT on the >3-target path (`leaf.rs:203`).
Under the `diagnostics` feature, `string`/`regex`/`take_until`/`dispatch` labels are minted with
`Box::leak(format!(…))` (`leaf.rs:41,59,88,105,215,273,323,357`) — a permanent leak that grows
unbounded if parsers are constructed in a loop. Gate: `diagnostics` is off by default, so this is a
latent hazard, not a hot cost; the fix is a `&'static` interner or deferring label materialization
to the error path.

**Per-parse allocation: the padded buffer.** `ParserState::new` allocates a fresh 64-byte-aligned
`Vec<PaddedChunk>` and `copy_nonoverlapping`s the *entire* input into it on every parse
(`state.rs:82-106,285-305`). For a 2 MB input this is a 2 MB memcpy + allocation per parse, to buy
per-chunk-bounds-check-free SIMD tails. The design is documented and sound, but the cost is real and
should be a load-accounted line in the graduation profile — for streaming/repeated small parses the
copy can dominate. Candidate transposition: pad in place when the caller owns a `String` with spare
capacity, or expose a `parse_padded(&PaddedView)` entry that lets a caller amortize the pad across
many parses.

**Compiler exploitation, current state.** `call` is `#[inline(always)]` with a flag fast-path
(`flags==0` and `flags==TRIM_WS`) and a `#[inline(never)]` cold tail (`parse.rs:80-143`) — good
branch shape. `dyn ParserFn` is the monomorphization boundary: each `.call()` is an indirect
virtual call. Leaf scanners (`string_impl`, `take_while_byte_span`, `dispatch_byte`) are
`#[inline(always)]` and byte-branch on `state.src_bytes.get(offset)` — sound. `lazy()` deliberately
bypasses the flag dispatch on the cached inner parser (`lazy.rs:56-62`) to shave a branch per
recursive call — a real, correct micro-opt. Gap: the whole-tree indirect-call chain is the ceiling;
only enum-dispatch (C) or codegen (skinny's route) removes it, and skinny already chose codegen.

---

## 2. Consumer map — the graduation-relevant fold (the pivotal finding)

Who actually depends on the Rust combinator crate vs. the regex engine determines what uplifts vs.
archives:

| Consumer | parse_that combinators | bbnf-regex engine |
|---|---|---|
| `bbnf-lang/crates/core` (legacy totality — **archives**) | `parse_that = "0.4"` | via ir |
| `bbnf-lang/crates/ir` (legacy — **archives**) | 0.4 | `bbnf-regex = "0.1"` (the 6,742-LOC engine) |
| `bbnf-lang/crates/gorgeous`, `wasm`, `xtask` (legacy — **archive**) | 0.4 | — |
| `bbnf-lang/crates/bbnf-path` | — | `bbnf-regex` path-lexer |
| **`skinny` (the tree of record)** | **NONE** | its OWN 322-LOC recognizer + `parse-that-regex` fork |
| value.js / latex-paper / keyframes (backbones) | **TS package only** — not the Rust crate | — |

**The Rust combinator crate has no consumer that survives graduation.** skinny uses hand-written
generated scanners, not combinators; the backbones consume the *TypeScript* package. Every Rust
consumer of `parse_that` combinators (`crates/core`, `ir`, `gorgeous`, `wasm`, `xtask`) is in the
archiving legacy tree. **So the combinator-crate allocation census in §1, while real, applies to a
crate that is a candidate for archival, not uplift** — unless the graduated tree grows a general
parser-combinator surface (e.g. to host the self-hosted BBNF bootstrap in combinators rather than
codegen). This is the first adjudication the synthesis must make (open question A).

---

## 3. The regex engine SOTA check (rust/regex, the 6,742-LOC bbnf-regex)

Pipeline: bespoke HIR (`hir/mod.rs` — flat enum, `CharClass` retains a `negated` flag instead of
materializing the complement, the deliberate divergence from `regex-syntax`) → egraph HIR
simplification (`egraph/`, equality-saturation with a per-compile `SaturationCache`) → NFA
(`automata/nfa.rs`) → subset construction → Hopcroft minimization over byte equivalence classes
(`automata/dfa.rs:1-102`) → flat transition table + self-loop acceleration (`automata/accel.rs`).
A `RegexInfo::analyze` pass (`info/mod.rs`) derives one-pass eligibility, literal prefix/suffix,
dfa-size estimate, accel candidacy, and an `EngineSet` bitset (`info/mod.rs:60-90`) that a
recognizer-tier CSP consumes to pick an engine.

**What it HAS that matches SOTA** (vs. [regex-automata](https://docs.rs/regex-automata)):
byte-class compression (`sets/equiv::compute_byte_classes` → `[u8;256]` map + flat table,
`dfa.rs:43-99`) ✓; Hopcroft minimization ✓; anchored `find_at`/`find_at_shortest`
(`dfa.rs:116-170`) ✓; one-pass eligibility detection (`info/one_pass.rs`) ✓; literal
prefix/suffix extraction (`info/literal_prefix.rs`) ✓; memchr/nibble-LUT self-loop acceleration —
regex-automata's "accelerated states" (`accel.rs`, matches simdjson two-VPSHUFB nibble technique) ✓;
first-char sets (`first.rs`) ✓; egraph algebraic simplification of the HIR — this is *beyond*
regex-automata, which does not equality-saturate its HIR.

**What it LACKS vs. SOTA, and whether the lack is right:**

- **No lazy/hybrid DFA.** The engine compiles a full minimized DFA eagerly with a hard
  `state_limit: 4096` bail (`dfa.rs:24,36`); patterns that blow the limit return `None` and the
  caller panics (`scanners.rs:76`). regex-automata's hybrid lazy DFA builds states on demand to
  dodge exponential compile. **Verdict: eager is RIGHT for bbnf** — patterns are grammar literals,
  compiled once, cached (`scanners.rs:55-80`), and small; a lazy tier is complexity with no payoff.
  The one gap: the 4096 bail should *degrade* (fall to an NFA/PikeVM walk) rather than `panic!`. Name
  it — but it is a robustness fix, not a SOTA-parity ask.
- **The `find_at` hot loop carries a redundant per-byte load.** `dfa.rs:124-135` does `bytes[pos]`
  (bounds-checked) then, after the transition, `states[state].is_accept` — a *second* array
  indirection every byte purely for the accept test. SOTA folds accept into the transition table
  (tag the `StateId` high bit, or a parallel `is_match` bitset) so the loop touches one array. And
  `bytes[pos]` should read the `padded_bytes()` view (`state.rs:334`) to drop the bounds check —
  the padding machinery exists but the DFA loop does not use it. Two clean transpositions on the
  hottest loop in the engine.
- **No in-DFA prefilter for the *start* of an unanchored search.** N/A — every bbnf match is
  anchored `find_at(bytes, offset)`, so a start-prefilter (Teddy/memchr-first-byte) is irrelevant.
  Rightly absent.
- **No PikeVM / bounded-backtracker / capture-group tier.** Rightly rejected — bbnf never needs
  capture offsets or catastrophic-backtracking protection on a DFA-only surface. The NO-GO on the
  WDM/packrat tier (below) is the same disposition on the combinator side.
- **Byte-class map is `[u8;256]` (≤256 classes, `num_classes: u16`).** Fine; no compression gap.

**What it rightly rejects:** full backtracking, look-around, Unicode word boundaries on non-ASCII
(the `Look` enum is line/anchor only, `hir/mod.rs:171-185`), multi-pattern capture. All correct for
a grammar-literal DFA surface.

---

## 4. The fork divergence and the fold-back (the second pivotal finding)

There are **two crates named `bbnf-regex`** and they are architecturally unrelated:

- **Canonical (`parse-that/rust/regex`, 6,742 LOC):** the general HIR→NFA→DFA engine of §3.
  Consumed by legacy `bbnf-lang/crates/ir` and `bbnf-path`.
- **skinny (`skinny/crates/bbnf-regex`, 322 LOC):** NOT an engine. A coarse *family recognizer* —
  `analyze(pattern) -> RegexFacts { nullable, first, byte_classes, hir: RegexKind }` where
  `RegexKind ∈ {Whitespace, QuotedString, Numeric, Unknown}` (`skinny/crates/bbnf-regex/src/
  lib.rs:15-20,45`). It builds no automaton. The actual matching for the recognized families is
  hand-written scanners in **`skinny/crates/parse-that-regex`** (3,022 LOC:
  `number/` eisel_lemire float parse, `unicode/` Höhrmann UTF-8 byte-DFA decoder,
  `StringMode`/`StringFlags` typed byte classification — `lib.rs:35-75`), NEON-accelerated via
  `bbnf-simd`. skinny does NOT fork the automata pipeline at all.

So the graduated tree, *as it stands today, has no general DFA regex engine* — only a 4-family fast
path. Any grammar regex outside {ws, quoted-string, number} falls to `RegexKind::Unknown` with no
matcher behind it. **The fold-back is therefore bidirectional and is the core Rust-uplift decision:**

1. **Fold the canonical engine IN, beneath the recognizer.** The graduated `bbnf-regex` should be
   the skinny family-recognizer front tier (fast, allocation-free, handles the common case with
   NEON scanners) *backed by* the 6,742-LOC HIR/NFA/DFA engine as the general fallback for
   `Unknown`. This is the elegant union: skinny's recognizer already computes the `EngineSet`-style
   facts the canonical `info/mod.rs` produces; wire `Unknown` to `Dfa::compile`.
2. **Absorb what the fork learned upstream:** (a) the `bbnf-simd` stable-NEON façade replaces
   `#![feature(portable_simd)]` (§0); (b) `StringMode`/`StringFlags` typed byte classification and
   the Höhrmann UTF-8 decoder (branch-lean, stable) are strictly better than the ad-hoc
   `take_while`/`char_indices` UTF-8 handling in `leaf.rs:118-149`; (c) the `RegexKind` recognizer
   tier is a faster front-end than routing every literal through `Dfa::compile`.
3. **Kill the duplication.** `parse_that/src/parsers/eisel_lemire/table.rs` (660 LOC) and
   `skinny/crates/parse-that-regex/src/number/eisel_lemire/table.rs` (660 LOC) are byte-identical
   powers-of-ten tables; `algorithm.rs` (93 LOC each) likewise. One canonical `eisel_lemire` module
   after the fold.

---

## 5. Legacy / deprecated census

- **`span_parser/` (11 files, ~1,150 LOC).** The TS side executed a "SpanParser KILL"
  (`7901314 Tranche B (0.12.0): … SpanParser KILL`; `afea5c2` FALSIFIED its perf hypothesis and
  retired it). The Rust `SpanParser` (`span_parser/mod.rs:131` `Boxed(Box<dyn ParserFn>)`, a tagged
  union of leaf strategies) is the same falsified experiment, still live and still consumed by
  `parsers/css/*` and `parsers/json.rs`. Since those consumers are demo/bench parsers that archive
  with the combinator crate, `span_parser` archives with them — it should NOT graduate. Named
  legacy.
- **`parsers/css/` (7 files, ~1,600 LOC) + `parsers/csv.rs`, `parsers/json.rs`.** Demonstration /
  competitor-bench parsers (the Cargo.toml competitor benches: nom/pest/winnow/lightningcss/
  cssparser, `Cargo.toml:37-85`). Not part of the primitive surface; the TS side already removed its
  CSS parser (`c86a149 A.W1: CSS-parser removal — parse-that becomes pure primitives`). The Rust
  side never followed. **The Rust combinator crate should mirror the TS "pure primitives" cut** if
  it graduates at all — else archive whole.
- **`rust/src` (the `main.rs` bin) and `rust/bootstrap` (62 LOC).** `bootstrap` is a thin re-export
  of `parse_that::regex::generated` preserving one divan bench (`bootstrap/src/lib.rs`); its
  proc-macro-host role was already retired at bbnf-lang B2. Archives with the bench harness.
- **`.cargo/config.toml` path-patch web.** `rust/.cargo/config.toml` is `.gitignored` and rewrites
  8 crate coordinates to sibling-repo paths (`pprint`, `bbnf`, `bbnf-ir`, `bbnf-ser`, `csp-solver`,
  `egraph`, `egraph-derive`, and self-patches `parse_that`/`bbnf-regex`). This is a *local dev
  mirror*, not a pin — it silently breaks if a sibling moves, and it couples parse-that's build to
  the entire bbnf-lang + csc411 checkout. For graduation the surviving crates
  (`bbnf-regex`, `eisel_lemire`, `bbnf-simd`) must resolve by **published version pin or
  workspace-relative path inside the graduated tree**, not a gitignored cross-repo patch table. The
  egraph/csp-solver edges (`regex/Cargo.toml:35-37`) are the deepest coupling — the HIR e-graph
  pulls `egraph` + `egraph-derive` + `csp-solver` from three separate repos.

---

## 6. Staleness census

`rust/` is frozen at `051a6d6` 2026-05-02; only 2 commits touch `rust/` since 2026-05-01 (both the
`bbnf-regex` path-lexer, AZ-IV.W2.3). The active TS tranches (A/B/Q/S → 1.0.0) since then never
touched Rust. Meanwhile skinny's scanner fork advanced *independently*: `parse-that-regex` +
`bbnf-simd` carry NEON kernels, the Höhrmann decoder, and `StringMode` typing that the frozen Rust
`parse_that/src/parsers/scan/*` and `leaf.rs` never received. Net drift: **the frozen rust/ scanners
are behind skinny's fork; the frozen rust/ regex *engine* is ahead of skinny's recognizer.** The
fold reconciles both directions at once (§4).

---

## 7. The Rust uplift wave list (each item: transposition + consumer-safety gate)

Ordered; every item is a transposition to a better design, not a strip. "Backbone" = value.js /
latex-paper / keyframes; note these consume the **TS** package, so no Rust item can break them —
the Rust consumer-safety gate is `bbnf-lang/crates/ir` + `bbnf-path` (regex-engine consumers) and,
if the combinator crate graduates, `crates/core`/`gorgeous` — all of which archive, so most Rust
gates are internal test-parity, not external semver.

- **RU-1 — De-nightly to stable.** Delete `#![feature(portable_simd)]` and route all `std::simd`
  through the `bbnf-simd` stable-NEON façade (+ `wide`/`pulp` for the non-NEON portable path); drop
  `smallbox[nightly]` (see RU-2); drop `fmt_helpers_for_derive` (pprint-derive bump) and
  `stmt_expr_attributes` (hoist the `#[cfg]`). Gate: DFA-compile + scan test parity in `regex/tests`
  and `parse_that/tests/{number_simd,quoted_simd,ws_comment}_test.rs`; byte-identical scan outputs.
- **RU-2 — Retire smallbox; arena the parser tree.** Replace `SmallBox<dyn…,S32>` with `BumpSlab`
  arena allocation of the parser tree (transposition A), or `Box<dyn ParserFn>` (fallback B). Kills
  the fourth nightly gate and the defeated-inline waste (§1). Gate: `combinator_test.rs` parity;
  only relevant if the combinator crate graduates (open question A).
- **RU-3 — Fold the canonical regex engine beneath the skinny recognizer.** Unify the two
  `bbnf-regex` crates: skinny's `RegexKind` recognizer front tier + the 6,742-LOC HIR/NFA/DFA engine
  as the `Unknown`-family fallback (§4.1). Gate: `bbnf-lang/crates/ir` regex-facts parity +
  skinny recognizer test suite; the DFA path must produce identical spans to the family scanners on
  {ws, quoted, number}.
- **RU-4 — Fold the scanner fork home; kill eisel_lemire duplication.** Absorb `StringMode`/
  `StringFlags`, the Höhrmann UTF-8 decoder, and the NEON scanners; collapse the two byte-identical
  `eisel_lemire/table.rs` (660 LOC ×2) to one module (§4.2-4.3). Gate: `number_fastpath_test.rs`,
  `decode_test.rs`, `quote_parity_test.rs` parity across both trees.
- **RU-5 — Hot-loop the DFA `find_at`.** Fold `is_accept` into the transition table (tagged
  `StateId` / `is_match` bitset) to remove the per-byte `states[]` load; read `padded_bytes()` to
  drop the per-byte bounds check (§3). Gate: `regex.rs` + `bbnf-regex/tests/*` match-position
  parity; profile the cycles/byte delta under load (charter correction 5).
- **RU-6 — Degrade the 4096-state bail instead of panicking.** Replace the `panic!` in
  `scanners.rs:76` / the `None` bail in `dfa.rs` with an NFA-walk fallback for over-limit patterns
  (§3). Gate: adversarial large-pattern test; no panic on any grammar-legal regex.
- **RU-7 — Pure-primitives cut / archive the demo parsers.** Mirror the TS "pure primitives"
  removal: `parsers/css/*`, `csv.rs`, `json.rs`, and `span_parser/` (the falsified SpanParser)
  archive with the legacy combinator crate; only leaf primitives + scanners graduate (§5). Gate:
  none external (bench-only consumers); competitor benches move to the archive.
- **RU-8 — Relocate the resolution graph.** Replace the gitignored cross-repo
  `.cargo/config.toml [patch.crates-io]` table with workspace-relative paths / published pins inside
  the graduated tree; sever the `egraph`/`egraph-derive`/`csp-solver` three-repo edges via
  workspace membership (§5). Gate: clean `cargo metadata` with no sibling-repo path escape.
- **RU-9 — Diagnostics label interning + padded-buffer amortization (lower priority).** Replace
  `Box::leak(format!(…))` labels with a `&'static` interner (unbounded-leak fix, `leaf.rs`); expose
  a `parse_padded(&PaddedView)` entry so repeated small parses amortize the per-parse input copy
  (§1). Gate: `diagnostics`-feature tests; load-accounted profile of the copy cost.

---

## Open questions for synthesis

- **(A)** Does the graduated tree keep a general **parser-combinator** surface at all, or is
  combinators legacy-only (skinny uses codegen, not combinators; the sole survivors would be leaf
  primitives feeding a bootstrap)? RU-2/RU-7 hinge on this.
- **(B)** Which crate name wins the `bbnf-regex` collision, and does the recognizer-over-engine
  union (RU-3) live as one crate or two layered crates?
- **(C)** Is `bbnf-simd` (aarch64-only today) the graduated SIMD façade, and what is the x86/portable
  fallback — hand-written `core::arch::x86_64` to match, or `wide`/`pulp`?
</content>
</invoke>
