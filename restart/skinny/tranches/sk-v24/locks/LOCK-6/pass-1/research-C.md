# LOCK-6 / family 6C — WASM-CORE: the Rust core is the one implementation

**Charter.** The Rust core compiled to wasm is the single implementation; TypeScript is
bindings plus an idiom layer; TS-native code exists only where V8 measurably wins. The
PROVE-14 full-walk lesson is the proof such boundaries exist — and, read carefully, it is
also the proof of *where the boundary lawfully falls*.

This family is not greenfield. Two live wasm-core precedents already stand in-tree, and the
one certified cross-language datum (§0 row 14) was measured with the boundary drawn exactly
where 6C's strongest form draws it. The work is to make that boundary a *discipline* — named,
measured, and defended against the two forces that break it (NEON not crossing to wasm;
synchronous ESM consumers that cannot await a module).

---

## 1. The mechanism, made concrete

**The wasm boundary is DATA, not behavior — and the datum is the tape.** The one Rust
implementation parses source to the offset-tape SoA planes inside wasm; those planes cross
the boundary *once, in bulk*, as typed-array views over wasm linear memory; TS-native code
walks them. This is not a design choice imposed on PROVE-14 — it is the shape PROVE-14
already measured and certified (row 14 CONFIRMED):

- The four planes `Tape::{offsets, flag_cursors, flag_values, source}` cross byte-exactly,
  node-for-node across 320 cold children (all six structural counts + codeUnits + a 32-bit
  fingerprint identical to `JSON.parse`).
- Stop-early get (extract three leaves from `statuses[0]`, skip the rest) clears the 2× gate
  at **4.4×** — *because the walk stays TS-native and the boundary is crossed once*, not per
  node. The tape skips `statuses[1..99]`; `JSON.parse` must materialize the whole document.
- Full-walk is at **~0.91× parity** (2× honestly unmet). This is the load-bearing negative
  result: a full materialization crosses no fewer nodes than JSON.parse and hands the
  parse-avoidance edge back to cold-JIT JS walk cost. **Full-walk parity is the empirical
  proof that a FINE boundary loses.** A per-node `wasm.child(cursor)` FFI walk would pay the
  boundary tax 29,573 times and land far below even 0.91×. The lesson is not "wasm loses on
  walks"; it is "cross the boundary at the plane, walk in the idiom layer."

The concrete split, for the value fabric:

| stage | home | why |
|---|---|---|
| parse source → tape (SIMD scan, structural classify) | **wasm-core** | batch, one boundary crossing, no consumer awaits mid-parse |
| tape planes → JS typed arrays | **the binding** | one `wasm-bindgen` export, zero-copy views (with the growth caveat, §4) |
| classify / lazy `nextSibling` walk / stop-early get / string decode | **TS-native idiom** | V8 measurably wins here (4.4×, boundary-free); this IS the "where V8 wins" clause |

"One implementation" is honest for the **parser + regex automata + solver** — the comptime
and batch surfaces. It is *not* honest for the walk, and 6C must say so: the walk is
TS-native by measurement (row 14's 4.4×), not by concession.

---

## 2. The substrate already exists — two precedents

**(a) `bbnf-path-ts` — the archetype cdylib twin (the exact 6C pattern, realized).**
`crates/bbnf-path-ts` is a `cdylib`+`rlib`, `wasm-bindgen 0.2`, `serde-wasm-bindgen 0.6`
crate that compiles the *same* Rust path-lexer/validator the `bbnf-path` proc-macro uses
(`bbnf_regex::lex_path` + a per-grammar `GrammarFixture`) to wasm, and exposes it to a TS
template-tag:

- `crates/bbnf-path-ts/src/lib.rs:1-45` — two `#[wasm_bindgen]` exports, `compile_path`
  (lex+validate → serialized `TypedPathPayload`, byte-identical to the Rust frontend's
  `TypedPath::owned_segments()` projection) and `execute_path` (apply payload to a JS
  document view). The doc states the isomorphism contract verbatim: the payload shape is
  byte-identical so the TS frontend renders the same diagnostic the Rust frontend renders.
- `crates/bbnf-path-ts/ts/path.ts` — the TS binding idiom (template-literal tag).
- `crates/bbnf-path-ts/tests/isomorphic_path_error.rs:1-50` — **the differential oracle**:
  drives both the wasm-transported `PathErrorPayload` and the canonical Rust `PathError`
  through one JSON-fixture registry and compares `(segment_index, segment_str, struct_name,
  alternatives, reason)` field-for-field. Note the one honest asymmetry it already handles:
  `alternatives` is `Vec<String>` on the payload vs `Vec<&'static str>` canonical (static
  borrows do not survive a serde round-trip). This is what "isomorphism to a reasonable
  point, measured not asserted" looks like *in code*.
- `crates/bbnf-path-ts/Cargo.toml` — `wasm-opt = ["-O3", "--enable-simd"]` already set;
  constant-folding bundlers (Vite/esbuild/SWC) can lift path literals at build time.

This is 6C's template: **twin cdylib + serde payload + a differential test that is the drift
gate.** Generalize the pattern; keep the discipline.

**(b) `wasm/` (`bbnf-wasm` v0.1.0) — the shipped playground core.** `wasm/src/lib.rs:1-45`
is a `cdylib`+`rlib` binding of `bbnf-analysis` + LSP + `gorgeous` + the bytecode VM
(`wasm/src/{analysis,lsp,gorgeous,vm}.rs`), returning structured results via a `to_js_value`
serde-wasm-bindgen helper (`wasm/src/lib.rs:~24`). It is delivered into the UI at
`playground/src/wasm/bbnf_wasm{.js,_bg.wasm,.d.ts}` (three pkg targets: `pkg`, `pkg-node`,
`pkg-node-debug`). `wasm-opt = ["-O3","--enable-simd"]` (`wasm/Cargo.toml`).

**Homing collision (a real 6C decision, not a detail).** GN-4.W4 (`docs/tranches/sk-v24/
GN-4.md` §W4) specifies a *new* `crates/bbnf-wasm` cdylib for the tape export — but a crate
named `bbnf-wasm` **already exists** at `wasm/`. Two cdylibs mean two `.wasm` downloads and
two instantiations in the playground. 6C's strongest form consolidates: the value-fabric
tape export lands as a **module inside the extant `bbnf-wasm`** (colocation edict; one wasm
artifact, one instantiate), not a second crate. The prototype must measure the binary-size
and cold-instantiate delta of the added `tape_export` module against a second-crate split.

---

## 3. Integration seams (path:line where extant code serves)

- **The archetype to generalize:** `crates/bbnf-path-ts/src/lib.rs:1-45` (exports),
  `crates/bbnf-path-ts/tests/isomorphic_path_error.rs:1-50` (the differential-oracle model),
  `crates/bbnf-path-ts/ts/path.ts` (binding idiom).
- **The shipped core + homing target:** `wasm/src/lib.rs:1-45`, `wasm/Cargo.toml`
  (`wasm-opt --enable-simd`, `crate-type=["cdylib","rlib"]`), delivery at
  `playground/src/wasm/bbnf_wasm*`.
- **The tape export shape (PROVE-14, greenfield):** four plane accessors
  `crates/runtime/src/tape/mod.rs:126-137` (public slices); export-shape proof
  `bbnf-bench/src/bin/tape_dump.rs:78`; decoder to productionize
  `bbnf-bench/ts-tape/{tape,load,pointer,verify,bench}.mjs` → `ts/tape/src/*.ts`.
- **GN-4.W4 file plan (the value-fabric face):** `crates/bbnf-wasm/src/tape_export.rs`
  (`TapeExport{offsets:Uint32Array, flag_cursors:Uint32Array, flag_values:Uint8Array,
  source:Uint8Array}`), `ts/tape/src/{tape.ts,load.ts,pointer.ts}`,
  `ts/tape/test/tape-parity.test.ts`.
- **The serde bridge helper already written:** `wasm/src/lib.rs` `to_js_value<T:Serialize>`
  — reuse for structured (non-plane) returns (diagnostics, payloads); planes go zero-copy.

---

## 4. Prior art, with citations

- **Zero-copy typed-array views over wasm memory** are the wasm-bindgen idiom (`js-sys`
  `Uint32Array::view`/`Float32Array`): expose ptr+len, build a `Uint8Array`/`Uint32Array`
  subarray over linear memory, no copy. **The killer caveat:** views are valid only while the
  backing buffer is not resized — a wasm memory *growth* (a larger second parse) detaches
  every outstanding view (wasm-bindgen #2741 "minimizing copying", #1961, #3298). PROVE-14
  sidesteps this by *copying* the 118KB offset plane into the timed region and still winning
  4.4× — the certified number already pays the copy. (js-sys `Float32Array` docs; wasm-bindgen
  issues #2741/#1961.)
- **The boundary understands only i32/f64/raw linear memory, synchronously.** Every string,
  every structured value crosses by copy+re-encode (UTF-8↔UTF-16). "16 Patterns for Crossing
  the WebAssembly Boundary" (dev.to) and radu-matei's "practical guide to wasm memory" both
  land on: keep the hot path on one buffer, cross coarsely — the *same* conclusion PROVE-14's
  full-walk parity forces empirically.
- **portable_simd → wasm32 does NOT inherit the NEON win for free.** aarch64 *always* has NEON
  (guaranteed); wasm `simd128` is an *optional* extension with **no runtime feature detection**
  — the binary either has `simd128` or it does not, gated by `-C target-feature=+simd128`
  (rust-lang/rust #133290; `core::arch::wasm32`; nickb.dev "authoring a SIMD-enhanced wasm
  library"; learn.arm.com SIMD-on-Rust). The §0-certified kernels are *hand-written aarch64
  NEON intrinsics* (`bbnf-simd`, parse_that `scanners.rs`), which do **not** compile to wasm
  v128 — they fall to scalar unless re-expressed in `std::simd`/`core::arch::wasm32`. Decisive
  6C risk (§5 R-A).
- **Component Model / WIT** is the SOTA successor ABI to raw wasm-bindgen (canonical ABI,
  language-neutral) but is heavier and unneeded here — the boundary is one bulk tape plus a
  serde payload; wasm-bindgen + `js-sys` views suffice. Noted, not adopted.

---

## 5. Named risks and counterexamples

- **R-A — NEON does not cross; "one implementation" is a half-truth on the hot kernels (the
  killer).** The certified SIMD wins (§0 rows 01 shufti/truffle, 03 fused string scan 0.898×,
  05 CSS shufti, 06 NEON skip engine) are aarch64 intrinsics. Compiling the "one core" to
  `wasm32` ships those kernels *scalar* unless rewritten in `std::simd`/wasm128 — a second
  implementation of the hottest code, precisely what the family exists to avoid.
  **Counterexample:** row 03's 0.898× fused string scan becomes ≥1.0× in scalar wasm; the
  wasm-core parse loses the throughput that justifies wasm over the TS-native combinator. This
  is also a §0/09 shim-perf-claim trap: quoting the aarch64 numbers for the wasm build is
  disinformation. **Mitigation / test:** §6.1 measures it head-to-head and decides whether the
  scan layer is portable_simd (one source, two codegens) or genuinely dual.
- **R-B — memory-growth invalidates the zero-copy view (silent corruption).** A `TapeDoc`
  holding a `Uint32Array` view over wasm memory; a second, larger `parse()` grows the heap;
  the view now aliases moved/freed bytes and still "decodes." **Counterexample:** parse
  twitter.json, hold the view, parse citm.json (bigger) through the same instance — the first
  view's fingerprint silently changes. **Mitigation:** copy-out per parse (PROVE-14's counted
  cost), re-fetch views after every call, or a pre-grown fixed arena; §6.2 quantifies the copy.
- **R-C — per-node boundary tax (the full-walk lesson generalized).** Any API that crosses the
  boundary per tape node (`wasm.kind(cursor)`, `wasm.child(i)`) pays 29,573 crossings and
  loses catastrophically. **Counterexample:** a "clean" `WasmTapeDoc` whose `.child()` is an
  FFI call. **Mitigation:** the boundary is the *plane*; the walk is TS-native — not a
  compromise, the measured optimum (row 14).
- **R-D — string re-encode tax erodes stop-early.** If wasm's `get_str` returns a *JS string*
  per call, each get pays a UTF-8→UTF-16 copy across the boundary and the 4.4× decays.
  **Counterexample:** a wasm `get(pointer)->string` export. **Mitigation:** return `offset+len`
  into the already-crossed `source` Uint8Array; decode in TS-native (PROVE-14's byte-hash fast
  path already does this — decode only the flagged escaped minority via the flag channel).
- **R-E — synchronous ESM consumers cannot await wasm (possibly disqualifying for
  parse-that).** value.js/latex-paper/keyframes (R4 §7) `import` pure-TS combinators and call
  `dispatch()`/`all()` *synchronously*; a wasm-core parse-that forces async
  `WebAssembly.instantiate` + a `.wasm` asset into their bundles. **Counterexample:** value.js's
  59 synchronous `all()` sites cannot `await` a module init. **Consequence — the sharp
  finding:** wasm-core is lawful on the **bbnf side** (the generated parser + tape +
  regex-automata + solver — batch, async-tolerant, consumed by the playground and by build-time
  TS) but **collides with parse-that's synchronous combinator consumer contract** (LOCK-8D
  territory). 6C's strongest form therefore draws the wasm boundary on bbnf's
  generated/value-fabric plane, leaves parse-that's combinator backbone TS-native (PT-3), and
  makes the *tape* the crossing seam — exactly the PROVE-14 + `bbnf-path-ts` +
  consumer-contract reconciliation. This makes "TS-native where V8 wins" precise: V8 wins on
  the incremental combinator walk (sync, R4 §6 shape-stable) and the tape walk; wasm wins on
  the batch parser/regex-automata/solver.
- **R-F — cold-instantiate is a real, uncounted cost.** Each fresh process/tab pays wasm module
  compile + instantiate before the first parse; the warm-bench-is-disinformation law (§0 iv)
  binds — count it like cold parse, not amortize it away. Two cdylibs (the R-B homing
  collision) double it.

---

## 6. What a prototype must test to move this family

1. **The NEON-crossing decision (decisive).** Compile a single §0-certified kernel — row 03
   fused string scan or row 06 skip engine — to `wasm32-unknown-unknown` with
   `-C target-feature=+simd128`, and measure parse throughput three ways: aarch64 native NEON,
   wasm (scalar fallback of the same source), wasm with the scan re-expressed in
   `std::simd`/`core::arch::wasm32`. **Question that gates the family:** does the wasm build
   keep the SIMD win, or is the hot kernel a genuine second implementation? Cold, paired, PMU
   where the OS allows; wasm timed with instantiate counted (R-F).
2. **Memory-growth view-invalidation.** Two increasing-size parses through one instance holding
   a live `TapeDoc` typed-array view; assert the second does not corrupt the first, or prove
   copy-out is mandatory and quantify its cost against the 4.4× stop-early margin.
3. **Boundary-granularity confirmation.** wasm-tape + TS-native walk (PROVE-14 shape) vs a
   deliberately per-node wasm-FFI walk on the same corpus — confirm the full-walk parity lesson
   generalizes (fine boundary loses by a large factor), fixing the boundary at the plane by
   evidence, not assertion.
4. **Homing reconcile + one-artifact cost.** Land the value-fabric `tape_export` as a module
   inside the extant `bbnf-wasm` (`wasm/`) vs a second `crates/bbnf-wasm` cdylib; measure
   `.wasm` size and cold-instantiate delta of one consolidated artifact vs two.
5. **The differential-oracle generalization.** Extend the `isomorphic_path_error.rs` model to
   the tape: one conformance corpus (twitter/citm/canada), the wasm-produced planes vs a TS
   `JSON.parse` reference, digest/byte-for-byte, as the standing drift gate — the CI artifact
   that makes "isomorphism to a reasonable point" *measured*.

---

## 7. The family's thesis, stated

wasm-core is one implementation of the **batch, comptime, async-tolerant** surfaces (the bbnf
generated parser, the regex automata, the solver, the value-fabric tape) — crossing to TS at
exactly one seam, the tape plane, proven byte-exact and boundary-coarse by row 14. The idiom
layer (tape walk, stop-early get, string decode) is TS-native by *measurement*, because V8
wins there and the boundary tax would sink it. The combinator backbone stays TS-native (PT-3)
because its consumers are synchronous ESM. Two forces bound the plane and must be prototyped,
not asserted: NEON does not cross to wasm (R-A — the hot kernels risk becoming a second
implementation), and synchronous consumers cannot await a module (R-E — which confines
wasm-core to the bbnf side). The precedent is in-tree (`bbnf-path-ts`); the discipline is the
differential oracle; the null this family must defeat or concede is R-A.
