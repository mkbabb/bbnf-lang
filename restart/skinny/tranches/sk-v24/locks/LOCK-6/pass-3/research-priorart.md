# LOCK-6 pass-3 — PRIOR-ART: THE ISOMORPHISM PLANE (rust/ts split)

**Role.** Prior-art researcher, no family allegiance. Registry + favored-approach withheld by
protocol. **Problem class.** "Proper isomorphism to a reasonable point, in layout and in code"
between a Rust generator/runtime and a TypeScript backbone — what is *mirrored* (module tree,
names, oracle tables, tape layout, bench protocol), what stays *idiomatic per language*, and
where V8 laws diverge from LLVM laws. Candidate mechanisms (PORTFOLIO LOCK-6): 6A mirror-tree +
isomorphism linter, 6B spec-generated skeleton, 6C wasm-core, 6D oracle-isomorphism (the stated
null hypothesis).

The settled prior art (banked across pass-2/pass-3): the **SWC/Biome** split proves the winning
delivery shape is *one core, two bindings* — napi for Node (≈45–60% faster than the same logic as
WASM; the value.js/latex-paper/keyframes consumers all run under Node/vitest, R4 §7), wasm for
browser only — never "wasm is the one implementation" [deepwiki swc, biome; yieldcode; jetbrains].
**FlatBuffers/Cap'n Proto/Kaitai** are the trodden 6B path for a byte-identical layout decoder
generated into both languages [flatbuffers.dev; kaitai.io]. **tree-sitter** proves multi-language
parser systems mirror by *generation/binding from one source*, never hand-maintained twins
[tree-sitter docs]. **test262/CommonMark** are the 6D behavioral standard [tc39/test262;
commonmark-spec]. The wasm boundary strips NEON (WASM SIMD scalarizes non-`q` ops [emscripten;
v8.dev]) — so 6C cannot carry §0's ISA-specific differentiator. **This pass advances that base
with a running in-tree artifact neither prior sweep cited, plus three on-point external anchors.**

---

## 1. THE HEADLINE: `crates/bbnf-path-ts` is a live, shipping hybrid of ALL FOUR families

The tree already contains a working answer to LOCK-6, unnoticed by pass-2 and the prior pass-3
sweep: **`bbnf-path-ts`** (AZ-IV.W5.1), a `cdylib + rlib` wasm-bindgen binding that mirrors the
Rust `path!` proc-macro at the TS surface. It is not a prototype in a worktree — it is a
first-class crate with a fidelity test. It embodies each candidate family *simultaneously*,
which is itself the strongest evidence that no single family is the whole answer:

- **6C (wasm-core) for the compute:** the cdylib exports `compile_path(path, grammar)` /
  `execute_path(typed_path, doc)` via wasm-bindgen + **serde-wasm-bindgen 0.6**; release profile
  `wasm-opt = ["-O3","--enable-simd"]` (binaryen ≥119) — the SIMD toggle exists in-tree, but the
  path compiler is a cold, non-hot lane, so it does not test the §0 NEON-scalarization wall
  (`crates/bbnf-path-ts/Cargo.toml`).
- **6B (single-source) for the alphabet + types:** the **lexer is shared, not mirrored** —
  `bbnf-regex::lex_path` + `PathToken` are the *identical* tokens the proc-macro uses, so "segment
  alphabet is isomorphic across Rust + TS frontends" by construction; the error **taxonomy** is
  shared through `bbnf::path::error::PathError`; wasm-bindgen `--typescript` emits the `.d.ts`
  skeleton (`Cargo.toml` header; `ts/path.ts:20-58`).
- **6A (hand-mirror) for the logic — forced, and named as the risk:** because "**`bbnf-path`
  (proc-macro = true) cannot be a library dependency**, the macro's lex/lower/validate logic is
  mirrored here at function scope" (`Cargo.toml` header). This is the drift-prone hand-twin the
  industry (simd-json) warns of, present in-tree by a hard toolchain constraint (proc-macros are
  not linkable libraries).
- **6D (oracle) as the drift gate:** `tests/isomorphic_path_error.rs` asserts the TS-bridge
  payload is *field-for-field* identical `(segment_index, segment_str, struct_name, alternatives,
  reason)` to the canonical `bbnf::path::PathError`, and `reason_variants_are_isomorphic` is an
  **exhaustive match** — "adding a new variant on either side without updating the other fails
  this test" (`tests/isomorphic_path_error.rs:88-100,227-268`).

**The measured divergence (the load-bearing lesson).** The test *pins only the surface where both
checkers agree* and documents a real asymmetry it deliberately does not cross: for
`$.statuses[0].nope`, the canonical offline checker leaves `TypeDesc::Named(_)` intact →
`UnregisteredRule`, while the cdylib resolves `Named(_)` **eagerly** through its in-registry
`layout(_)` → `UnknownField`. **Same input, different diagnostic**
(`tests/isomorphic_path_error.rs:129-138`). This is a live, in-tree instance of exactly the 6A
hand-mirror drift the prior art predicts — two independently authored resolution *policies*
diverge under a *shared taxonomy*. 6D caught it; the fix is deferred to the proc-macro's
IR-resolved path, not patched in the twin. **Verdict:** shared *taxonomy/alphabet* (6B) does not
imply shared *behavior* — behavior still needs the 6D oracle, and even the oracle can only pin
the agreed surface until the two logics are unified at one source. This is the null hypothesis
(6D) *and* its limit, demonstrated by running code rather than argued.

## 2. THE SMALL-PAYLOAD / LARGE-PAYLOAD BOUNDARY LAW (new constraint, from two in-tree artifacts)

Placing `bbnf-path-ts` beside PROVE-14 draws a boundary neither prior sweep isolated:

| Artifact | What crosses | Mechanism | Cost model |
|---|---|---|---|
| **bbnf-path-ts** | a small structured **diagnostic** (`PathErrorPayload`, a handful of fields) | full **serde-wasm-bindgen** marshal — `JsError` message = JSON-serialized payload; TS `JSON.parse(message)` round-trips it back to a typed `PathError` (`ts/path.ts:126-140`) | full marshal is *fine* — the payload is tiny; correctness/ergonomics dominate |
| **PROVE-14** | a large **value tape** (631 KB source, 29,573 tokens) | **offset/handle** typed-array views (`offsets/flag_cursors/flag_values/source`), never full-value marshal | full marshal would eat the win — 4.4× stop-early holds *only* by crossing offsets, not values |

**Constraint imposed:** the cross-language plane is *bimodal by payload size*. Small structured
control-plane data (diagnostics, typed-path descriptors, error taxonomy) crosses by full
serde-wasm-bindgen JSON and that is idiomatic and correct; large value-plane data (the tape, the
document) crosses by offset/handle views and full marshal is forbidden (the wasm-bindgen
"minimize copying / hold a `JsString` handle" discipline [wasm-bindgen string guide]). A LOCK-6
verdict must not impose the offset-only tape discipline on the diagnostic plane (over-engineering
a 5-field payload) nor permit serde marshal on the value plane (re-importing the full-walk tax).
Two in-tree artifacts already sit on the correct sides of this line.

## 3. ANTLR — the canonical "one grammar → many language runtimes," and the named target-lag trap

The closest large-scale analog to bbnf's ambition (a grammar yielding parsers consumed across
languages) is **ANTLR 4**: *one* Java tool generates lexer+parser for **10 targets** — C++, C#,
Dart, Java, JavaScript, PHP, Python3, Swift, **TypeScript**, Go — from one grammar, with a
per-target hand-written runtime library. "Ensuring consistency across these targets is a unique
and highly valuable feature" [antlr4 targets.md; antlr-ng]. **The trap, stated by the project
itself:** new features "generally appear in the Java target and then migrate to the other targets,
[but] these other targets don't always get updated in the same overall timeframe" — feature
parity **lags per target**, and every release re-versions all 10 runtimes in lockstep even when
unchanged [antlr4 project-testing.md]. **Lesson for LOCK-6:** a two-target (rust/ts) mirror is the
*minimal* instance of ANTLR's problem; the lag trap says the isomorphism gate must fire on the
*derivative* (has TS fallen behind Rust?) — corroborating the pass-2 ratchet finding
(monotone-non-increasing hand-idiom fraction, not an absolute constant). ANTLR's answer to lag is
a single shared conformance harness run across all runtimes — i.e. 6D is load-bearing even where
6B-generation is the primary mechanism.

## 4. toml-test — an adoptable 6D corpus already sized to the GN-5.W2 TOML grammar

GN-5.W2 authors a **new TOML grammar** ("no TOML exists anywhere" — SPEC §2/GN-5). The industry
already ships the exact 6D artifact for it: **`toml-lang/toml-test`**, a language-agnostic
conformance suite (valid/invalid corpora; decoders read TOML on stdin, emit a canonical **JSON**
tagged-value form, or non-zero exit on invalid; compatible with TOML v1.0/v1.1). Go, Rust
(`crates.io/toml-test`, `toml-test-data`), and JS parsers all run it [toml-lang/toml-test;
crates.io]. **Lesson:** 6D is not merely "nearly free" in the abstract — for each new grammar
there is often a *ready-made, adoptable* differential corpus, and TOML's is directly on-point.
The bbnf TOML grammar can be conformance-gated against toml-test *from both the Rust and any TS
face* with zero corpus authoring, making the rust/ts behavioral isomorphism for TOML a
plug-in rather than a build. This is concrete evidence the 6D face scales per-grammar at near-zero
marginal cost — the pass-2 estimate ("one differential corpus + one reference impl") is, for
TOML, *already written by someone else*.

## 5. tsify / wasm-bindgen `--typescript` — the type-face generation stack `bbnf-path-ts` instantiates

The 6B *type* face (Rust data types → TS `.d.ts`) is a solved, documented stack, and
`bbnf-path-ts` is a live instance of it: **`wasm-bindgen --typescript`** generates `.d.ts`
definitions; **`tsify`** auto-emits `.d.ts` and wires **serde-wasm-bindgen** so complex serde
types cross with generated TS types; **ts-bindgen** goes the other direction (TS decls → wasm-
bindgen Rust) [lib.rs/tsify; ratchetdesigns/ts-bindgen; flinect; timryan.org]. `bbnf-path-ts`
uses precisely `serde` + `serde-wasm-bindgen` + wasm-bindgen's TS emission (`Cargo.toml`), so its
`TypedPath`/`PathErrorPayload` TS shapes (`ts/path.ts:20-58`) are the generated-skeleton pattern
in production.

**The bound (settled, re-confirmed):** this generates *data* types, not *inference* types. The
ts-rs/typeshare/specta limit — "TypeScript generics, conditional types, or advanced type
inference cannot be expressed via Rust struct derives; generated types will be simpler than
hand-written" [dev.co ts-rs; Aleph-Alpha/ts-rs] — means **PT-E2's `ExtractValue` type-level
inference and the typed combinator algebra (LOCK-8B) are NOT derivable from Rust** and stay
hand-authored in TS (R4 §5: the `Parser<any>` leaks are a TS-inference problem). So 6B-generation
covers the *layout ABI + data-type skeleton* (PROVE-14 tape planes, `bbnf-path-ts` payloads,
FlatBuffers/Kaitai) but **cannot** cover the TS-native compile-time inference plane. Do not
conflate "generate the layout/data types" (true, in-tree) with "generate the inference types"
(ts-rs-false).

---

## 6. Synthesis — where "reasonable" ends (updated with in-tree corroboration)

| Plane | Isomorphic? | Prior art + in-tree witness |
|---|---|---|
| Tape/value **layout ABI** | YES — byte-exact | 6B generation (FlatBuffers/Kaitai; **PROVE-14** node-for-node, fp `986912547`) |
| **Data-type skeleton / diagnostic payload** | YES (generated) | wasm-bindgen `--typescript`/tsify; **`bbnf-path-ts`** `TypedPath`/`PathErrorPayload` in production |
| **Behavior** (values, accept/reject, error-*class*) | YES | 6D corpus (test262/CommonMark; **toml-test** for GN-5.W2; **`isomorphic_path_error.rs`** exhaustive-variant gate) |
| **Resolution *policy* / logic** | NO by default — drift-prone | **`bbnf-path-ts`** measured divergence (`Named` eager vs left-intact) — hand-mirror diverges even under shared taxonomy; 6D pins the agreed surface |
| **Hot-path kernels** (NEON vs V8-monomorphic) | NO — measured | per-language idiom; full-walk parity + wasm-SIMD scalarization forbid a shared strategy |
| **Regex engine** | NO (functor only) | Rust bespoke HIR vs TS native `RegExp` (R4 §3) — LOCK-5's functor |
| **Type *inference* plane** | NO | ts-rs conditional-type limit — PT-E2/LOCK-8B hand-authored in TS |
| **Delivery** | split | napi (Node, keeps NEON) + wasm (browser, scalar tax) — SWC/Biome; **`bbnf-path-ts`** ships the wasm face |

**Forced conclusion (unchanged in direction, now witnessed by running code).** No single family
is the whole answer; the evidence composes **6B(layout+data-type skeleton) + 6D(behavior) as the
spine**, a **native-core / two-binding** delivery (not a literal single-wasm-core — loses ~45% on
the Node consumer surface and strips the NEON differentiator), an **explicit non-mirror** of the
hot path, regex engine, and TS inference plane, and a **grudging, gated 6A hand-mirror only where
a toolchain constraint forces it** (proc-macro-not-linkable → `bbnf-path-ts`), fenced by a 6D
exhaustive oracle. `bbnf-path-ts` is that exact composite, shipping today.

**Traps carried forward (named).** (1) single-wasm-core costs ~45% on Node — SWC's measured napi
reason; (2) full-value marshal eats the win on the *value* plane — cross handles/offsets
(PROVE-14) — but full serde marshal is correct on the *small diagnostic* plane (`bbnf-path-ts`) —
**the boundary is payload-size-bimodal**; (3) wasm SIMD scalarizes non-`q` NEON — the
differentiator dies at that boundary; (4) generated code covers *data types, not inference types*
(ts-rs limit) — PT-E2/8B stay hand-authored; (5) hand-mirror logic drifts *even under a shared
taxonomy* — the in-tree `Named` eager-vs-intact divergence is the proof; unify at one source or
gate exhaustively; (6) ANTLR-class **target lag** — the isomorphism gate must fire on the
derivative (has TS fallen behind?), i.e. the pass-2 ratchet, not an absolute constant; (7)
proc-macros are **not linkable libraries** — any Rust logic that must also run in TS/wasm cannot
be shared by linking and is forced into a mirror + oracle (`bbnf-path-ts` header); (8) two-arm
window asymmetry (§0/09) — any cross-language bench is paired or it is disinformation.

---

## Sources

- **In-tree (primary):** `crates/bbnf-path-ts/{Cargo.toml, src/lib.rs, ts/path.ts,
  tests/isomorphic_path_error.rs}` — live wasm-bindgen `path!` mirror + exhaustive fidelity
  oracle + measured `Named` divergence; PROVE-14 REPORT/VERIFY (tape planes, fp `986912547`,
  4.4× stop-early / full-walk parity); SPEC §0 rows 06/09/11/14; GN-4.md W4; PT-3.md; R4 §3/§5/§7.
- ANTLR multi-target: [antlr4 targets.md](https://github.com/antlr/antlr4/blob/master/doc/targets.md),
  [antlr-project-testing.md (feature-lag)](https://github.com/antlr/antlr4/blob/dev/doc/antlr-project-testing.md),
  [antlr-ng](https://www.antlr-ng.org/introduction.html).
- toml-test 6D corpus: [toml-lang/toml-test](https://github.com/toml-lang/toml-test),
  [crates.io/toml-test](https://crates.io/crates/toml-test), [toml-test-data](https://crates.io/crates/toml-test-data).
- Rust→TS type-face generation: [tsify (lib.rs)](https://lib.rs/crates/tsify),
  [ratchetdesigns/ts-bindgen](https://github.com/ratchetdesigns/ts-bindgen),
  [flinect: rust+ts+serde](https://flinect.com/blog/rust-wasm-with-typescript-serde),
  [timryan.org: serde→TS](https://timryan.org/2019/01/22/exporting-serde-types-to-typescript.html);
  ts-rs conditional-type limit [dev.co ts-rs; github.com/Aleph-Alpha/ts-rs].
- Banked base (pass-2/pass-3): SWC/Biome napi-vs-wasm [deepwiki swc, biome; yieldcode; jetbrains];
  FlatBuffers/Cap'n Proto/Kaitai [flatbuffers.dev; kaitai.io]; tree-sitter [tree-sitter docs];
  test262/CommonMark [tc39/test262; commonmark-spec]; wasm-SIMD scalarization [emscripten; v8.dev];
  wasm-bindgen string-copy discipline [rustwasm docs]; IEEE-754/NaN fold hazard [rust-lang#107247;
  RFC 8785].
- **SWC napi-vs-wasm magnitude (re-confirmed this pass):** native Node modules measure
  **~1.75–2.5× faster than the same logic as WASM** across independent benchmarks — i.e. the
  single-wasm-core delivery would surrender ~43–60% on the Node consumer surface (value.js/
  latex-paper/keyframes all run under Node/vitest, R4 §7). [deepwiki swc node bindings
  (https://deepwiki.com/swc-project/swc/7.2-node.js-bindings); nickb.dev wasm-vs-native
  (https://nickb.dev/blog/wasm-and-native-node-module-performance-comparison/); yieldcode
  (https://yieldcode.blog/post/native-rust-wasm/)].
