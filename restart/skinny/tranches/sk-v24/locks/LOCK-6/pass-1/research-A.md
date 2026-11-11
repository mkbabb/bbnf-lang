# LOCK-6 / family 6A — MIRROR-TREE: hand-authored both sides + a CI isomorphism linter

Pass 1 research. The plane is the rust/ts split: bbnf-lang's cdylib bridges (`bbnf-path` ↔
`bbnf-path-ts`, and the coming wasm tape) and parse-that's dual Rust+TS combinator core. 6A's
thesis: both trees stay hand-written and idiomatic per language; a checked-in, machine-readable
**name map** is the single source of truth for what mirrors what; a CI **linter** proves the map
holds against both *extracted* surfaces. Structure is pinned at ITEM granularity; idiomatic bodies
and file-tree partition float free. Behavioral parity is the backstop tier, not the front line.
This is Kotlin Multiplatform `expect`/`actual` reconstructed across two repos and two languages,
with an external linter standing in for the single KMP compiler no toolchain here provides.

---

## 1. The mechanism, made concrete — four tiers, a mirror pair declares which bind it

**T1 — name/tree correspondence.** A machine-readable manifest enumerates mirror pairs and an
item-granularity map: `(rust_path, ts_path, { RustItem → tsItem })`. The linter asserts every
mapped Rust item exists, every mapped TS item exists, AND — decisively — that **no surface item on
either side lacks a manifest entry** (bidirectional totality; a silent omission is RED). This is
the exhaustive-match discipline the live code already hand-writes at
`crates/bbnf-path-ts/tests/isomorphic_path_error.rs:227-268` (`reason_variants_are_isomorphic`
enumerates all 8 `PathErrorReason` variants against their payload twins; adding a variant on one
side alone fails it) — generalized from one hand test to a checked artifact spanning the tree.

**T2 — surface correspondence.** Each mirrored module's *public surface* is extracted to a
committed snapshot and diffed *through* the name map, not textually: identical item set, field
names, enum-variant sets, arities. Rust side = rustdoc-JSON via `cargo-public-api` (git-committed
`public-api.txt`, CI-diffed) or `cargo-semver-checks` for variant/field-level lints. TS side =
Microsoft API Extractor `.api.md` or a `ts-morph` surface script emitting the same normalized JSON.
Reports `field renamed on one side only`, `variant present in Rust absent in TS`, `arity drift`.

**T3 — behavioral correspondence.** One shared fixture set drives BOTH surfaces and compares
outputs field-for-field. This tier is already LIVE in prototype at `isomorphic_path_error.rs`
(five differential cases + the exhaustive enum test); 6A generalizes it so ONE corpus feeds a Rust
integration test AND a vitest test. T3 is where the coverage argument lives (see risk 1).

**T0 — byte tier (the tape only).** The offset-tape (PROVE-14) mirrors at the BYTE level, stricter
than any name map: wasm-exported planes byte-identical to a Rust `tape_dump` fixture. This is the
born-RED GN-4.W4 gate (b). A name-map linter is too weak here; the byte fixture is its own tier and
composes over T1–T3.

**Why this is not generation (6A's deliberate refusal).** T1's manifest is exactly KMP's `expect`
declaration; the idiomatic per-language bodies are the `actual`s; the linter is the compiler that
spans no single toolchain. Generation (spec→both skeletons) would make drift *unrepresentable* but
force a shared surface and forfeit idiomatic freedom on it. 6A pays a **linter tax** to keep both
trees hand-authored — it *detects* drift rather than *precluding* it, and buys per-language
idiom freedom as the consideration.

---

## 2. The grammar-totality obligation on the name map (the sharpest 6A design decision)

Constitution clause (i) forbids a free-standing IDL: everything is grammar-driven save primitives.
A raw `isomorphism.toml` authored by hand would be a second source of truth — a name-gate in
disguise (clause vi) and consumer-less if the linter lapses (clause viii, Y.13). The strongest 6A
form therefore makes the map **grammar-expressible**: a `.bbnf` fragment (or a projection of the
same grammar-derived schema the LIVE Rust emitter already walks at
`crates/core/src/backend/emitter.rs` + `crates/core/src/grammar/schema/emit/rust/`) supplies the
*generated* item names on both sides for free — the rule/variant names are already shared truth —
while the manifest carries ONLY the hand-idiom correspondences the grammar cannot name (e.g.
`Parser::then ↔ Parser.then`). This confines the hand-authored map to the idiom residue and keeps
the generable core drift-checked against the grammar itself. (Note the boundary: where the map's
entries are *derived* rather than *authored*, 6A shades toward 6B; 6A's distinct claim is that the
residue — hand bodies mapped by a thin manifest and enforced by extraction — is the majority of the
combinator surface and must stay hand-written.)

---

## 3. The substrate (what exists, what is prose, what must be built)

- **The name map.** Today the correspondence is carried as *prose*: `crates/bbnf-path-ts/src/
  schema.rs:1-15,19,37,52,70` are doc comments — `"mirror of bbnf::path::*"`, `"byte-identical
  under serde-wasm-bindgen"`, `"field-for-field identical"` — an UNCHECKED, drift-prone name map.
  6A promotes those comments to a machine-checked manifest and DELETES the prose duplicates
  (risk 3). This is the single LIVE seed of the whole family.
- **Rust extractor** — `cargo-public-api` (rustdoc-JSON) / `cargo-semver-checks`. Snapshot committed
  per crate; the diff is a CI gate. Fits parse-that's existing proof-gate idiom (R4 §4: `proof:*`
  node scripts, `manifest-gate.mjs`, `subpath-gate.mjs`).
- **TS extractor** — API Extractor `.api.md` or a `ts-morph` script, a new sibling in
  `parse-that/typescript/scripts/*.mjs` alongside the extant surface gates.
- **The differential harness** — generalize `isomorphic_path_error.rs`: lift its inline
  `json_registry()` fixture (`:33-83`) and the shared `crates/bbnf-path-ts/src/fixture.rs` into a
  corpus consumed by both a Rust test and a vitest test.
- **The crossing ABI** — `wasm-bindgen` + `serde-wasm-bindgen`, already the transport for the
  bbnf-path bridge (`schema.rs:25` `serde(tag="kind", content="value")`; `ts/path.ts` round-trips
  the JSON-serialized payload).

---

## 4. Integration seams (path:line — verified this repo unless marked)

- **The one LIVE mirror pair (the seed):** `crates/bbnf-path/src/{lib,registry,path_macro}.rs` ↔
  `crates/bbnf-path-ts/src/{lib,schema,compile,fixture,template_tag}.rs` +
  `crates/bbnf-path-ts/ts/{path,index}.ts`. `schema.rs:1-15` declares the mirror contract in prose;
  `ts/path.ts` carries the matching TS interfaces (`PathErrorReason`, `PathErrorPayload`).
- **The prototype differential linter (T3 seed):** `crates/bbnf-path-ts/tests/isomorphic_path_error.rs`
  (268 LOC) — five differential cases + `reason_variants_are_isomorphic:227-268` (the exhaustive
  enum-parity test T1 generalizes from hand-written to manifest-driven).
- **The tape byte seam (T0):** four public plane accessors on the runtime tape at
  `crates/core/src/runtime/tape/mod.rs` (`source/offsets/flag_cursors/flag_values`) — the wasm
  export shape PROVE-14 proved (its REPORT cites the pre-move path `crates/runtime/src/tape/mod.rs`;
  the tree has since consolidated under `crates/core/`). GN-4.W4 gate (b) productionizes it. Note:
  `crates/bbnf-wasm` and `ts/tape/` are ABSENT (PLANNED, GN-4.W4).
- **The combinator core (the hard case — the "reasonable point" measured here):**
  parse-that Rust `rust/parse_that/src/` — `parse.rs` (214 LOC, the `Parser` struct + entry
  methods, `pub fn call/parse/save_state/trim_whitespace/eof …`), `leaf.rs` (370 LOC), and a
  **`combinators/methods/` directory module** + `span_parser/` — ↔ TS `typescript/src/parse/` —
  `parser.ts` (711 LOC, the class AND every combinator method FLAT), `leaf.ts` (399), `state.ts`,
  `packrat.ts` (R4 §2). The file trees are genuinely NOT isomorphic: Rust partitions methods into a
  dir-module; TS keeps them flat in one class. The name map records `Parser::then ↔ Parser.then` at
  ITEM granularity WITHOUT forcing file-tree identity — this is exactly 6A's soft-warn/hard-RED
  split (risk 2). (Separate repo — cross-repo CI ownership is open question c.)

---

## 5. Prior art (citations)

- **Kotlin Multiplatform `expect`/`actual`** — the archetype. The compiler enforces that every
  `expect` in common has a matching `actual` (same name/params/return) in every platform source
  set, bodies idiomatic per platform. https://kotlinlang.org/docs/multiplatform-expect-actual.html —
  6A = `expect`/`actual` across two repos/languages with an external linter as the missing compiler.
- **Microsoft tsc-go (typescript-go)** — a file-by-file port of the TS compiler from TS to Go,
  validated by the *conformance suite*, not by layout inspection. The winning play was mirror
  structure + a behavioral oracle *together* — precisely 6A's T1+T3 layering, and the reason T3 is
  non-optional. https://github.com/microsoft/typescript-go
- **rustc `tidy` (`src/tools/tidy`)** — a structural linter run in-CI over the tree that enforces
  cross-tree invariants (no god-files past a line budget, feature-gate discipline, forbidden
  patterns). The direct precedent for "isomorphism as a tidy check in the normal gate lane," and for
  encoding the colocation/no-god-module edict INTO the linter (risk 4, gate iv).
- **cargo-public-api / cargo-semver-checks** — rustdoc-JSON-derived public surface, committed and
  CI-diffed to catch surface/semver drift. https://github.com/obi1kenobi/cargo-semver-checks — the
  T2 Rust extractor.
- **Microsoft API Extractor / ts-morph** — the TS analogue: an `.api.md` surface report committed
  and diffed. The T2 TS extractor.
- **Architecture fitness functions / ArchUnit / import-linter** — architecture rules expressed as
  ordinary tests (package correspondence, naming, dependency direction) run in the normal pipeline
  (Ford, *Building Evolutionary Architectures*). https://www.archunit.org — the linter-as-test cost
  model 6A adopts (no separate CI stage).
- **cbindgen / ts-rs / typeshare** — the GENERATION alternative 6A deliberately rejects: it would
  forfeit idiomatic hand code. 6A pins shape with a linter instead of minting the surface.
- **wasm-bindgen + serde-wasm-bindgen** — the crossing ABI already in use by the bbnf-path bridge.

---

## 6. Named risks & counterexamples

1. **Silent behavioral asymmetry under matching structure — PROVEN LIVE, 6A's central caution.**
   `isomorphic_path_error.rs:123-140` documents that `$.statuses[0].nope` yields `UnregisteredRule`
   in the offline checker but `UnknownField` via the cdylib — IDENTICAL field shapes, DIVERGENT
   semantics (offline leaves `TypeDesc::Named(_)` intact; the cdylib resolves it eagerly through the
   in-registry `layout(_)`). T1/T2 pass green here; only T3 catches it, and only if the corpus
   contains that input. Consequence: T1–T2 are necessary but NOT sufficient; T3 is mandatory and its
   corpus owes a coverage argument — which is exactly a behavioral-corpus's content. 6A is
   necessary-not-sufficient without T3.
2. **The trees genuinely diverge in partition.** parse-that Rust `combinators/methods/` (dir-module)
   vs TS flat `parser.ts` (711 LOC). A linter demanding file-tree identity fights "code idiomatic
   per language" and loses. Mitigation: the manifest maps at ITEM granularity; file-tree
   correspondence is a SOFT warn where item-map is a HARD RED.
3. **Name-map staleness — the manifest is itself drift-prone.** The prose `mirror of bbnf::path::*`
   comments already ARE an unchecked name map that rots. Mitigation: make the manifest the ONLY map,
   delete the prose, and fail the linter when any surface item lacks an entry (bidirectional
   totality). Reinforced by §2: derive generable names from the grammar so they cannot go stale.
4. **Idiom allow-list inflation (a NEW, self-inflicted failure mode).** The linter must allow-list
   the genuine one-sided idioms — Rust-only `bump_slab`/`span_parser`/bespoke-DFA and TS-only
   `packrat`/native-`RegExp` (R4 §3: no bespoke TS regex engine; §6g: packrat opt-in). But an
   unbounded allow-list makes the linter vacuous — everything becomes "exempt." Mitigation: BUDGET
   the exemption ratio; the linter goes RED (or warns loudly) when the exempt fraction grows, and
   the allow-list itself is a reviewed, committed artifact. Over-mirror-through-the-idiom-seam
   (forcing a TS `RegExp` twin of the Rust DFA) is equally refused — the allow-list is bidirectional.
5. **Toolchain cost.** `cargo-public-api` needs nightly rustdoc-JSON; API Extractor needs a TS build.
   If the greenfield workspace is stable-only this is a NAMED blocker; if nightly is already
   tolerated it folds into the proof-gate lane. Measured, not assumed (open question b).
6. **Two-repo boundary (real, not rhetorical).** bbnf-path is in-repo; parse-that Rust+TS is a
   SEPARATE repo (only `docs/parse-that/*.md` present here). No single CI owns both. Mitigation:
   per-repo manifest fragments + a cross-repo conformance corpus vendored the way parse-that's regex
   is already vendored into bbnf-lang.
7. **Consumer-less substrate (Y.13, clause viii).** A name map with no linter, or a linter no CI
   runs, is forbidden. The linter's consumer is the CI gate; the manifest's consumer is the linter;
   both land in the wave that mints them.

---

## 7. What a prototype must test to move this family

1. **Name-map linter MVP on the one LIVE pair.** Author the `.bbnf`/manifest map for `bbnf-path` ↔
   `bbnf-path-ts`, a Rust extractor (cargo-public-api/rustdoc-JSON) and a TS extractor (ts-morph),
   and a checker proving the map is TOTAL and consistent. PASS = it mechanically reproduces what
   `schema.rs`'s prose asserts; the falsifiable core = it goes RED when a single field/variant is
   renamed on one side only. Deliverable: delete the `schema.rs:1-15,19,37,52,70` prose once the
   linter subsumes it (proves it is the ONLY map).
2. **Catch the known asymmetry (the counterexample-driven proof).** Feed the linter the
   `$.statuses[0].nope` divergence: confirm T2 passes while behavior differs (structure is
   necessary-not-sufficient), then show T3's differential corpus catches it. This is what most
   sharply distinguishes 6A from behavioral-only AND from structural-only.
3. **MEASURE the clean-map ratio on the combinator core — 6A's "where reasonable ends" NUMBER.**
   Build the item-granularity map for parse-that Rust `combinators/methods/` + `parse.rs`/`leaf.rs`
   vs TS `parser.ts`/`leaf.ts`, and measure: fraction of items mapping 1:1 vs needing an idiom
   escape hatch, and the exemption-ratio (risk 4). A high clean ratio validates 6A; a low one means
   the trees are too idiomatic to mirror and the family weakens. This ratio IS the constitution's
   demanded measured evidence and is currently UNMEASURED (separate repo).
4. **Compose the byte tier (T0).** Reproduce GN-4.W4 gate (b) — wasm-exported tape planes
   byte-identical to the `tape_dump` fixture — as a standing linter tier over
   `crates/core/src/runtime/tape/mod.rs`, proving byte-mirror and name-mirror coexist in one plane.
5. **CI budget.** Time the linter cold (dual surface extraction + name-map diff + differential
   corpus) and show it fits the proof-gate lane (parse-that `proof:all`; rustc-`tidy` cost model).
   If nightly rustdoc-JSON is required and the greenfield is stable-only, report it as a HARD blocker
   with the exact toolchain demand.

---

## 8. The family in one line

6A pins the rust/ts plane with a grammar-anchored, checked-in name map and a four-tier linter
(name → surface → behavior, plus a byte tier for the tape), keeping both trees hand-written and
idiomatic — Kotlin Multiplatform's `expect`/`actual` across two repos, the linter as the compiler
that spans no single toolchain. Its strongest evidence is the measured clean-map ratio on the
combinator core and the proof that structural pinning catches the shape drift a behavioral-only
corpus lets slip (the LIVE `$.statuses[0].nope` asymmetry). Its decisive tax is the linter, the
allow-list budget, and the two-repo CI seam; its floor obligation is that T3 is mandatory because
T1/T2 alone are provably necessary-not-sufficient.
