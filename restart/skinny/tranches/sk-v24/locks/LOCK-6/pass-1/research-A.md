# LOCK-6 / family 6A — MIRROR-TREE: hand-maintained mirrored trees + an isomorphism linter

Pass 1 research. The plane in question is the rust/ts split (bbnf-lang's cdylib bridges and
parse-that's dual Rust+TS core). 6A's thesis: both trees stay hand-written and idiomatic per
language; a checked-in **name map** is the single source of truth for what mirrors what; a CI
**linter** proves the map holds against both extracted surfaces. Structure is pinned; idiomatic
bodies float free. Behavioral parity is the backstop, not the front line.

---

## 1. The mechanism, made concrete

The linter is three tiers of increasing strength; a mirror pair declares which tiers bind it.

**Tier 1 — tree/name correspondence.** A machine-readable manifest (`isomorphism.toml`, or a
`.bbnf` fragment per constitution clause i) enumerates mirror pairs and an item-granularity name
map: `(rust_path, ts_path, map: { RustItem → tsItem })`. The linter asserts every mapped Rust
item exists, every mapped TS item exists, and — crucially — that **no item on either side lacks a
manifest entry** (totality; silent omission is RED, echoing the exhaustive-match discipline the
live code already uses at `crates/bbnf-path-ts/tests/isomorphic_path_error.rs:227-268`).

**Tier 2 — surface correspondence.** Each mirrored module's *public surface* is extracted to a
committed snapshot and diffed through the name map: identical item set, field names, enum variant
sets, arities. Rust side = rustdoc-JSON via `cargo-public-api` (git-committed `public-api.txt`,
CI-diffed). TS side = a `.api.md` report (Microsoft API Extractor) or a `ts-morph` script emitting
the same normalized surface JSON. The diff is not textual — it walks the name map and reports
`field renamed on one side only`, `variant present in Rust absent in TS`, `arity drift`.

**Tier 3 — behavioral correspondence.** A shared conformance corpus drives BOTH surfaces with
identical fixtures and compares outputs field-for-field. This tier already exists in prototype at
`crates/bbnf-path-ts/tests/isomorphic_path_error.rs` and must be generalized so ONE fixture set
feeds a Rust integration test and a vitest test.

**Tier 0 (byte, where it applies) — the tape.** The offset-tape (PROVE-14) mirrors at the *byte*
level, stricter than any name map: the wasm-exported planes must be byte-identical to a Rust dump
fixture. This is the born-RED cross-check already specified as GN-4.W4 gate (b). A name-map linter
is too weak here; the byte fixture is its own tier and composes with tier 1-3.

**Why this is not generation.** Tier 1's manifest is exactly Kotlin Multiplatform's `expect`
declaration; the idiomatic per-language bodies are the `actual`s; the linter stands in for the KMP
compiler that no single toolchain provides across two repos and two languages. Generation
(spec→both skeletons) would eliminate drift by construction but force a shared IDL and forfeit
idiomatic freedom on the generated surface — 6A deliberately pays a linter tax instead of a
generator tax, keeping both trees hand-authored.

---

## 2. The substrate

- **The name map** — one checked-in manifest, the ONLY name map. Today the correspondence is
  carried as *prose*: every payload in `crates/bbnf-path-ts/src/schema.rs:1-15,19,37,52,70` says
  "mirror of `bbnf::path::*`" in a doc comment — an unchecked, drift-prone name map. 6A promotes
  those comments to a machine-checked manifest and deletes the prose duplicates.
- **Rust extractor** — `cargo-public-api` (rustdoc-JSON). Snapshot committed per crate; the diff is
  a CI gate. Fits parse-that's existing proof-gate idiom (`proof:*` node scripts, `manifest-gate`,
  `subpath-gate`; R4 §4, R4:114-118).
- **TS extractor** — API Extractor `.api.md` or a `ts-morph` surface script, run as a new sibling
  in `parse-that/typescript/scripts/*.mjs` alongside the extant surface gates.
- **The differential harness** — generalize `isomorphic_path_error.rs`: lift its inline
  `json_registry()` fixture (`:33-83`) and the shared `fixture.rs` (`crates/bbnf-path-ts/src/
  fixture.rs`) into a corpus consumed by both a Rust test and a vitest test.
- **The crossing ABI** — `wasm-bindgen` + `serde-wasm-bindgen`, already the transport for the
  bbnf-path bridge (schema.rs:24-25 `serde(tag="kind")`, path.ts:129-136 round-trips the
  JSON-serialized payload).

---

## 3. Integration seams (path:line)

- **The one live mirror pair (the seed):**
  `crates/bbnf-path/src/{lib,registry,path_macro}.rs` ↔
  `crates/bbnf-path-ts/src/{lib,schema,compile,fixture,template_tag}.rs` +
  `crates/bbnf-path-ts/ts/{path,index}.ts`. `schema.rs` declares the mirror contract
  ("byte-identical under serde-wasm-bindgen", `:1-15`); `ts/path.ts:20-58` carries the matching TS
  interfaces (`OwnedPathSegment`, `TypedPath`, `PathErrorReason`, `PathErrorPayload`).
- **The prototype differential linter:** `tests/isomorphic_path_error.rs:123-268` — five
  differential cases plus `reason_variants_are_isomorphic` (`:227-268`), the exhaustive enum-parity
  test the tier-1 linter generalizes from hand-written to manifest-driven.
- **The tape byte seam:** four public plane accessors on the runtime tape (`crates/runtime/src/
  tape/mod.rs` `source()/offsets()/flag_cursors()/flag_values()`, ~`:121-137`) — the wasm export
  shape PROVE-14 proved and GN-4.W4 productionizes. Tier-0 byte fixture: GN-4.W4 gate (b).
- **The combinator core (the hard case):** `parse-that/rust/parse_that/src/` (`combinators/methods/`,
  `leaf.rs`, `parse.rs`, `span_parser/`, `scanners.rs`) ↔ `parse-that/typescript/src/parse/`
  (`parser.ts` 711 LOC flat, `leaf.ts`, `state.ts`, `packrat.ts`; R4 §2). The file trees are NOT
  isomorphic — Rust partitions methods into a directory module; TS keeps them flat in one class.
  This is precisely where "reasonable" is measured: the name map records `Parser::then ↔
  Parser.then` at ITEM granularity without forcing identical file partitions.

---

## 4. Prior art (citations)

- **Kotlin Multiplatform `expect`/`actual`** — the archetype: the compiler enforces that every
  `expect` in common has a matching `actual` (same name, params, return type) in every platform
  source set, while the `actual` bodies are idiomatic per platform.
  https://kotlinlang.org/docs/multiplatform/multiplatform-expect-actual.html — 6A = expect/actual
  across two repos/languages with an external linter substituting for the single KMP compiler.
- **cargo-public-api** — rustdoc-JSON-derived public-API surface, committed as `tests/
  public-api.txt`, diffed in CI to catch surface drift/semver breaks.
  https://github.com/cargo-public-api/cargo-public-api — the tier-2 Rust extractor.
- **Microsoft API Extractor / ts-morph** — the TS analogue: an `.api.md` report file committed and
  diffed. The tier-2 TS extractor.
- **ArchUnit / import-linter / architecture fitness functions** — architecture rules expressed as
  ordinary tests (package correspondence, naming, dependency direction) that run in the normal test
  pipeline. https://www.archunit.org/userguide/html/000_Index.html — the linter-as-test idiom and
  the "run alongside unit tests, no separate stage" cost model 6A adopts.
- **wasm-bindgen + serde-wasm-bindgen** — the crossing ABI already in use by the bbnf-path bridge.
- **cbindgen / ts-rs / typeshare** — the *generation* alternative 6A deliberately rejects (it would
  forfeit idiomatic hand code; 6A pins shape with a linter instead of minting the surface).

---

## 5. Named risks & counterexamples

1. **Silent behavioral asymmetry under matching structure — PROVEN LIVE.** `isomorphic_path_error.
   rs:129-138` documents that the deeper `$.statuses[0].nope` path surfaces `UnregisteredRule` in
   the offline checker but `UnknownField` in the cdylib — identical field shapes, divergent
   semantics. A tier-2 name-map diff passes green here; only tier-3's differential corpus catches
   it, and only if the corpus contains that input. Consequence: tier 1-2 are necessary but NOT
   sufficient; tier 3 is mandatory, and the corpus itself needs a coverage argument. This single
   counterexample is 6A's central caution and its justification for layering.
2. **The trees genuinely diverge in partition.** parse-that Rust `combinators/methods/` vs TS flat
   `parser.ts`. A linter demanding file-tree identity would fight the constitution's "code
   idiomatic per language" and lose. Mitigation: the manifest maps at ITEM granularity; file-tree
   correspondence is a *soft* warn where item-map is a *hard* RED.
3. **Name-map staleness — the manifest is itself drift-prone.** The prose `mirrors bbnf::path::*`
   comments already ARE an unchecked name map that can rot. Mitigation: make the manifest the
   ONLY map, delete the prose, and fail the linter when any surface item lacks an entry (totality).
4. **Toolchain cost.** `cargo-public-api` needs nightly rustdoc-JSON; API Extractor needs a TS
   build. If the greenfield workspace is stable-only this is a named blocker; if nightly is already
   tolerated it folds into the existing proof-gate lane. Must be measured, not assumed.
5. **Two-repo boundary.** bbnf-path is in-repo; parse-that Rust+TS is a separate repo. No single CI
   owns both. Mitigation: per-repo manifest fragments plus a cross-repo conformance corpus vendored
   the way parse-that is already vendored into bbnf-lang for the regex home.
6. **Consumer-less substrate (Y.13).** A name map with no linter, or a linter no CI runs, is
   forbidden. The linter's consumer is the CI gate; the manifest's consumer is the linter; both
   must land in the same wave that mints them.

---

## 6. What a prototype must test to move this family

1. **Name-map linter MVP on the one live pair.** Author `isomorphism.toml` for bbnf-path ↔
   bbnf-path-ts, an extractor pulling the Rust item surface (cargo-public-api/rustdoc-JSON) and the
   TS item surface (ts-morph), and a checker proving the map is total and consistent. PASS = it
   mechanically reproduces what the `schema.rs` prose asserts; the falsifiable core = it goes RED
   when a single field is renamed on one side only. (Delete the prose comments once the linter
   subsumes them.)
2. **Catch the known asymmetry.** Feed the linter the `$.statuses[0].nope` divergence: confirm
   tier-2 passes while behavior differs (proving structure is necessary-not-sufficient), then show
   tier-3's differential corpus catches it. This is the counterexample-driven proof that the tier
   layering is correct — the deliverable that most sharply distinguishes 6A from behavioral-only.
3. **Measure the drift/maintenance cost — the "where reasonable ends" number.** Build the
   item-granularity name map for the parse-that combinator core (Rust `combinators/methods/` vs TS
   `parser.ts`) and MEASURE the clean-map ratio: how many items map 1:1 vs need an idiom escape
   hatch. A high ratio validates 6A; a low ratio means the trees are too idiomatic to mirror and
   the family weakens. This ratio is the measured evidence the constitution demands.
4. **Compose the byte tier.** Reproduce GN-4.W4 gate (b) — wasm-exported tape planes byte-identical
   to the `tape_dump` fixture — as a standing linter tier, proving byte-mirror and name-mirror
   coexist in one plane.
5. **CI budget.** Time the linter cold (surface extraction both sides + diff + differential corpus)
   and show it fits the proof-gate lane (parse-that `proof:all`). If nightly rustdoc-JSON is
   required and the greenfield is stable-only, report it as a hard blocker with the exact toolchain
   demand.

---

## 7. The family in one line

6A pins the rust/ts plane with a checked-in name map and a three-tier linter (name → surface →
behavior, plus a byte tier for the tape), keeping both trees hand-written and idiomatic — the
`expect`/`actual` discipline of Kotlin Multiplatform reconstructed across two repos, where the
linter is the compiler that spans no single toolchain. Its strongest evidence is a measured
clean-map ratio on the combinator core and a demonstration that structural pinning catches the
shape drift a behavioral-only corpus lets slip (the `$.statuses[0].nope` asymmetry).
