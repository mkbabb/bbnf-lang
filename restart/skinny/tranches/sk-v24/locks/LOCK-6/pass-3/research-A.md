# LOCK-6 · 6A MIRROR-TREE · pass-3 research-A

Family 6A is the hand-maintained-mirror route made **scoped**: two idiomatic module trees
(parse-that's `rust/parse_that/src` and `typescript/src/parse`, plus the bbnf tape face) kept
in correspondence by a CI **isomorphism linter** — a tree-correspondence check, a total name
map, and a bounded idiom-exemption budget. A checked-in name-map plus a **fixed-enum
exemption budget** governs the faces that carry a byte-layout or oracle-table contract; a
behavioral oracle backstops the names that must be authored on both sides; and engine
internals are conceded, by construction, to behavioral-only isomorphism (6D territory).
"Isomorphism to a reasonable point" (Constitution iii) is made concrete: the *reasonable
point* is the layout/oracle face, and it is drawn (grep-countable), not asserted. This report
makes the mechanism concrete against the live census, answers the exemption budget
fit-independently, traces representative names to origin, and assigns linter ownership.

---

## Census — the substrate the linter reads (grep ground truth)

Concept-normalized basename sets (dirs collapsed to concept; `parse`↔`parser`,
`combinators/methods/*`↔the `Parser` methods, `lib`↔`index`):

**Present on BOTH sides — the structural mirror T:** `state`, `leaf`, `lazy`, `debug`,
`split`, `utils`, the combinator core (`parse.rs`/`parser.ts`), `parsers/json`,
`parsers/csv`, `parsers/utils`, module-root (`lib.rs`/`index.ts`). **T = 11 mirrored
concepts.**

**Rust-only (13.5 KLOC src vs TS 2.65 KLOC — a 5.1× asymmetry):** `bump_slab.rs`,
`span_trait.rs`, `span_parser/{mod,leaves,methods,constructors,combinators,assertions,scan,
sep_by,wrap,span_scanner}.rs`, `scanners.rs`, `parsers/scan/{balanced,decode,digits,ident,
number,number_f64,number_simd,quote_parity,quoted,quoted_simd,structural_bitmap,ws_comment,
mod}.rs`, `regex/{generated,host,mod}.rs`, `parsers/eisel_lemire/{algorithm,table,mod}.rs`,
`parsers/css/{declaration,media,selector,specificity,types,value,mod}.rs`,
`combinators/macros.rs`.

**TS-only:** `packrat.ts`, `ansi.ts`, and barrels `{core,index,diagnostics,packrat-entry,
utils-entry}.ts` + `parsers/index.ts`.

Rust splits combinator methods into eight files
(`combinators/methods/{alternation,sequence,sep_by,repetition,map,minus,recover,mod}.rs`); TS
carries all 34 methods inline in one 711-LOC `parser.ts` (`parser.ts:81-707`). **File
granularity does not match; concept granularity does** — the linter operates on a declared
concept↔{paths} map, not raw paths. No isomorphism/name-map linter exists in either tree
today (verified: the only `isomorph` hits are unrelated e-graph code); 6A is greenfield
tooling.

---

## 0. Frame and domain (what 6A may and may not mirror)

The naive reading — mirror whole module trees rust↔ts — is refuted by the only two candidate
pairs in the tree:

- **The live pair (tape decode).** Canonical Rust: the four tape planes on `Tape`
  (`crates/runtime/src/tape/mod.rs` ~121-137 per GN-4.W4/PROVE-14 §Integration) plus the
  generated JSON runtime (`classify`, `next_sibling_cursor`, `JsonValue::pointer` at the
  greenfield `codegen/src/json_templates/view.rs:106-114,330,355-382`). TS mirror: PROVE-14's
  `crates/bbnf-bench/ts-tape/{tape.mjs (TapeDoc @410), load.mjs, pointer.mjs, verify.mjs}`,
  graduating to GN-4.W4 `ts/tape/src/{tape,load,pointer}.ts`. This face **is** mirrorable: a
  byte-layout contract crosses it and PROVE-14 proved node-for-node parity across 320 cold
  children (fp `986912547` identical).
- **The non-pair (parse-that engine).** Rust `rust/parse_that/src/` splits combinators into a
  directory module (`combinators/methods/{alternation,map,minus,recover,repetition,sep_by,
  sequence}.rs`), carries a whole retired-in-TS family (`span_parser/`, excised from TS at
  1.0.0 — S.H2 `*Span`), a bespoke regex engine (`rust/regex/`, `regex/generated.rs`,
  NFA/DFA), and NEON kernels (`parsers/scan/{number_simd,quoted_simd,structural_bitmap}.rs`).
  TS collapses every method into one class file (`typescript/src/parse/parser.ts` 711 LOC),
  uses **native `RegExp`** (`leaf.ts:317-369`; R4 §3 — no NFA/DFA anywhere under `typescript/`),
  and has no SIMD. A whole-repo mirror linter here is **all exemption** — the budget degenerates.

**The domain law for 6A:** the structural mirror is scoped to faces bearing a *layout or
oracle contract* (the tape planes, the flag channel, the `Value` enum's variant set, oracle
tables). Engine internals (combinator algebra, regex automata, kernels) are **out of the
structural mirror's domain** and are governed by behavioral oracles alone. This is not a
retreat; it is the honest locus of "reasonable point," and it is exactly where PROVE-14
already delivers byte-exact evidence.

---

## Q1 — The exemption budget: T, I, and a cap not fit to the count

**Mechanism.** Every source node (module, file, exported name) on a mirrored face is one of:
(a) a **pair** — bound to its opposite-language node by a row in a checked-in `mirror.toml`;
or (b) a **typed exemption** — a one-sided declaration naming one of `I` frozen *idiom
classes* and satisfying that class's grep-checkable predicate. A node that is neither is a
**hard error** (unmirrored surface). This makes the map incapable of hiding a new file: the
tree is greppable ground truth, the map only supplies pairing and class tags.

### The two inputs, by a rule independent of the observed exemption total

- **T (structural mirrors)** = count of two-sided rows in `mirror.toml` — rows with **both**
  `rust=` and `ts=` populated. Grep-countable: `grep -cE '^\[\[pair\]\]' mirror.toml` (schema
  guarantees a `[[pair]]` block iff both sides are non-empty). T is defined by the *pairing*,
  never by the exemptions, so it is orthogonal to the exemption total by construction.

- **I (idiom classes)** = count of blocks in a **frozen** registry `idiom-classes.toml`,
  `grep -cE '^\[\[idiom_class\]\]'`. I is a **design constant** (enumerated below), not a
  tally of which classes current exemptions happen to use. Adding a class is a reviewed
  constitutional edit (a supersession), never silent growth.

**The five idiom classes (I = 5; CASE is a transform, not a class — see below):**

| class | meaning | grep predicate over the *source tree* (not the exemption file) |
|---|---|---|
| `TYPEREP` | primitive/container substitution per a fixed `type-map.toml` (`u32`↔`Uint32Array`, `&[u8]`↔`Uint8Array`/`Buffer`, `enum K`↔`const K_*`+`classify`, `Option<T>`↔`T\|undefined`) | decl whose type pair ∈ type-map |
| `OWNERSHIP` | node existing only for Rust's ownership model | `struct \w+Builder`, `\w+Checkpoint`, `<'\w>`-lifetime types, `bump_slab`, `*_slab` (e.g. `crates/runtime/src/tape/{cursor.rs:TapeRef/TapeCursor, mod.rs:TapeStructBuilder/TapeCheckpoint}`, `parse_that/src/bump_slab.rs`, `span_parser/`) |
| `KERNEL` | platform-accelerated kernel with a scalar sibling | file/fn under a `neon`/`_simd` path or `#[cfg(target_arch=)]` (`parse_that/src/parsers/scan/*_simd.rs`, `structural_bitmap.rs`) |
| `SUBSTRATE` | a subsystem present one side because the other uses a host primitive | module root tagged `substrate_root` in the map (`rust/regex/` ↔ TS native `RegExp`) |
| `ORACLE-DIR` | decode-direction methods with no method-for-method twin because the tape is **built** in Rust and **walked** in TS (XL-D: runtime decoders survive as oracles) | TS-only method under a `*Doc`/decoder module tagged `@mirror(oracle)` (`classify`, `nextSibling`, `decodeString`, `walkJs` in `tape.mjs`) |

**CASE is excluded from I** because it is a *total, injective transform*, not a divergence:
`normalize_case(rust) == ts` (snake_case↔camelCase). It resolves pairs, consumes **zero**
exemption budget. Confirmed divergences it handles: `flag_cursors`↔`flagCursors`,
`next_sibling`↔`nextSibling`, `sep_by`↔`sepBy`, `look_ahead`↔`lookAhead`
(`parse_that/rust/.../combinators/methods/{sep_by.rs:16, minus.rs:94}` ↔ `parser.ts:569,365`).
The transform must be proven total+injective (a collision test is a gate — `next` vs
`next_sibling` must not both fold onto one camelCase name).

### The cap — structural, so it cannot be fit to the count

A scalar magic budget (`cap = k·I`) tuned to today's total is exactly the forbidden fit.
Instead the cap is **re-derived from the tree**:

```
cap = Σ_{c ∈ I}  |sites in the source tree matching class c's predicate|
legal ⇔ (#exemptions ≤ cap) ∧ (∀ exemption e: predicate[class(e)] holds at e's site)
```

The RHS is grepped from **source**, never from the exemption file. Properties the directive
demands, each satisfied:

1. **Reproduces the live count without being fit to it.** A well-formed `mirror.toml` has
   exactly one exemption per predicate-matching site, so `#exemptions = cap` identically — the
   number *falls out of the tree*, no constant is chosen.
2. **RED on a synthetic extra exemption.** An added exemption with no backing divergence site
   pushes `#exemptions > cap` (and its predicate fails to match any site) → RED. You cannot
   mint budget without minting real divergent code.
3. **Forbids growth at a defended count.** At steady state `#exemptions = cap`; the only way
   to add a legal exemption is to add a real divergence site (a new `*Builder`, a new
   `_simd.rs`), which raises `cap` by exactly the sites added — the budget tracks *real*
   divergence, never inflates. A *tight* predicate per class is the load-bearing invariant: a
   loose predicate inflates `cap` and lets drift hide, so each predicate is grep-verified
   against the actual tree in CI and reviewed on change.

**Prior art (the exemption-list pattern is standard SOTA):** rustc's `tidy`
(`src/tools/tidy`) carries explicit `EXCEPTIONS` allowlists and fails CI when the actual set
diverges from the declared list; Kotlin Multiplatform's `expect`/`actual` is a *compiler-level*
mirror linter — every `expect` declaration must have a per-platform `actual`, checked, with
idiomatic bodies (the single closest analog to 6A); Bazel `buildifier`/`unused_deps` and
Buck use allowlist-with-budget gates; `cbindgen` checks a generated-and-checked-in C header
for drift. 6A's novelty over these is the **typed, predicate-derived** budget (the cap is
computed from the tree, not hand-listed), which removes the maintenance rot those allowlists
suffer.

---

## Q2 — Two representative names on the clean spine, traced to origin

**(1) `offsets` / `flag_cursors` / `flag_values` / `source` (the four planes) — SINGLE source.**
Origin is the Rust `Tape` struct's fields (`crates/runtime/src/tape/mod.rs` ~121-137; the
legacy tree confirms the plane at `crates/core/src/runtime/tape/cursor.rs:210`). The name
crosses **once**, through the wasm-bindgen export ABI `TapeExport { offsets, flag_cursors,
flag_values, source }` (GN-4.W4 `crates/bbnf-wasm/src/tape_export.rs`), and reappears on the
TS side as `TapeDoc` constructor fields (`tape.mjs:413-416`) purely by `normalize_case`. Trace:
`Rust Tape field → wasm export field → CASE-transform → TS TapeDoc field`. **Provenance =
single; class = CASE (transform, zero budget).** This is the genuinely clean spine — a
bijection with one authoring site.

**(2) `classify` / `JsonNodeKind` (the byte→kind derivation) — DUAL-authored, oracle-enforced.**
The tape stores **no discriminator**; kind is re-derived from `source[offsets[cursor]]`
(PROVE-14 §Layout). Rust's `classify`/`at_cursor` lives in the generated JSON runtime; TS's
`classify(cursor)` (`tape.mjs:420`, `switch(this.classify(cursor))`) **re-implements the same
byte→kind switch independently**. There is no shared source and no method-for-method pairing —
the isomorphism is guaranteed *behaviorally* (fp parity), not by provenance. Trace: `grammar
kind set → Rust generated classify` **and** `→ TS hand-authored classify`, joined only by the
oracle. **Provenance = dual; class = ORACLE-DIR.** This name is the mirror-tree's honest
limit: a structural name-map records the correspondence but *cannot* prove the two bodies
agree — only the behavioral drive can (the crux the Q-evidence directive targets, and the seam
to 6D).

**(bonus, the sharpest) `STRING_NEEDS_DECODE = 0x01 = GRAMMAR_BIT0` — grammar-derived, hand-copied.**
Rust: `OffsetFlags::GRAMMAR_BIT0` (the grammar's bit assignment; skinny/greenfield tape). TS:
`const STRING_NEEDS_DECODE = 0x01; // OffsetFlags::GRAMMAR_BIT0` (`tape.mjs:348`) — a
hand-copied literal with a provenance comment. This is a **value-mirror of a grammar-derived
constant**: if the grammar renumbers the bit, the Rust side updates by regen and the TS
literal silently rots. This single name is the whole argument for Q3's provenance linter.

---

## Q3 — Ownership of the provenance / name-map linter

**Split the concern along where the name is *born*.** Grammar-derived names (`JsonNodeKind`,
`GRAMMAR_BIT0`/the flag bits, the plane layout, oracle-table row order) are **minted by the
emitter** — LOCK-3's tower (XL-B: LOCK-3 owns the middle-IR tower + decision-driver seat).
Cross-language correspondence (the `mirror.toml`, `type-map.toml`, casing transform, the cap)
is a **weave** concern — LOCK-6, the isomorphism plane.

**Proposal.**
- **LOCK-3 owns the provenance manifest** — a machine-readable byproduct of regen listing
  every grammar-derived symbol and its canonical value/layout (bit assignments, kind
  discriminants, plane order). This is the single source of truth; it exists *because* the
  names are grammar-derived (Constitution i). Emitting it satisfies "consumer-less substrate
  forbidden" (Y.13) — its consumer is the linter. Prior art: tree-sitter's `node-types.json`
  (codegen-emitted, consumed by every language binding); Protobuf/Cap'n Proto/FlatBuffers
  schema→descriptor; rustc's `sourcegen` up-to-date check.
- **LOCK-6 owns the name-map linter + `mirror.toml`.** It consumes LOCK-3's manifest and
  asserts: (i) the Rust emission matches the manifest, (ii) the TS mirror matches the manifest
  (value-equality for constants like `GRAMMAR_BIT0`, layout-equality for planes), (iii) the
  structural pair/exemption budget of Q1 holds. Homing the *tool* in LOCK-6 keeps the weave in
  the weave lock; homing the *manifest* in LOCK-3 keeps the truth where the name is born.

**Rationale against the XL-B tower frame.** A name born in the tower must be gated at birth
(the manifest is a tower output), but its *correspondence across languages* is not a tower
property — it is precisely the thing LOCK-6 exists to decide. This assignment also forbids the
name-gate anti-pattern (Constitution vi): the linter checks **provenance-equality against a
generated manifest**, never a hand-maintained name allowlist. The `GRAMMAR_BIT0` hand-copy
(Q2 bonus) becomes illegal-or-linted: either the TS constant is *generated* from the manifest
(preferred, per Constitution i grammar-totality) or the linter asserts `tape.ts`'s literal ==
`manifest.GRAMMAR_BIT0` at regen. The gate runs in CI at regen time; manifest ≠ TS-mirror is a
build break.

---

## Evidence — drive TS behaviorally against canonical Rust (the one live pair)

PROVE-14 already runs the exact drive the directive requires — **not** an intra-Rust compare:
Rust `parse_tape` builds the tape → `tape_dump.rs:78` dumps the four planes → TS `TapeDoc`
decodes them → `verify.mjs:55` folds a 32-bit fingerprint and compares to a `JSON.parse` walk
and to the dumped planes. Result (VERIFY, CONFIRMED): all six structural counts, `codeUnits
304329`, and fp `986912547` identical across 320 cold children; the 312 escaped strings decode
through the sparse flag channel to identical UTF-16. This behavioral drive is the backstop 6A
*needs* for the dual-authored `classify` (Q2#2) that no structural map can certify. The
mirror-tree is thus **two-layered**: structural map + typed budget on the layout face
(Q1/Q2#1), behavioral oracle on the semantic names (Q2#2), provenance manifest on the
grammar-derived constants (Q2#3).

---

## Substrate and integration seams (path:line)

- **Live-pair TS**: `crates/bbnf-bench/ts-tape/tape.mjs:410` (`class TapeDoc`), `:420`
  (`classify`), `:348` (`STRING_NEEDS_DECODE`/`GRAMMAR_BIT0`), `:587` (`nextSibling`),
  `verify.mjs:55` (oracle). Graduates to `ts/tape/src/{tape,load,pointer}.ts` (GN-4.W4).
- **Live-pair Rust**: `crates/runtime/src/tape/mod.rs` ~121-137 (plane accessors);
  legacy anchor `crates/core/src/runtime/tape/{cursor.rs (TapeRef/TapeCursor), mod.rs
  (TapeStructBuilder:58/TapeCheckpoint:74), record.rs (TapeRec:103)}` (OWNERSHIP-class exemplars).
- **Export ABI**: GN-4.W4 `crates/bbnf-wasm/src/tape_export.rs` (`TapeExport` — the single
  crossing point; the name-map's `type-map` for `Uint32Array`↔`u32` etc.).
- **Idiom-class exemplars (parse-that)**: OWNERSHIP `rust/parse_that/src/{bump_slab.rs,
  span_parser/}`; KERNEL `.../parsers/scan/{number_simd,quoted_simd,structural_bitmap}.rs`;
  SUBSTRATE `rust/regex/` ↔ TS `typescript/src/parse/leaf.ts:317-369`; CASE `combinators/
  methods/{sep_by.rs:16, minus.rs:94}` ↔ `parser.ts:{569,365}`.
- **Name-map + linter home**: colocated with `ts/tape/` (GN-4.W4 makes it its own home) —
  `ts/tape/mirror.toml`, `ts/tape/idiom-classes.toml`, linter under `xtask` (consumes the
  LOCK-3 manifest) or `ts/tape/scripts/mirror-lint.mjs`.
- **Behavioral oracle production homes**: GN-4.W4 `ts/tape/test/tape-parity.test.ts`; W5
  `crates/runtime/tests/value_parity.rs` + `ts/tape/test/cross-lane.mjs` (four-way).

## Prior art (citations)

- **Kotlin Multiplatform `expect`/`actual`** — compiler-checked structural mirror with
  per-platform idiomatic bodies; the closest analog to 6A's map+budget.
- **rustc `tidy` (`src/tools/tidy`) EXCEPTIONS allowlists**; **rust-analyzer `sourcegen`
  up-to-date CI check** — the exemption-list-with-CI pattern (Q1).
- **tree-sitter `node-types.json`**, **Protobuf/Cap'n Proto/FlatBuffers schema→descriptor**,
  **LLVM TableGen `.td`** — codegen-emitted name manifests consumed by bindings (Q3 manifest);
  Cap'n Proto's canonical byte layout crossing languages is the direct tape analog. LLVM/TableGen
  is the *generate-both-sides* alternative 6A deliberately declines (that is 6B's route).
- **`cbindgen` generated-header drift check**, **regex-automata serialized DFA** crossing an
  ABI byte-identically — layout-crossing prior art.

## Named risks and counterexamples

1. **Map staleness** — the classic mirror-tree rot. Mitigated: T and exemption populations are
   computed *from the trees by grep*; a node in neither map nor a predicate-backed exemption is
   a hard error, so a new file cannot hide.
2. **Domain over-reach** — applying the structural mirror to whole parse-that is all-exemption
   and the budget degenerates (§0). Scope to layout/oracle faces; concede engine internals to
   6D. This is the "reasonable point," drawn.
3. **Dual-authored drift** — a structural map cannot certify `classify`'s two bodies agree
   (Q2#2). Requires the behavioral oracle; 6A alone is **insufficient** for semantic names and
   must carry the 6D backstop — stated, not hidden.
4. **Grammar-derived hand-copy rot** — `GRAMMAR_BIT0` copied into TS silently rots on a bit
   renumber (Q2 bonus). The Q3 provenance linter is *mandatory*, not optional.
5. **Non-bijective casing** — the CASE transform must be total+injective; a collision
   (`next`/`next_sibling`) is a gate failure, not a silent overwrite.
6. **Loose predicates inflate the cap** — a class predicate broader than its real sites lets
   drift hide under budget. Each predicate is grep-verified against the tree in CI and reviewed
   on change (the one place human judgment enters, bounded and auditable).

## What a prototype must test to move this family

1. **Budget mechanics.** Implement `mirror.toml` + `idiom-classes.toml` + `type-map.toml`;
   compute T, I, and `cap = Σ predicate-sites` from the trees; assert the live exemption count
   is reproduced with **no fitted constant**.
2. **RED on synthetic extra.** Inject a TS-only method with no backing divergence site; assert
   `#exemptions > cap` ∧ predicate-unbacked → RED.
3. **Defended growth is GREEN.** Add a real Rust-only OWNERSHIP type (`*Builder`); assert `cap`
   rises by exactly 1 and the exemption is legal — the budget tracks real divergence.
4. **Provenance gate.** Renumber `GRAMMAR_BIT0` 0x01→0x02 in Rust without touching TS; assert
   the provenance linter (manifest ≠ TS literal) **and** the behavioral oracle (fp divergence)
   both go RED.
5. **Behavioral drive at scale.** Extend PROVE-14's Rust-dump→TS-decode→fp-parity from
   twitter to citm/canada; confirm node-for-node isomorphism holds on the dual-authored
   `classify` across all three — the layer the structural map cannot reach.
