# LOCK-6 — THE ISOMORPHISM PLANE (rust/ts split): FAMILY SPECS (synthesis, pass 1)

*Synthesis, Fable-tier, pass 1. One cogent spec per family while the routes remain
incompatible, then the incompatibility map. Grounded against the extant tree, not only the
research prose: seams marked LIVE were verified present this session; seams marked PLANNED were
verified absent (GN-4.W4 substrate). Elegance earns nothing on its own — a family stalled at a
missing primitive equal in difficulty to the original problem is marked BLOCKED-candidate.*

---

## 0. The plane's coordinate system (the measured facts every family must consume)

LOCK-6 is not a blank design space. Six data points — five measured, one legal — pin the plane
before any family speaks. They are the prior-art sweep §0, verified in-tree:

- **F1 — the wasm/JS boundary is the cost center, not the Rust core.** PROVE-14 measured the TS
  tape *full-walk* at ~0.9–0.98× native `JSON.parse` (not the hoped 2×); the field agrees (a
  Rust-WASM JSON parser measured 25% *slower* than TS because every string crossing re-encodes).
  Any "wasm is the one impl" claim that routes per-value materialization across the boundary is
  refuted before it is written.
- **F2 — the win that crosses is stop-early, not compute.** PROVE-14 got 4.4× on stop-early
  `get` because the tape skips `statuses[1..99]`. Cross the **layout** once (offset planes as
  typed-array views), never per value.
- **F3 — the tape is already an Arrow/Cap'n-Proto-class zero-copy layout.** Four SoA planes
  (`offsets:u32, flag_cursors:u32, flag_values:u8, source`) decode byte-exactly node-for-node in
  TS. Seam: `crates/core/src/runtime/tape/mod.rs` (four public accessors). This byte-identity is
  a *requirement*, not a preference — it is what makes F2 zero-copy.
- **F4 — the TS side runs V8 laws, not LLVM laws.** Hidden-class stability, arity-unrolled
  monomorphizations, `Int8Array` dispatch tables exist because V8 rewards them and an
  LLVM-idiomatic Rust mirror would omit them. Line-for-line *code-shape* mirroring is in tension
  with V8 idiom; only *tree/name* mirroring is cheap.
- **F5 — the type plane costs zero at runtime, both sides (Constitution v).** Any scheme adding a
  runtime cast or marshalling shim to satisfy types is a defect.
- **F6 (legal) — parse-that is READ-ONLY to bbnf; the letter is the only artifact across the
  seam.** No single CI owns both trees; no bbnf agent runs parse-that's generator. Every
  mechanism must survive a repo boundary that tooling cannot dissolve.

**The one live mirror pair (the seed for all four families).** `crates/bbnf-path` (Rust
proc-macro) ↔ `crates/bbnf-path-ts` (cdylib + wasm-bindgen + `ts/path.ts`). The mirror contract
is carried today as **prose** (`schema.rs:1-15`: "byte-identical under serde-wasm-bindgen",
"field-for-field identical") — an unchecked, drift-prone name map. The differential oracle is
LIVE at `crates/bbnf-path-ts/tests/isomorphic_path_error.rs` (268 LOC: five differential cases +
`reason_variants_are_isomorphic:227-268`, an exhaustive 8-variant enum-parity test). Every family
below is a different answer to "what replaces the prose."

---

## 1. Family 6A — MIRROR-TREE (hand-authored both sides + a CI isomorphism linter)

**Thesis.** Both trees stay hand-written and idiomatic per language; a checked-in,
machine-readable **name map** is the single source of truth for what mirrors what; a CI linter
proves the map holds against both *extracted* surfaces. Kotlin Multiplatform `expect`/`actual`
reconstructed across two repos and two languages, the linter standing in for the single KMP
compiler no toolchain here provides.

**Architecture — four tiers, each mirror pair declares which bind it.**
- **T1 name/tree** — a manifest enumerates mirror pairs + an item-granularity map `(rust_path,
  ts_path, {RustItem→tsItem})`; the linter asserts bidirectional totality (every mapped item
  exists on both sides AND no surface item lacks an entry — a silent omission is RED).
- **T2 surface** — each module's public surface extracted to a committed snapshot, diffed
  *through* the name map (item set, field names, variant sets, arities). Rust:
  `cargo-public-api`/`cargo-semver-checks` (rustdoc-JSON). TS: API Extractor `.api.md` or a
  `ts-morph` script.
- **T3 behavioral** — one shared fixture set drives both surfaces, compared field-for-field.
  Already LIVE in prototype at `isomorphic_path_error.rs`.
- **T0 byte** — the offset-tape mirrors at the byte level (stricter than any name map); the
  born-RED GN-4.W4 gate (b), composes over T1–T3.

**Mechanism / integration seams.** Promote `schema.rs`'s prose to a machine-checked manifest and
**delete the prose duplicates**. Grammar-anchor the map (Constitution i): rule/variant names are
already shared truth emitted by `crates/core/src/backend/emitter.rs`; the hand-authored manifest
carries ONLY the idiom residue the grammar cannot name (`Parser::then ↔ Parser.then`). The linter
lands in parse-that's existing proof-gate lane (`proof:*` node scripts, `manifest-gate.mjs`);
rustc `tidy` is the cost model (structural lint in the normal gate lane).

**Gates it carries.** (a) linter goes RED when a single field/variant is renamed on one side
only; (b) bidirectional-totality RED on any unmapped surface item; (c) item-map is HARD RED,
file-tree correspondence is SOFT warn (the trees genuinely diverge: parse-that Rust
`combinators/methods/` dir-module vs TS flat `parser.ts` 711 LOC); (d) exemption-ratio budget —
the allow-list of one-sided idioms (Rust `span_parser`/bespoke-DFA, TS `packrat`/native
`RegExp`) is a reviewed committed artifact and the linter warns when the exempt fraction grows.

**Exact open questions.** (i) **The clean-map ratio on the combinator core** — fraction of items
mapping 1:1 vs needing an idiom escape hatch, on parse-that Rust `parse.rs`/`leaf.rs`/
`combinators/methods/` vs TS `parser.ts`/`leaf.ts`. UNMEASURED (separate repo, F6). This ratio IS
the constitution's "where reasonable ends" number; a low ratio weakens the family. (ii)
nightly-rustdoc-JSON toolchain demand vs a stable-only greenfield (HARD blocker if stable-only).
(iii) cross-repo CI ownership (F6): per-repo manifest fragments + a vendored conformance corpus.

**Disposition: ADVANCED, but strictly dependent on 6D.** 6A's own risk 1 proves T1/T2 are
*necessary-not-sufficient*: the LIVE `$.statuses[0].nope` asymmetry passes T1/T2 green
(identical variant set, identical payload shape) while behavior diverges. 6A therefore *contains*
6D's T3 as a mandatory tier — it is "6D plus a structural tax," never a standalone.

---

## 2. Family 6B — SPEC-GENERATED-SKELETON (one spec generates both skeletons)

**Thesis.** The isomorphism problem has a declarative half (drift-prone, low value-per-line, high
correctness stakes: layout constants, tape byte-contract, type interfaces, oracle tables) and an
imperative half (idiomatic, V8/LLVM-law-bound: the decode walk, the SIMD skip engine, the
monomorphic hot loop). **Generate the declarative half from one spec; hand-write the imperative
half; draw and measure the boundary.** Drift is not detected after the fact (6A) — it is made
*impossible* for the generated surface, because both faces are outputs of one generator run.

**Architecture — three generated faces, one hand face.**
- **Face I — layout contract (100% generated, 0 hand lines).** The node-kind alphabet is
  derivable from the grammar's structural node kinds; `STRING_NEEDS_DECODE` is a grammar-derived
  fact reachable from `crates/ir/src/passes/materialization.rs` (`MaterializationClass`, already a
  CSP variable). One new `Emitter` method `emit_tape_layout(&LayoutSpec, ctx)`: Rust emits `const
  K_OBJ_OPEN: u8 = 1;` into `runtime/tape/record.rs`; wasm emits plane-export names+strides; TS
  emits the const block at the head of `tape.ts`. This face is where drift *actually lives* —
  the AoS `record.rs` dialect and the SoA offset-tape are TWO hand-transcribed alphabets today.
- **Face II — type interfaces (partly built).** `ParserState`/`Span`/`Value`-variant interfaces
  already emitted for TS (`backend_ts.rs`, `bbnf-path-ts/src/schema.rs`). Addition: the
  `TapeDoc` field decls as a **skeleton** (ctor-fixed order — the load-bearing V8 detail).
- **Face III — oracle table (highest-leverage new face).** Generate the FNV digest comparator +
  structural-count table into BOTH a Rust `value_parity.rs` fixture AND a TS
  `tape-parity.test.ts`, so "same value, node-for-node" is true by construction of the
  comparator; the four lane-entries become four distinct generated call sites — GN-4.W5's R5
  "oracle stubs itself (`get==get`)" becomes *unwriteable*.
- **The hand face (never generated).** The decode hot loop, idiomatic per language, riding the
  generated skeleton: Rust NEON `skip_container`, TS `TapeDoc.walk` monomorphic cursor recursion.

**Mechanism / integration seams.** The spec is NOT a new artifact — it IS `GrammarIR` +
`MaterializationClass`; `LayoutSpec` is a *derived view* computed once in the shared driver
(`crates/core/src/backend/driver/mod.rs`), never serialized (honors "no-ts-ir" + parsimony). The
existing multi-backend spine already proves the pattern: `Emitter` trait
(`emitter.rs:31`, ~60 `emit_*`), three faces (`backend/{rust,wasm,ts}`), and a live isomorphism
gate `backend_ts_typecheck.rs` (`tsc --noEmit --strict` over four grammars). `bbnf-path`/
`bbnf-path-ts` is a literal 6B instance already shipping (both consume the same `bbnf-ir`
`StructRegistry` + `bbnf-regex` lexer ⇒ isomorphic by construction). **6B's real work is
completion: extend the Emitter to cover the three faces it does not yet generate.**

**Gates it carries.** (a) lockstep regen — one spec-line edit + `xtask regen` moves all three
faces together while PROVE-14 byte-exact parity (6 counts + `codeUnits` + fp 986912547) holds; a
hand-injected `tape.ts` drift fails a generated-const assertion; (b) `%HaveSameMap` V8 shape probe
(`node --allow-natives-syntax`) proving the generated skeleton is single-hidden-class shape-stable
(R1: a generated field-by-field init churns a hidden class and loses the 4.4× stop-early); (c)
the born-RED cross-check (GN-4.W4 gate b) — generated wasm planes byte-identical to the
`tape_dump` fixture, catching an AoS/SoA reconciliation that drifts.

**Exact open questions.** (i) **The reasonable-point NUMBER** — generated-LOC vs hand-LOC per face
across JSON + one more grammar (CSS-L4/Sheets); the plateau where generated coverage saturates IS
the "reasonable point." The null hypothesis R3: if the declarative surface is only ~40 constants +
two interfaces, a 6A linter is cheaper — 6B then honestly narrows to the oracle face alone. (ii)
cross-repo regen (F6): a 6B pipeline cannot run parse-that's generator; the generated artifact
ships *inside the letter*, regenerated parse-that-side — 6B relocates drift-prevention into the
regen gate, it does not dissolve the boundary.

**Disposition: ADVANCED on the declarative/tape/oracle plane; REFUTED on the combinator type
algebra.** Prior art is decisive and independent: `ts-rs`/`typeshare`/`specta` are documented to
*flatten* conditional types — "generated types are simpler than hand-written equivalents." PT-E2's
precise `infer V` inference (`all(a,b) → Parser<[A,B]>`, zero-cost) cannot be derive-macro'd. 6B's
line is principled: generate the regular data planes; hand-write the V8/conditional-type idiom.

---

## 3. Family 6C — WASM-CORE (the Rust core compiled to wasm is the one implementation)

**Thesis.** One Rust implementation parses source to the tape inside wasm; TS is bindings + an
idiom layer; TS-native code exists only where V8 measurably wins. The boundary is **data, not
behavior — and the datum is the tape.** The four planes cross once, in bulk, as typed-array views;
TS-native code walks them. This is the shape PROVE-14 already certified (row 14): stop-early get
at 4.4× *because* the walk stays TS-native and the boundary is crossed once.

**Architecture — the value-fabric split.**

| stage | home | why |
|---|---|---|
| parse source → tape (SIMD scan, structural classify) | **wasm-core** | batch, one crossing, no consumer awaits mid-parse |
| tape planes → JS typed arrays | **the binding** | one `wasm-bindgen` export, zero-copy views |
| classify / lazy `nextSibling` walk / stop-early get / string decode | **TS-native idiom** | V8 measurably wins (4.4×, boundary-free) |

"One implementation" is honest for the **parser + regex automata + solver** (batch, comptime,
async-tolerant). It is *not* honest for the walk — the walk is TS-native by *measurement* (row
14), not by concession.

**Mechanism / integration seams.** The archetype is LIVE: `bbnf-path-ts` (cdylib + serde payload
+ the differential test that IS the drift gate). The shipped core is `wasm/` (`bbnf-wasm` v0.1.0,
cdylib delivered to `playground/src/wasm/`). **Homing collision (a real 6C decision):** GN-4.W4
specifies a *new* `crates/bbnf-wasm`, but a `bbnf-wasm` already exists at `wasm/` — two cdylibs
mean two `.wasm` downloads + two instantiations. 6C's strongest form lands the tape export as a
*module inside* the extant crate. Boundary discipline: cross at the plane; the walk is TS-native;
strings return `offset+len` into the already-crossed `source` array, decoded TS-side.

**Gates it carries.** (a) memory-growth view-invalidation — two increasing-size parses through one
instance holding a live typed-array view must not corrupt the first (a `memory.grow()` detaches
every outstanding view — a hazard NOT in GN-4.md's R4); copy-out per parse or re-fetch views; (b)
per-node-boundary-tax refusal — no API crosses per tape node (29,573 crossings loses
catastrophically — the full-walk lesson generalized); (c) cold-instantiate counted like cold parse
(warm-bench-is-disinformation law), doubled by two cdylibs.

**Exact open questions — the two forces that bound the plane.**
- **R-A (the killer, decisive): does the NEON win cross to wasm?** The certified SIMD kernels are
  aarch64 intrinsics; `portable_simd`→wasm32 does NOT inherit the win — wasm `simd128` is an
  optional extension gated by `-C target-feature=+simd128` with no runtime detection, and CLMUL
  (the prefix-XOR path) is absent. In-tree, `crates/simd-scan` already carries a **separate
  hand-written `wasm.rs` SIMD128 twin** of `neon.rs`, with a documented CLMUL→shift-XOR fallback
  divergence — i.e., the hot kernel is ALREADY a second implementation. Whether it keeps the
  throughput is UNMEASURED (Brief 1).
- **R-E (confining, hard): synchronous ESM consumers cannot await a module.** value.js/latex-paper/
  keyframes call `all()`/`dispatch()` synchronously (value.js has 59 sync `all()` sites); a
  wasm-core parse-that forces async `WebAssembly.instantiate`. This is not a tax — it is a
  consumer-contract violation. It confines wasm-core to the **bbnf side** (generated parser + tape
  + regex-automata + solver) and leaves parse-that's combinator backbone TS-native (PT-3).

**Disposition: BLOCKED-candidate as an independent primary family; ADVANCED only as a component.**
The maximal reading ("Rust wasm is THE impl") is refuted by F1 (full-walk ~0.9×) and the field
benchmark. R-E confines it to the bbnf side — it cannot govern the parse-that combinator plane at
all. Its *performance* premise is gated on R-A, and R-A's missing primitive — SIMD that keeps the
aarch64 win in wasm — is genuinely hard: absent it, the hot kernel becomes a second
implementation (as `simd-scan/wasm.rs` already is), re-instantiating on the hottest surface the
dual-maintenance the family exists to abolish. **6C survives only as the zero-copy *crossing
mechanism* for a 6B-generated, 6D-verified tape — not as the plane's primary answer.** If Brief 1
shows the wasm kernel matches native NEON, this softens to ADVANCED-scoped (bbnf batch plane).

---

## 4. Family 6D — ORACLE-ISOMORPHISM (behavioral isomorphism only)

**Thesis.** The rust/ts split is held equivalent by a shared conformance corpus + differential
tests ALONE — layout free per language. The isomorphism boundary is **data** (a corpus + a
canonical language-neutral digest), never code. This is the plane's **null hypothesis**: it wins
by default and is defeated only if measured drift/maintenance cost exceeds a 6A linter's or a 6B
generator's.

**Architecture — four coupled pieces.**
- **Canonical digest** — a SAX-like typed event stream over the grammar-derived Value alphabet
  (`ObjectOpen · Key · String · Number · …`), folded by a fixed endianness-neutral rolling hash.
  **The decisive design insight: encode numbers as the 8-byte big-endian IEEE-754 bit pattern,
  not decimal text** — Rust `ryu` and V8 `Number.toString` produce the same value but different
  text (`1e21` vs `1000...0`); bit-equality is language-agnostic and total, dissolving the entire
  number-canonicalization hazard for the digest lane.
- **Differential runner** — promote today's ad-hoc `spawn node` (`ts_node_execute.rs`, LIVE:
  node-required no-silent-skip gate) into a protobuf-conformance wire protocol (`{corpus_id,
  grammar, input} → {accept, digest, error_class?}`), so any impl (incl. a third-party reference
  or the LOCK-7 meta-circular oracle) plugs in without host edits; two-tier CI (committed golden
  digests fast lane, live spawn gated slow lane).
- **Shared corpus** — the extant JSON/CSS/CSV fixtures + third-party references
  (`sonic_rs_parity.rs`, `lightningcss_parity.rs`, LIVE) + a JSONTestSuite-style **three-way
  partition** (must-accept / must-reject / may-either) so impl-defined inputs don't flap.
- **Grammar-directed differential fuzzing (the teeth)** — the grammar IS the source of truth
  (Constitution i); sample it for valid + near-miss invalid inputs, coverage-guided (Zest/RLCheck),
  with delta-debugging shrink. Converts "wins unless we forget a test" into "wins because drift
  cannot hide"; the measurable proxy is *grammar coverage × drift-escape rate*.

**Mechanism / integration seams.** No invention — *consolidation*. Lift the scattered parity
asserts (`json_canonical_parity.rs`, `css_l4_canonical_parity.rs`, `sheets_expr_parity.rs`,
`serialize_roundtrip.rs` — the metamorphic seed for reference-less grammars like sheets) into ONE
corpus + ONE digest + ONE wire-protocol runner set. Feeds the GN-4.W5 four-way parity close gate
(already plan-of-record).

**Gates it carries.** (a) 100% digest agreement across Rust eager/stop-early/typed + TS `TapeDoc`
+ a third-party reference — PROVE-14 parity as a STANDING law; (b) the in/out-of-contract
partition DECLARED as data: values + accept/reject = in-contract; message wording + span
byte-encoding = out; **error class = a declared toggle** (the `nope` case); (c) three-way
accept/reject partition so the gate is stable, not flaky.

**Exact open questions — two numbers decide the family, both measured.** (i) **drift-escape rate**
— inject N deliberate divergences (off-by-one escape decode, dropped CSS token class, number-format
delta, astral-string mishandle, the `nope` error-class flip); measure the fraction caught by
static corpus vs coverage-guided fuzz, and cycles-to-detection. (ii) **marginal maintenance cost
per grammar** — add TOML (none in-tree) and reach parity under 6D alone; 6D wins iff this cost is
strictly below the hypothetical 6A/6B cost for the same grammar AND escaped-drift is bounded.

**Disposition: ADVANCED — the mandatory FLOOR, but not the whole answer.** 6D is compatible with
every family and REQUIRED by each (6A's T3, 6B's parity oracle, 6C's drift gate). But **pure 6D
(layout totally free) is BLOCKED-candidate as the whole answer for the tape plane**: "layout free"
directly negates the byte-identical SoA layout the zero-copy stop-early win (F2/F3) requires. 6D
wins as the *verification substrate*; it loses as the *whole answer* where the tape needs byte
identity to be zero-copy. Its blind spot (silent drift in un-exercised behavior) is real and is
exactly where a thin structural cap (6A/6B) earns its tax — but only on the classes the fuzzer
provably cannot reach.

---

## 5. The incompatibility map (which families cannot coexist, and precisely why)

The families are **not** four mutually exclusive answers. They are one mandatory floor (6D) and
three candidate structural caps (6A, 6B, 6C) that conflict **only on a shared surface**. Once
surfaces are partitioned the conflicts dissolve into a stratification — and *drawing that
partition is the constitution's "reasonable point," which is the next pass's central deliverable.*

**Genuine pairwise incompatibilities (same surface):**

1. **6A ⊥ 6B — exclusive per item.** Generate-vs-hand-write-and-lint cannot both govern the same
   item. A 6B-generated pair has no independent hand-authored twin for 6A's name-map linter to
   check — the linter would compare the generator's two outputs, a tautology (consumer-less
   substrate, Y.13). Conversely, 6A's hand-authored idiom residue has no spec to generate from.
   They coexist ONLY by partitioning the surface (declarative → 6B, idiom residue → 6A), and that
   partition line IS the deliverable. On the **combinator type algebra** specifically, 6B is
   independently REFUTED (§2), so that surface is 6A + 6D with no 6B option.

2. **6C ⊥ 6A and 6C ⊥ 6B — on absorbed surfaces.** Where 6C absorbs a surface into the wasm core
   (parser, regex automata, solver), there is NO second implementation: the TS side is a thin
   binding, not a twin to mirror (6A) or a skeleton to generate to (6B). 6C thus *excludes* 6A/6B
   inside the wasm boundary and *defers* to them outside it (the TS-native walk). 6C partitions
   the plane at the wasm seam rather than competing across it.

3. **6C ⊥ the parse-that consumer contract (family-vs-constraint, hard).** Synchronous ESM
   `all()`/`dispatch()` cannot await `WebAssembly.instantiate` (R-E). Non-negotiable; confines 6C
   to the bbnf side. This is why 6C can never be the *plane-wide* answer.

4. **6D-pure ⊥ the tape plane (family-vs-measured-requirement).** "Layout free per language"
   negates the byte-identical SoA layout that the zero-copy typed-array-view stop-early win (F2/F3)
   requires. 6D-*as-whole-answer* conflicts with the tape's measured requirement; 6D-*as-floor*
   does not. This is the precise sense in which the null hypothesis is defeated *for the tape* and
   survives *as verification*.

**Compatibilities (the stratification the incompatibilities resolve into):**

| surface | primary mechanism | verification |
|---|---|---|
| tape layout (4 SoA planes) | **6B** generates the byte contract, **6C** crosses it zero-copy | 6D digest + T0 byte gate |
| decoder skeleton / oracle tables | **6B** (grammar-defined) | 6D |
| value-materialization / walk | **TS-native** (6C-tempered: "where V8 wins"); skeleton optionally 6B-generated | 6D |
| combinator type algebra | **hand-written** (6B refuted); **6A** tree/name mirror | 6D behavioral |
| module tree / names | **6A** tree+name mirror (NOT code-shape — F4) | 6D |
| everything (behavior) | — | **6D** — the mandatory floor |

The reading the evidence forces: **6D is installed everywhere as the floor; 6B governs the
declarative/tape/oracle surface; 6A governs the module-tree/name/idiom-residue surface; 6C
collapses from an independent family into the *crossing mechanism* for the 6B-generated tape,
confined to the bbnf side.** The three "caps" are not rivals across the whole plane — they are
rivals only where their surfaces overlap (6A⊥6B on any single declarative item), and the next
pass's job is to fix that overlap by measuring the two reasonable-point numbers (6B §2 open-Q i,
6A §1 open-Q i).

**BLOCKED-candidate summary.** 6C-as-primary (BLOCKED-candidate: refuted maximal, confined by
R-E, perf premise gated on the hard R-A primitive). 6D-as-whole-answer (BLOCKED-candidate: layout
freedom negates the tape's byte requirement). 6A and 6B are ADVANCED, mutually exclusive per
surface, and each strictly dependent on the 6D floor.

---

## 6. The two sharpest uncertainties for the next pass

The next pass must resolve two hinge questions: **(Q1)** is 6C a real independent family, or does
it collapse into a wasm-crossing of a generated/mirrored tape? **(Q2)** is a structural cap
(6A/6B) ever sufficient alone, or is 6D a mandatory floor beneath every cap? The two briefs below
are the cheapest, most decisive, in-tree resolutions.

- **Brief 1 (bbnf-worktree) — the NEON-crossing decider (6C R-A).** Decides Q1: whether "one wasm
  implementation" earns a performance claim on the hot kernel, or whether `simd-scan/wasm.rs` is
  dead-weight second-implementation maintenance. Uniquely runnable because the wasm SIMD128 twin
  already exists in-tree.
- **Brief 2 (bbnf-worktree) — the structural-pin necessary-not-sufficient decider (6A T1/T2 vs 6D
  T3).** Decides Q2: converts the LIVE `$.statuses[0].nope` assertion into a measured
  green-structural / red-behavioral pair, proving (or refuting) that 6D is a mandatory floor and
  that 6A/6B alone are insufficient. Cheapest, fully in-tree, single `cargo test`.

(Full briefs returned in the structured output.)
