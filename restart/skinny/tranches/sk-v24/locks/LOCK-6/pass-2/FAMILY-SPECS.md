# LOCK-6 — THE ISOMORPHISM PLANE (rust/ts split): FAMILY SPECS (synthesis, pass 2)

*Synthesis, Fable-tier, pass 2. One cogent spec per family while the routes remain incompatible,
then the incompatibility map. Grounded against the live tree this session — every seam below was
grepped in `/Users/mkbabb/Programming/bbnf-lang/crates` (the crates live at repo root, NOT under
`restart/skinny/`; the research reports' `crates/…` paths resolve there). Elegance earns nothing on
its own: a family stalled at a missing primitive equal in difficulty to the original problem is
marked BLOCKED-candidate. The plane is not four rivals — it is one mandatory behavioral FLOOR (6D)
and three candidate structural CAPS (6A/6B/6C) that conflict only where their surfaces overlap; the
deliverable is the partition line (§5), drawn by two reasonable-point numbers — 6A's clean-map ratio
(MEASURED 47/61/0) and 6B's gen-vs-hand LOC (still UNMEASURED).*

---

## 0. The coordinate system — pass-2 ground truth (verified in-tree this session)

Six measured/legal facts pin the plane (carried from pass 1: F1 boundary-is-the-cost-center ~0.9×
full-walk; F2 the-win-that-crosses-is-stop-early 4.4×; F3 tape-is-Arrow-class-zero-copy; F4 V8-laws-
not-LLVM-laws; F5 type-plane-costs-zero; F6 parse-that-READ-ONLY-the-letter-is-the-only-artifact).
Pass-2 adds five **verified** deltas that move the specs:

- **V1 — the LIVE mirror pair and its drift are exactly where research-A says.**
  `crates/bbnf-path` (proc-macro) ↔ `crates/bbnf-path-ts` (cdylib + `ts/path.ts`). The differential
  oracle `crates/bbnf-path-ts/tests/isomorphic_path_error.rs` is LIVE; the `$.statuses[0].nope`
  asymmetry is real at **lines 129–150** (canonical offline checker surfaces `UnregisteredRule`; the
  cdylib resolves `Named(_)` eagerly and surfaces `UnknownField` for the same input — the test *pins
  only where both agree* and documents the divergence), and the hand-rolled Rust↔Rust parity table
  `reason_variants_are_isomorphic` is at **line 228**. This is Brief 1's seed, confirmed.
- **V2 — 6B's grammar→layout-fact anchor is still phantom, and now its seams are stale too.**
  `STRING_NEEDS_DECODE` appears **nowhere** under `crates/` (grep, whole tree). Worse: the codegen
  seams both research passes cite — `DirectSchemaSet`, `grammar_profile.rs`/`COMPILED_RUNTIME_FILES`,
  `json_templates/config.rs`, `JsonNodeKind::at_cursor` — return **nothing** at those paths; the
  codegen/runtime/bench tree has been consolidated into `crates/core` since the research was written.
  What IS live: the `Emitter` trait at `crates/core/src/backend/emitter.rs`, the isomorphism gate
  `crates/core/tests/backend_ts_typecheck.rs` (`tsc --noEmit --strict`), and the pairing-linter
  descendant `crates/core/tests/recognizer_consumer_invariant.rs`. **6B is spec'd against a tree
  layout that no longer matches** — any 6B prototype needs a seam-relocation pass first.
- **V3 — the tape consolidated to `crates/core/src/runtime/tape/mod.rs`** (research-C/priorart cite
  the stale `crates/runtime/src/tape/mod.rs`; correct it downstream).
- **V4 — 6C's second kernel is LIVE and so is x86.** `crates/simd-scan/src/wasm.rs` exists alongside
  `neon.rs`/`scalar.rs` (the SIMD128 twin PROTO-1 measured at 0.34–0.47× native NEON), AND
  `avx2.rs`/`avx512.rs` are present — x86 SIMD in a probed crate despite constitution iv ("x86
  dead"). LOCK-9 surface; surface to GESTALT, do not bury.
- **V5 — three cdylib homes exist:** `crates/bbnf-path/Cargo.toml`, `crates/bbnf-path-ts/Cargo.toml`,
  and `wasm/Cargo.toml`. GN-4.W4's proposed `crates/bbnf-wasm` is absent; `wasm/` already claims the
  name — the homing collision 6C names is real.

---

## 1. Family 6A — MIRROR-TREE (hand-authored both sides + a CI isomorphism linter)

**Thesis.** Both trees stay hand-written and idiomatic per language; a checked-in machine-readable
manifest is the single source of truth for what mirrors what; a CI linter proves it holds against
both *extracted declaration surfaces* (never bodies). Kotlin-Multiplatform `expect`/`actual`
reconstructed across two repos, the linter standing in for the single compiler no toolchain provides.

**Architecture — the Q2 identity fork is a TRICHOTOMY, and it routes the linter (pass-2's central
advance).** The correspondence surface splits into three disjoint classes, each with a *different*
tautology status and a *different* lawful check — this dissolves the pass-1 "hand-authored-twice vs
grammar-derived" dilemma:

- **Class 1 — grammar-derived names** (registry field/rule names: `"statuses"`, `Document`/`Status`).
  ONE source: projected by `project_types` → `xtask regen` → `<ident>.registry.json`, consumed by
  both frontends (`bbnf-path/src/registry.rs`, `bbnf-path-ts/src/fixture.rs`). Name-checking these is
  tautological (both descend from one generator). **Route → PROVENANCE check** (both sides read the
  identical sidecar path/hash; flag any hand-authored `StructRegistry` fork) — anchored AND
  non-tautological because the proposition is "single-source held," not "name_A == name_B."
- **Class 2 — hand-authored-twice taxonomy** (`PathErrorReason` 8 variants + `PathError` 5 fields +
  `OwnedSegment` + `TypedPath` ≈ 19 items, authored on THREE homes: Rust canonical, Rust payload
  mirror `schema.rs:58-93`, TS union+class `path.ts:37-101`). This is the ONLY class where a name-map
  is non-tautological, and it is **where drift LIVES** — the `nope` case shows the 8 variant *names*
  correspond perfectly while the *routing logic* forked. **Route → NAME-MAP check** (types exist both
  sides; variant/field sets set-equal; the TS `reasonText`/`format` switch exhaustive over the union
  — closing the ungated TS gap the hand table `:228` never covers).
- **Class 3 — idiom residue** (`#[wasm_bindgen]` exports, `init()`, `makePathTag`, V8
  monomorphizations, native `RegExp`, lifetime-shape `Vec<&'static str>`↔`Vec<String>`). No twin.
  **Route → EXEMPTION BUDGET.**

**The Q1 RED line — a three-conjunct gate, not a raw fraction (pass-2's second advance).** A budget
that only "warns as it grows" cannot fail. The defensible gate:
1. **Mapped-set 0%-tolerance (hard RED):** any Class-2 item missing/renamed/added on one side → RED.
   Falsifiable: add a 9th `PathErrorReason` variant Rust-only → immediate RED (today's TS union rots
   silently).
2. **Allow-list monotonicity (creep RED):** a Class-3 item not on the reviewed allow-list → RED; a
   list entry whose declaration vanished → RED. Growth is legal only via an explicit reviewed diff.
3. **Impedance-derived integer cap (the number):** `MAX_EXEMPTIONS(crate) = 2·T + I` where `T` =
   transport entry-points (grep-countable `#[wasm_bindgen]`), `I` = declared idiom classes (a closed
   enumerated set: V8-monomorph, RegExp, lifetime-shape). For `bbnf-path-ts`: 2·2 + 2 = **6** = the
   observed count. Exceed → RED. Un-gameable because the multiplier is pinned to named seams, not to a
   re-labellable percentage. (The raw ~24–39% exemption *fraction* is a directional calibration only.)

**Mechanism / seams.** `syn` over the Rust mirror crates' declarations; the TS surface from built
`dist/*.d.ts` (declarations only — no TS runtime in the Rust gate) or `ts-morph`; the sidecar
manifest; a checked-in `mirror-map.toml` (correspondence + allow-list). Three passes, one per class.
On GREEN it *emits* the parity table `isomorphic_path_error.rs:228` hand-maintains, shrinking that
test to a generated fixture. T2 extraction is a proven-cheap mechanism: rustdoc-JSON emits arities +
full variant sets in 0.25s on parse-that's already-nightly pin (PROTO-2) — but scope to crate-local
`impl` items or blanket-std-trait noise floods.

**Gates.** (a) mapped-set 0%-tolerance RED; (b) allow-list monotonicity RED; (c) impedance cap
`≤ 2·T + I`; (d) file-tree correspondence SOFT-warn (trees genuinely diverge: Rust dir-modules ↔ TS
flat `parser.ts`).

**Exact open questions.** (i) **The mechanism is still UNBUILT** — only a python syntactic ratio
differ ran (PROTO-2); the manifest + T1 linter, the `schema.rs` prose deletion, and the TS-side
extractor never ran on the real surface (the rustdoc run was a 12-line toy twin). (ii) **The
green/red PAIR is unproduced** — "T1/T2 pass green on `$.nope` while T3 goes RED" is asserted about a
non-existent linter (Brief 1). (iii) Cross-repo CI ownership under F6 (per-repo manifest fragments +
vendored corpus) — proposed, unbuilt, the family's hardest logistics.

**Cross-lock (proposals).** LOCK-3 owns the Class-1 grammar-derived names (they are `project_types`
output); 6A owns only the *provenance invariant* over them — there is no grammar-derived *name map* to
double-home, only a single-source gate. The Rust-DFA↔TS-`RegExp` seam is a Class-3 exemption and is
inherently behavioral (TS has no NFA/DFA — native `RegExp` only), so a LOCK-5 ONE-HIR verdict that
forbids a *layout* mirror there is compatible: 6A never claims that seam as a layout mirror, only as a
behavioral correspondence routed to 6D. Freeze AFTER LOCK-1 (the ADT the map ranges over) + LOCK-7
(the tie-break oracle).

**Disposition: ADVANCED · dependent-on-6D.** The trichotomy + three-conjunct gate resolve pass-1's
two load-bearing gaps (vacuous budget; identity fork) at the *design* level — a genuine advance. But
the mechanism is SPEC_ONLY (only the feasibility ratio is measured), and 6A's own Class-2 finding
proves it is *necessary-not-sufficient*: names stay green while routing forks. 6A therefore *contains*
6D's T3 as a mandatory tier — "6D plus a structural tax," never standalone.

---

## 2. Family 6B — SPEC-GENERATED-SKELETON (one spec generates both skeletons)

**Thesis.** The isomorphism problem has a declarative half (drift-prone, low value-per-line, high
correctness stakes: layout constants, tape byte-contract, type interfaces, oracle tables) and an
imperative half (idiomatic, V8/LLVM-bound: the decode walk, the SIMD engine). Generate the
declarative half from one grammar-derived spec; hand-write the imperative half; draw and MEASURE the
boundary. Drift is not detected after the fact — it is impossible-by-construction on the generated
surface.

**Architecture — Q3 bounds the surface; the honest scope is generator-owns-schema + linter-guards-
constants (pass-2's advance).** Exhaustive census for JSON: the per-grammar layout surface is ~35
facts, of which ~15 (config constants, flag-bit assignments, the fixed 4-plane descriptor) are
**fixed policy a human authors once** — generating a 7-constant `config.ts` for one grammar in one
language is the over-build Q3 warns against; a per-name linter (the `recognizer_consumer_invariant`
pairing list extended TS-side) beats it. Generation earns its cost on: **(a) the schema/type face**
(rich, per-`->`, already a generated structure — extend it to a TS `interface`), and **(b) the
multiplication** — the classify table + config constants, trivial for JSON, grow per grammar across
`{json, css_l4, csv, toml, sheets} × {rust, ts}`. So 6B's defensible form: **generator owns the
schema face + the N-grammar classify/plane face; a lightweight mirror-linter guards the ~15 fixed
constants.** The two are complementary, not rivals.

**Q4 — three distinct generation claims (the sharpest cross-lock, owed to LOCK-4/LOCK-8).**
- **Claim A — type-generation** (Rust struct/enum → TS `interface`/union). ts-rs/typeshare/specta
  *flatten conditional types* — a documented, sound refutation for type-level *computation*. But 6B's
  schema is product-shaped (`Struct{…}` + string enums → plain interface, value-discriminated union),
  so the flattening limit **does not bind** the schema face. PT-E2's `infer V` inference is LOCK-8B's
  plane atop the generated interface, NOT 6B's.
- **Claim B — layout-generation** (tape/classify/plane descriptor → both decoders). ts-rs is
  *irrelevant* here (it generates types, never a decoder). The prior art is Kaitai-Struct /
  FlatBuffers / Cap'n-Proto / TableGen-ODS: one layout spec, N language readers, provable byte-parity.
  PROVE-14 proves the offset-tape crosses byte-exactly with zero type-projection machinery.
- **Claim B-full — LOCK-4C VM.** If combinators lower to a compact bytecode VM with one instruction
  set shared by Rust and TS, the instruction *stream* is the shared layout, isomorphic by
  construction — the strongest 6B, with no per-language body to hand-fill. **The type-gen refutation
  does NOT touch this.** Pass-1 conflated 6B-as-layout-gen with the refuted 6B-as-type-gen and wrongly
  foreclosed 6B on the combinator plane; pass-2 re-opens it as a LIVE option owed jointly to LOCK-4C /
  LOCK-8C. This is the lock's sharpest unresolved cross-lock adjudication.

**Mechanism / seams (must be relocated — V2).** The spec is not a new artifact: it is the grammar IR
+ the materialization lattice (`MaterializationClass` is live in `crates/core/src/backend/driver/`,
`…/pipeline/compile/pipeline.rs`, `…/generate/serialize/serialize.rs`; `crates/ir/tests/lattices/
materialization.rs`). `LayoutSpec` is a derived view computed once in the shared driver, never
serialized (honors no-ts-ir + parsimony). The live substrate: `Emitter` at
`crates/core/src/backend/emitter.rs`; the isomorphism gate `backend_ts_typecheck.rs`; the
pairing-linter `recognizer_consumer_invariant.rs`; the shipping `bbnf-path`/`bbnf-path-ts` instance
(both consume one `StructRegistry` ⇒ isomorphic by construction). **6B's work is completion — extend
the Emitter to a mirror TS roster — but the DirectSchemaSet/config-template/classify seams the
research cites no longer resolve; grounding the mechanism requires relocating them first (Brief 2).**

**Gates.** (a) lockstep regen — one spec-line edit + `xtask regen` moves all faces while PROVE-14
byte-exact parity (6 counts + `codeUnits` + fp 986912547) holds; (b) `%HaveSameMap` V8 shape probe
(`node --allow-natives-syntax`) — the generated skeleton must be single-hidden-class shape-stable, or
the 4.4× stop-early evaporates; (c) the born-RED wasm-plane byte-identity cross-check (GN-4.W4 gate b).

**Exact open questions.** (i) **The reasonable-point NUMBER is 0% measured** — gen-LOC vs hand-LOC per
face across JSON + one more grammar IS the family's entire verdict; no prototype ran. Null hypothesis
R3 (if the declarative surface is ~15 constants + two interfaces, the linter is cheaper and 6B narrows
to the schema+oracle faces) is unexcluded. (ii) **The grammar→layout-fact anchor is phantom** —
`STRING_NEEDS_DECODE` is absent tree-wide; the bit *assignment* is hand-authored policy, not a
grammar derivation. What the grammar genuinely derives is *which strings carry the flag* (the escape
class) and *the schema* — not the bit number. Prove a real derivation exists before Face I is called
"100% generated," or concede Face I is linted, not generated. (iii) **The AoS↔SoA reconciliation** —
the AoS `record.rs` stores a discriminator nibble; the SoA offset-tape stores none (kind re-derived
from `source[offset]`). One `LayoutSpec` emitting BOTH is not obviously expressible; parked as "GN-1
reconciles" — a genuinely hard unconverged core dressed "definitional." (iv) Cross-repo regen (F6):
the generated artifact ships inside the letter, regenerated parse-that-side.

**Cross-lock.** LOCK-3 owns the grammar-derived schema names; 6B consumes them to emit the TS mirror.
`LayoutSpec` + `emit_*` faces home inside LOCK-3's driver and consume a LOCK-1/2 CSP variable
(`MaterializationClass`) — name the ownership before building. Regex face deferred to LOCK-5's ONE-HIR
verdict (if regex rides one HIR, it is generated like the tape, not exempted). Freeze AFTER LOCK-1
(its schema is 6B's type-face input) + LOCK-7 (its oracle is 6B's non-tautological parity source).

**Disposition: ADVANCED · SPEC_ONLY** (combinator-**type**-algebra RETIRED as refuted). Crowned to
govern the largest surface with the least evidence: ZERO prototype, the decisive number 0% measured,
the grammar-derivation anchor phantom, the mechanism seams stale-cited (V2). The Q3 bounding and the
Q4 layout-gen re-opening are real design advances; the family's *product* remains unbuilt. Every claim
not backed by PROVE-14 or the shipping `bbnf-path-ts` carries the SPEC_ONLY label.

---

## 3. Family 6C — WASM-CORE (the Rust core compiled to wasm is the one implementation)

**Thesis.** One Rust implementation parses source to the tape inside wasm; TS is bindings + an idiom
layer; TS-native code exists only where V8 measurably wins. The boundary is data, not behavior — the
datum is the tape. Four SoA planes cross once, in bulk, as typed-array views; TS-native code walks
them (PROVE-14's 4.4× stop-early *because* the boundary is crossed once). The dividing line is not
"Rust vs TS" but "scan vs walk," and PROVE-14 already measured which side each belongs on.

**Why BANKED (the primary redirect stands; the unblock experiment ran and FAILED).** PROTO-1 ran the
NEON-crossing decider cold-paired parity-exact and resolved **R-A NEGATIVE decisively**: wasm SIMD128
at 0.34–0.47× native NEON (native 2.1–3.0× faster; parity exact — structural-index len
30318/93731/223235 identical). `simd-scan/wasm.rs` (V4-verified LIVE, 13 `core::arch::wasm32` sites,
CLMUL→shift-XOR fallback) is a genuine second implementation on the hottest surface — the
dual-maintenance the family exists to abolish. "One wasm impl earns a perf claim on the hot kernel" is
refuted by measurement. Compounded by F1 (full-walk ~0.9×) and R-E (synchronous ESM `all()`/
`dispatch()` cannot await `WebAssembly.instantiate` → 6C can never govern the parse-that combinator
plane). 6C-as-independent-primary does not advance.

**What is BANKED (load-bearing, not discarded).** The **zero-copy tape crossing**: 6B generates the
tape byte-contract, **6C crosses it once as typed-array views**, 6D digests both sides — confined to
the bbnf side (generated parser + regex-automata + solver + value-fabric tape). This is the
stratification's spine (§5, graft 1).

**Pass-2 corrections that survive into the banked line.**
- **Q6 — the sync-instantiation constraint is REFUTED as a per-call tax.** The 4 KB synchronous-
  `new WebAssembly.Module()` main-thread limit was deprecated+removed in Chrome 114 (V8 lazy
  compilation makes instantiation <1s even for the largest modules); Safari/Firefox never imposed it.
  It survives only as a **one-time boot obligation** (`await init()` once at module load; every
  `parse()` after is synchronous) — which the batch deploy target already amortises. A construction-
  ordering seam, not a per-parse async boundary.
- **Q6 — one consolidated cdylib beats two** on cold-instantiation and boundary count (one boot, one
  linear memory, zero wasm↔wasm boundary; lazy compilation means the larger blob is not a proportional
  cold tax). The only argument for two is independent versioning. Ties to LOCK-8C + LOCK-9.
- **Q7 — the wasm128 kernel cost re-attributes to LOCK-9.** The row-09 refutation (portable_simd→NEON
  is a native perf loss) does NOT bind the wasm arm (on wasm32 there is no native NEON to beat; `v128`
  is the only SIMD). What carries is the row-09 *lesson* (movemask narrowing is the lever) → R-BITMASK:
  V8's `i8x16.bitmask` arm64 lowering may be the same expensive double-`addv` reduce. If LOCK-9A
  unifies the kernel home behind a `{aarch64-NEON, scalar, wasm128}` backend seam, 6C inherits the scan
  for free and owns only plumbing; if LOCK-9B keeps repo-local kernels, the wasm128 arm orphans onto
  this plane. **Interlock: LOCK-6C.wasm-scan ⇐ LOCK-9A.kernel-home; LOCK-9B is the cost-bearing
  fallback.**

**Residual open gaps.** (i) **The decision-relevant comparator was never run** — PROTO-1 measured
wasm-SIMD vs native-NEON (not browser-deployable); the bbnf-side question is **wasm-SIMD128 vs a
TS-native scalar scan** (both browser-deployable). If TS-native scalar is < ~3 GB/s, wasm-core still
wins on the bbnf side *despite* NEON not crossing. 6C-on-bbnf-side is undecided, not settled-viable.
(ii) The crossing-mechanism role is PROVE-14-anchored, not PROTO-1-anchored (PROTO-1 measured the scan
kernel, not the planes→typed-arrays crossing). (iii) R-B memory-grow view-invalidation gate unbuilt (a
`memory.grow()` detaches every outstanding view — the two-parses-through-one-instance corruption case
never exercised). (iv) R-E rests on the unverifiable cross-repo count "value.js has 59 sync `all()`
sites" (F6-bound). (v) Homing collision (V5) unpriced.

**Cross-lock.** 6C mirrors a runtime (tape ABI: plane order, stride, endianness), not grammar-derived
names — the name map is LOCK-3's. Under 6C the regex exemption dissolves favourably (Rust DFA compiled
to wasm → no TS-`RegExp` mirror to exempt), unless a measured V8-wins carve-out keeps small/cold
patterns TS-native. Freeze AFTER LOCK-1, LOCK-5, LOCK-7, LOCK-9.

**Disposition: BANKED** (crossing-mechanism folded into the 6B/6D tape stratum; confined to the bbnf
side). High convergence on the family's *fate*, not on its viability as a primary. Not RETIRED — the
zero-copy crossing is the plane's tape stratum, and the bbnf-side wasm-vs-TS-scalar decision is
genuinely still open.

---

## 4. Family 6D — ORACLE-ISOMORPHISM / DIGEST (the mandatory FLOOR)

**Thesis.** The rust/ts split is held equivalent by a shared conformance corpus + a differential
digest ALONE — layout free per language. Correspondence is proven at runtime by the fold, not
statically by names: the file-granularity correspondence surface a linter/generator would police is
replaced by a node-granularity runtime differential. The plane's null hypothesis: it wins by default,
defeated only if measured drift/maintenance cost exceeds a cap's.

**Architecture — four coupled pieces.**
- **One digest spec, folded twice.** ~60 LOC of arithmetic (FNV-1a string fold, Murmur-style number
  mix, order-independent XOR member fold with count salt — the substrate is live at
  `bbnf-bench/ts-tape/tape.mjs:34-68`) specified once, each language folding over its own native value
  over the LOCK-1 Value ADT. **Widen u32 → u64** (the single most load-bearing substrate change: the
  u32 `fp` collides at ~77k inputs by the birthday bound; a coverage fuzzer emitting millions hits it
  and a colliding divergence escapes silently). The structural counters stay as an independent coarse
  channel — a divergence must collide in *both* to escape.
- **The alphabet IS the grammar-derived Value ADT** (LOCK-1). One node-hash constructor per variant;
  new variants extend the alphabet, not the transport → grammar-total by construction.
- **The corpus is coverage-generated + curated.** A grammar-driven fuzzer (the bootstrapped grammar as
  generator) + the static canon, three-way partitioned (must-accept / must-reject / may-either).
- **N+1-version tie-break.** LOCK-7's meta-circular reference is the third fold that adjudicates a
  both-engines-wrong case (the JEST model) — without it a differential oracle detects disagreement but
  cannot adjudicate it.

**Q10 — the IEEE-754 digest is CORRECTED, not merely verified (pass-2's decisive advance).** The
registry called the "8-byte IEEE-754-bit digest" 6D's single most important sharpening. Pass-2 research
shows the **bit-fold is a FALSE-DIVERGENCE GENERATOR** and the right design folds spans/canonical-
decimal, NOT bits:
- The as-built fold is **little-endian** (`setFloat64(…, littleEndian=true)`); the directive's
  "big-endian" framing would itself be a drift bug (aarch64 is LE; the Rust mirror is `to_le_bytes`).
- For **valid JSON the float lane holds by construction, not by luck**: Rust `str::parse::<f64>`
  (Eisel-Lemire) and V8 `StringToDouble` are both correctly-rounded (round-to-nearest-ties-even), so
  the bit-identical f64 is a unique function of the decimal string. No corpus needed to believe it
  (though the prototype must still exercise it — PROVE-14 never did; both its arms parse with V8).
- The un-exercisable residue is exactly the **non-JSON-expressible float set**: NaN (2⁵² payloads;
  aarch64 default-NaN vs V8-canonical vs Rust-bit-preserved diverge — a raw fold is NOT bit-identical)
  and signed-zero (`-0.0` vs `+0.0` — a *policy* the spec must state). Closed by **spec rules**
  (NaN→sentinel; decide signed-zero), not by test.
- **The binding recommendation:** PROVE-14's certified digest never enters this residue — it folds
  UTF-16 code units + structural counts + source-byte number *spans*, no decoded double. Fold source-
  byte number spans or RFC-8785 canonical decimal, **never raw IEEE-754 bytes.** A digest that folds
  decoded doubles re-imports the NaN/±0 hazard and manufactures false divergences.

**Q8 / Q9 — the deciders as measured questions.** Injected-divergence classes (settled art):
structural miscount, string-decode (escape/surrogate/overlong), number-rounding last-ULP
(fuzzer-only-reachable — needs mantissa-boundary generation), NaN/±0 (unreachable by JSON corpus),
order-dependence. The must-accept/must-reject/may-either partition maps onto the live checkasm strict
flag (`BBNF_SIMD_INJECT_BUG`/`BBNF_SIMD_STRICT`); the escape-fraction budget counts only *reachable*
classes (unreachable closed by spec, else the budget can never go green). **Q9 error-class DECIDED
IN-contract** (gate the small class enum — UnexpectedChar / UnterminatedString / TrailingData /
DepthLimit — NOT message wording, NOT span bytes): the class is grammar-derivable, stable across
languages, and a value-level fact callers branch on; wording is idiom, offset is layout. Named
supersession: this relaxes `css_scan_differential.rs`'s intra-Rust "same error offset + message" to
error-class-only *across* languages.

**Mechanism / seams.** No invention — *consolidation*. The in-tree differential machinery is mature
but intra-Rust: `checkasm_parity.rs` (FFmpeg model, misalignment sweep, bug-injection knob, strict
split), `css_scan_differential.rs` (clean-room scalar reference), `parity.rs` (plane-equality). The
gap is narrow and nameable: **every fold runs inside one language** — PROVE-14's two arms are both JS.
The missing artifact a prototype must build is a **Rust-side fold of the same spec**, compared against
the TS fold. Feeds the GN-4.W5 four-way parity close gate.

**Gates.** (a) 100% digest agreement across Rust eager/stop-early/typed + TS `TapeDoc` + a third-party
reference — PROVE-14 parity as a STANDING law; (b) the in/out-of-contract partition as data (values +
accept/reject in; wording + span out; error-class toggle DECIDED-ON); (c) three-way accept/reject
partition so the gate is stable, not flaky.

**Exact open questions.** (i) **Both verdict numbers UNMEASURED** — drift-escape rate and marginal
maintenance-cost-per-grammar are 6D's entire verdict; no prototype ran. (ii) **The green/red PAIR
unrun** (shared with 6A gap ii — one `cargo test` discharges both). (iii) The Rust-side fold does not
exist. (iv) The coverage-guided fuzzer + delta-debug shrink (the "teeth") unbuilt — gate-(a) today
exercises only the *easy* direction (agreement among correct impls). (v) The reasonable-point in/out
partition named but not fully drawn.

**Cross-lock.** A digest-only family needs no name map (correspondence is runtime, not static). The
regex split becomes a behavioral conformance obligation (same match span/captures/accept-reject over a
shared corpus), compatible with a LOCK-5 ONE-HIR verdict without an allow-list freeze. **Freeze
strictly AFTER LOCK-1 (the alphabet) and LOCK-7 (the tie-breaker):** a differential oracle with no
ground-truth reference can only detect disagreement, not adjudicate it — 6D-as-floor cannot freeze
before LOCK-1's ADT and LOCK-7's oracle freeze.

**Disposition: ADVANCED · the mandatory FLOOR · SPEC_ONLY.** Compatible with every cap and REQUIRED by
each (6A's T3, 6B's parity oracle, 6C's drift gate) — every rival is "6D plus a structural tax." The
IEEE-754 correction and the error-class decision are real advances. But **pure-6D-as-whole-answer is
BLOCKED for the tape**: "layout free per language" directly negates the byte-identical SoA layout the
zero-copy stop-early win (F2/F3) requires. 6D wins as the *verification substrate*, loses as the
*whole answer* where the tape needs byte identity to be zero-copy. Both verdict numbers carry SPEC_ONLY.

---

## 5. The incompatibility map (which families cannot coexist, and precisely why)

The families are one mandatory floor (6D) and three candidate structural caps (6A/6B/6C) that conflict
**only on a shared surface.** Once surfaces are partitioned the conflicts dissolve into a
stratification — and drawing that partition is the next pass's central deliverable.

**Genuine pairwise incompatibilities (same surface):**

1. **6A ⊥ 6B — exclusive per item.** Generate-vs-hand-write-and-lint cannot both govern the same item.
   A 6B-generated pair has no independent hand-authored twin for 6A's name-map to check — the linter
   would compare the generator's two outputs, a tautology (consumer-less substrate, Y.13). They coexist
   ONLY by partitioning the surface. Pass-2 sharpens the line: 6A's Class-1 (grammar-derived) is
   tautological to *name*-check and is instead 6A-provenance-checked OR 6B-generated (same source); 6A's
   genuine non-tautological load is **Class 2 (the hand-authored taxonomy, ~19 items) + arity-drift**;
   the ~15 fixed layout constants are LINTED (6A/pairing-linter), and the schema + N-grammar classify
   are GENERATED (6B). The partition requires 6B's UNMEASURED gen-vs-hand number to draw — neither is
   banked. On the combinator **type** algebra specifically, 6B is REFUTED → that surface is 6A + 6D with
   no 6B option — **but 6B-as-layout-gen (the LOCK-4C VM stream) is a live re-opening the type-gen
   refutation does not touch.**

2. **6C ⊥ 6A and 6C ⊥ 6B — on absorbed surfaces.** Where 6C absorbs a surface into the wasm core there
   is NO second implementation: the TS side is a thin binding, not a twin to mirror (6A) or a skeleton
   to generate to (6B). 6C partitions the plane at the wasm seam rather than competing across it — but
   PROTO-1's R-A-negative means the absorbed surface does not include the hot scan kernel (it stays a
   dual implementation), so 6C absorbs only the *crossing*, not the *scan*.

3. **6C ⊥ the parse-that consumer contract (family-vs-constraint, hard).** Synchronous ESM
   `all()`/`dispatch()` cannot await `WebAssembly.instantiate` (R-E). Confines 6C to the bbnf side; it
   can never be the plane-wide answer. (Pass-2 softens this at the boundary: the constraint is a
   one-time boot obligation, not a per-call async tax — but the confinement to the bbnf side stands,
   because a synchronous combinator built at import time still cannot touch wasm before boot resolves.)

4. **6D-pure ⊥ the tape plane (family-vs-measured-requirement).** "Layout free per language" negates the
   byte-identical SoA layout the zero-copy stop-early win (F2/F3) requires. 6D-*as-whole-answer*
   conflicts with the tape's measured requirement; 6D-*as-floor* does not. The null hypothesis is
   defeated *for the tape* and survives *as verification*.

**Compatibilities (the stratification the incompatibilities resolve into):**

| surface | primary mechanism | verification |
|---|---|---|
| tape layout (4 SoA planes) | **6B** generates the byte contract, **6C** crosses it zero-copy (bbnf side) | 6D digest + T0 byte gate |
| decoder skeleton / oracle tables / schema face | **6B** (grammar-defined; N-grammar-scaled) | 6D |
| ~15 fixed layout constants / bit assignments | **linter** (6A/pairing-linter — generating them over-builds, Q3) | 6D |
| value-materialization / walk | **TS-native** (6C-tempered "where V8 wins"); skeleton optionally 6B | 6D |
| combinator **type** algebra | **hand-written** (6B-type-gen refuted); **6A** name-map | 6D behavioral |
| combinator **layout** (IF LOCK-4C VM lands) | **6B-as-layout-gen** (shared instruction stream) — OPEN | 6D behavioral |
| module tree / names | **6A** name-map (Class 2) + provenance (Class 1) + budget (Class 3); NOT code-shape (F4) | 6D |
| everything (behavior) | — | **6D** — the mandatory floor |

**The reading the evidence forces:** 6D is installed everywhere as the floor; 6B governs the
schema/tape/oracle + N-grammar classify surface; a linter guards the ~15 fixed constants; 6A governs
the module-tree/name/idiom-residue surface via the trichotomy; 6C collapses into the zero-copy crossing
mechanism for the 6B-generated tape, confined to the bbnf side. The three caps are rivals only where
surfaces overlap (6A⊥6B on any single declarative item), and the next pass fixes the overlap by
measuring the two reasonable-point numbers (6A's ratio is done; 6B's is Brief 2) and adjudicating the
6B-as-layout-gen ⊗ LOCK-4C VM contradiction.

**Cross-pollination (named grafts).** (1) 6B ⊗ 6C — the generated tape byte-contract crossed zero-copy
(PRIMARY, the stratification's spine): 6B without a crossing is a spec with no consumer; 6C without a
generated contract is two hand-transcribed alphabets (the AoS/SoA drift). (2) 6A ⊗ 6D — 6A's T3 tier IS
6D's differential corpus: 6A's manifest carries T1/T2 (structural) and delegates T3 entirely to 6D's
shared corpus + digest; 6A *is* "6D plus a structural tax" by construction. (3) 6B ⊗ 6D — the generated
oracle table (SECONDARY): 6B generates the digest comparator + structural-count table into BOTH a Rust
fixture and a TS test, so GN-4.W5's self-stub (`get==get`) becomes unwriteable.

**BLOCKED-candidate summary.** 6C-as-primary (BANKED, past BLOCKED into crowded-out: refuted maximal,
confined by R-E, perf premise on the hot kernel REFUTED by PROTO-1's measured R-A — the missing
primitive "wasm SIMD that keeps the aarch64 win" was measured absent, and re-supplying it is the
original hard problem). 6D-as-whole-answer (BLOCKED for the tape: layout freedom negates byte
identity). 6A and 6B are ADVANCED, mutually exclusive per surface, each strictly dependent on the 6D
floor — and both SPEC_ONLY on their central mechanism (6A's linter unbuilt; 6B's number unmeasured +
anchor phantom + seams stale).

---

## 6. The two sharpest uncertainties for the next pass

The next pass draws the partition line (§5) and adjudicates the cross-lock contradictions before either
gestalt consumes the lock. The partition rests on two numbers: 6A's clean-map ratio (MEASURED 47/61/0)
and 6B's gen-vs-hand LOC (UNMEASURED) — and on the load-bearing claim beneath the whole stratification:
that 6D is a *mandatory* floor (structural caps necessary-not-sufficient). The two briefs below are the
cheapest, most decisive, in-tree resolutions of those two open hinges. Brief 1 is the plane's
non-negotiable minimum — the cheapest experiment in the lock, skipped in both prior passes, on a seam
verified LIVE this session; it discharges 6A gap ii and 6D gap ii at once. Brief 2 draws the second
half of the partition line and tests whether 6B's crown has any generated product at all.

*(Full briefs returned in the structured output.)*
