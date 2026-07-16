# LOCK-6 — THE ISOMORPHISM PLANE (rust/ts split): FAMILY SPECS (synthesis, pass 3)

*Synthesis, Fable-tier, pass 3. One cogent spec per family while the routes remain incompatible,
then the incompatibility map. The plane is not four rivals: it is **one mandatory behavioral FLOOR
(6D) and three candidate structural CAPS (6A/6B/6C)** that conflict only where their surfaces
overlap. The deliverable is the **partition line (§5)**, drawn by two reasonable-point numbers —
6A's clean-map ratio (MEASURED 47/61/0, still python-differ not the linter) and 6B's gen-vs-hand LOC
on the **tape-layout face** (now MEASURED at the classify face by PROTO-1) — over the load-bearing
claim that 6D is a mandatory floor (structural caps necessary-not-sufficient). Elegance earns
nothing: a family stalled at a missing primitive equal in difficulty to the original problem is
BLOCKED-candidate.*

*Binding cross-lock frame (facts, XL-A..H, do not re-litigate): substrate is **skinny@HEAD**
(the only living implementation / measurement substrate); designs target the greenfield workspace;
top-level `crates/` is legacy (archives at GN-0). Grammar tier holds Regex opaque (`StringId`),
engine tier holds rich `Hir` (XL-C). The fast TS face is the BUILD-TIME EMITTER; runtime decoders
survive only as isomorphism/correctness ORACLES (XL-D). LOCK-3 owns the middle-IR tower +
decision-driver seat (XL-B, working). Every number names its HEAD/base (XL-H).*

---

## 0. What pass 3 moved (verified deltas — research AND the two prototypes now folded)

Pass 2 left every crown *mechanism* SPEC_ONLY and named the RE-EMERGENT prototype-allocation defect
(both probes measured softer targets than the sharpest uncertainty). Pass-3 research moved the
**feasibility** frontier; **the two pass-3 prototypes then moved the MECHANISM frontier on the two
families whose crowns were still SPEC_ONLY** — the exact defect the last two passes repeated is not
repeated here. Base-pin: both probes measured on `ce51c36fc` (PROTO-1 ff-merged its stale worktree
to the pin; numbers are on-artifact).

- **P1 — 6B's crown product RAN (PROTO-1), the emit machine that was 0% built through three passes.**
  ONE data-driven `emit_classify(spec, Backend)` consuming a single backend-agnostic `LayoutSpec`
  emits, **byte-identical to the live hand code at `ce51c36fc`**: (a) the Rust JSON `FromSource`
  face (`JsonNodeKind::at_cursor`, enum) — 0-diff; (b) the Rust Sheets `FromFlagTag` face
  (`GoogleSheetsKind` + `KIND_TABLE` + `at_cursor`) — 0-diff; (c) the **TS** classify face (consts +
  `classify` switch) — 0-diff, after the machine learned to defer the single `default:` arm to switch
  end (a real generation bug found and fixed in-probe). The generated TS spliced into the hand
  decoder harness holds V8 monomorphism under `node --allow-natives-syntax`:
  `%HaveSameMap(a,b)=true` and `%HasFastProperties=true` **both pre- and post-walk** — the single
  `TapeDoc` hidden class the 4.4× stop-early depends on is preserved *by construction* (generated ≡
  hand, byte-for-byte). A one-line spec edit (`+ "NamedRange"`) drove **3 emit sites across 2
  backends** (Sheets enum + `KIND_TABLE` + TS const) while JSON stayed invariant and the twitter.json
  fingerprint `986912547` held. **This is the single largest SPEC_ONLY→RAN conversion of the pass:
  6B's crown is BUILT at the classify face, R3 is NOT refuted, and the AoS↔SoA expressibility item is
  DISCHARGED by run** (one machine expresses both the SoA-byte-re-derive and the tag-table
  discriminator, both backends, from shared spec data). Scope honesty (§2 below): the probe is the
  *classify FACE* (27 TS LOC → 0; 138 Rust LOC → 0), a standalone `rustc` binary, NOT the full
  625-LOC decoder and NOT yet wired into `xtask regen`.
- **P2 — 6C's thrice-deferred comparator is now SCAFFOLDED and RUNNABLE (PROTO-2), still UNRUN.**
  The `v128`/`simd128` structural-byte kernel (`PROTO-2/wasm-scan/src/lib.rs`, `#![no_std]`,
  raw-C-ABI, nibble-LUT `u8x16_swizzle`+`u8x16_bitmask` classify16, 64-byte stripes, marshalling tax
  driven explicitly from JS and counted INSIDE the wasm lane) and the paired cold browser harness
  (`PROTO-2/harness.html`: monomorphic TS-scalar `Uint8Array` byte loop vs `wasmFull`, per-byte
  timing, synthetic 64B→64KB size sweep + real twitter/citm/canada, parity assert, cold single-shot
  lane) are **authored and ready** — but there is **no `PROTO-2.md`, no measured numbers, no browser
  run**. The decision-relevant comparison — wasm-SIMD128 vs TS-native scalar, both browser-deployable
  — remains **unmeasured a fourth consecutive pass**. The difference from prior passes: the design/
  harness is no longer owed, only the *build + run* is. This is Brief 1 (§6), now a build-and-run
  away, not a design away.
- **D1 — 6D's cross-language fold RAN (research-D §1), the artifact that had NEVER run.** A Rust
  fold (`rustc -O`) and a V8 fold (node v26) over a boundary f64 corpus: the **raw IEEE-754 bits
  lane folds BIT-IDENTICAL** (`b99874e8 == b99874e8`) while the **native-decimal-text lane DIVERGES**
  (`cce194de` vs `c8d6e5e1`) on three enumerable classes (exp threshold ≥1e21 / <1e-6; `-0` sign;
  subnormal expansion). The fold arithmetic is genuinely isomorphic; the divergence is in the
  *canonical-decimal spec*, not the machinery. Injected-divergence table ran: **static corpus 4/5,
  coverage-fuzzer 5/5**, detection latency 1 input once the class is covered (bool-collapse escapes
  forever on a bool-free corpus — the empirical case for the fuzzer).
- **D2 — 6D's number lane is INTERNALLY CONTRADICTED across passes (bits ⊥ spans).** research-D +
  `contract-6D.toml` commit to **`f64::to_bits()` little-endian** (with a canonicalized residue:
  NaN→sentinel, `-0`→`+0` before the byte fold) and argue bits are the *superior* last-ULP guard.
  This **reverses pass-2's registry-credited Q10 correction** ("fold spans / canonical-decimal,
  **NEVER** raw bits") — the third representation flip in three passes (bit→span→bit), now with
  running evidence on the bits side. Reconcilable in principle (bits-with-canonicalization addresses
  the pass-2 concern at the residue), but the checked-in contract commits to bits while the pass-2
  correction forbids them. **A live, unadjudicated contradiction in the mandatory floor's frozen
  contract — surfaced §4, routed to GESTALT (a spec-probe settles it), NOT resolved by fiat here.**
- **D3 — 6A's cap is re-derived defensibly (research-A Q1).** The arithmetically-inconsistent scalar
  `2·T+I` is **gone**, replaced by `cap = Σ_{c∈I} |source-tree sites matching class c's predicate|`
  — computed from the tree by grep, never from the exemption file. `I` is resolved to **5 idiom
  classes** on the live pair (serde-transport, native-target shim, language-native error,
  surface-syntax constructor, marker-representation); each a *relational* rewrite rule `r(t)`
  requiring a spine pre-image `t ∈ T` (so `BogusPayload` with no canonical `Bogus` REDs). Reproduces
  the live count without fitting it; REDs on an injected `FooView`; forbids growth at a defended
  count. The pass-2 gap-1 defect is dissolved at the *design* level — but the linter that computes it
  is still UNBUILT (§1).
- **D4 — 6B's AoS↔SoA "hard core" is dissolved into a discriminator-provenance spectrum
  (research-B §2), now CONFIRMED BY RUN (P1).** The tape is **always** SoA (`offsets` + sparse
  `flag_cursors`/`flag_values`); "AoS vs SoA" was never a storage question. The real axis is
  **discriminator provenance**, the sparse flag plane a polymorphic slot. Three dialects ship at HEAD
  over the identical physical tape: JSON `FromSource`, Sheets `FromFlagTag`, CSS-L4
  `FromSourceThenFlag`. PROTO-1 emitted the first two from one spec, byte-identical — the pass-2
  "parked-definitional" reconciliation is answered *by the tree AND by the emit machine*.
- **D6-superseded — 6C's kernel framing corrected.** research-C establishes the Q1 kernel is the
  *existing* portable `u8x16` nibble-LUT arm the generator already emits for wasm32
  (`simd-scan/.../emit/simd.rs:85-135`, lowering to `v128` under `+simd128`) — no new emitter, only a
  build; the decisive cost is the **boundary** (UTF-16→UTF-8 encode+copy per call), not the kernel
  width. PROTO-2 realizes exactly this kernel as a standalone `#![no_std]` probe. The verdict
  (size-dependent crossover: wasm wins large single docs, TS-scalar can win batches of small docs) is
  now testable by running PROTO-2.

---

## 1. Family 6A — MIRROR-TREE (hand-authored both sides + a CI isomorphism linter)

**Thesis.** Both trees stay hand-written and idiomatic per language; a checked-in machine-readable
manifest (`mirror.toml` + `idiom-classes.toml`) is the single source of truth for what mirrors what;
a CI linter proves it holds against extracted *declaration surfaces* (never bodies). Kotlin-
Multiplatform `expect`/`actual` reconstructed across two repos, the linter standing in for the single
compiler no cross-repo toolchain provides. **Scoped:** the structural mirror covers only faces
bearing a *layout or oracle contract* (tape planes, flag channel, `Value` variant set, oracle
tables); engine internals (combinator algebra, regex automata, SIMD kernels) are out of domain,
conceded to 6D behavioral-only — the honest locus of "reasonable point," exactly where PROVE-14
delivers byte-exact cross-language evidence (research-A §0).

**Architecture — the Q2 trichotomy routes the linter (carried).** Three disjoint classes, each a
different tautology status and lawful check:
- **Class 1 — grammar-derived names** (field/rule names, kind discriminants, plane order, flag bits).
  ONE source, minted by the emitter. Name-checking is tautological. **Route → PROVENANCE** (both
  sides read the identical generated sidecar; hand-authored fork → RED) — anchored *and*
  non-tautological because the proposition is "single-source held."
- **Class 2 — hand-authored-twice taxonomy** (`PathErrorReason`-class enums authored on both sides;
  the dual-authored `classify`/`JsonNodeKind` byte→kind switch). The ONLY class where a name-map is
  non-tautological, and **where drift LIVES** (research-A's live `nope` counterexample: 8 variant
  names correspond perfectly while the *routing* forked — `UnregisteredRule` vs `UnknownField`).
  **Route → NAME-MAP** (set-equal variant/field sets; TS switch exhaustive over the union).
- **Class 3 — idiom residue** (`#[wasm_bindgen]` exports, ownership types, SIMD kernels, native
  `RegExp`, lifetime-shape). No twin. **Route → EXEMPTION BUDGET.**

**Q1 — the cap re-derived from the tree (D3).** `cap = Σ_{c∈I} |source sites matching predicate[c]|`,
grepped from source, never from the exemption file. Each idiom is a *relational* rewrite `r: T →
names` requiring the spine pre-image (`I5` marker-rep: Rust ZST ↔ TS string; `I1` serde twin; etc.);
`legit(B) = {identity mirrors} ∪ {r(t) : r∈I, t∈T}`, `cap = |legit(B)|`, `RED ⇔ ∃ b ∉ legit(B)`.
Reproduces the live count (`4+2+1+2+4`) as an *output* of the rules, never a fitted integer; REDs an
injected `FooView` (no pre-image); REDs `BogusPayload` (no canonical `Bogus`); growth is legal only
by a reviewed edit to the closed `I` registry — the one bounded place human judgment enters. The
load-bearing invariant is a **tight predicate per class** (a loose predicate inflates `cap` and hides
drift), grep-verified in CI.

**Q2 — two representative names traced (research-A §Q2).** (1) `lex_path` / the path-segment lexer =
**SINGLE grammar-derived source** — one `bbnf_regex::lex_path`, consumed unchanged by the Rust
proc-macro (`path_macro.rs:270`) AND the TS-bridge cdylib (`compile.rs:125`, compiled Rust behind
wasm); *cannot* diverge, needs no mirror check. (2) `PathErrorReason` (the 8-variant taxonomy) =
**hand-authored on every side** — canonical `error.rs:44`, Rust twin `schema.rs:59`, TS union
`path.ts:37`; the only automated tie is one exhaustive Rust match; the TS union is tied by nothing
compiled. The archetypal Class-2 name. (bonus) `STRING_NEEDS_DECODE = 0x01 = GRAMMAR_BIT0` =
grammar-derived, hand-copied into a TS literal that silently rots on a bit renumber — the whole
argument for the Q3 provenance linter.

**Q3 — linter ownership (research-A §Q3).** **LOCK-3 owns the provenance MANIFEST** (a `cargo xtask
regen` byproduct: name → {single-source | authored}, governed by clean-regen discipline — its
consumer is the linter, satisfying Y.13); **LOCK-6 owns the name-map LINTER + `mirror.toml`** that
reads TS/serde surface idioms (knowledge that must NOT enter the backend-agnostic IR tower — a
layering violation). Single-source names are skipped (can't drift); only `authored` names enter
`legit(B)`. Prior art: tree-sitter `node-types.json`, Cap'n Proto schema→descriptor, rustc
`sourcegen` up-to-date check.

**Mechanism / seams.** `syn` over the Rust mirror declarations; TS surface from built `dist/*.d.ts`
(or `ts-morph` AST); the sidecar manifest; checked-in `mirror.toml` + `idiom-classes.toml` +
`type-map.toml`. The live behavioral drive available today is **PROVE-14** (Rust `parse_tape` → four
planes → TS `TapeDoc` decode → `verify.mjs` 32-bit fp against a `JSON.parse` walk; 320 cold children,
`fp 986912547`, `codeUnits 304329` identical) — a genuine Rust→TS drive, the backstop 6A *needs* for
the dual-authored `classify` no structural map can certify. But the ONE live cross-language pair for
the *linter itself* is `crates/bbnf-path` ↔ `crates/bbnf-path-ts` (research-A §0), whose third limb
(`ts/path.ts` + `PathError.format`/`reasonText`) is behaviorally **un-driven** today — the intra-Rust
`compile_path_native` shim stands in (the pass-2 proxy problem, still unresolved for this pair).

**Gates.** (a) Class-2 mapped-set 0%-tolerance RED; (b) allow-list monotonicity RED; (c) impedance
cap `#exemptions ≤ Σ predicate-sites`; (d) provenance gate (manifest ≠ TS literal → RED); (e)
file-tree correspondence SOFT-warn (Rust dir-modules ↔ TS flat `parser.ts`; concept granularity
mirrors, file granularity does not).

**Exact open questions.** (i) **The mechanism is STILL UNBUILT** — the `mirror.toml` manifest linter
(`syn` extractor + `dist/*.d.ts` extractor + `cap = Σ sites` + RED-on-synthetic + behavioral Node
drive of `ts/path.ts`) never ran. The registry's #1 6A directive is undischarged; PROTO-1 built 6B's
crown, not 6A's. **This is now the single largest remaining SPEC_ONLY crown in the plane** (6B's is
BUILT; 6D's fold RAN; 6C's harness is authored) — Brief 2 (§6). (ii) The green/red PAIR at the linter
level (T1/T2 green while T3 goes RED) is asserted about non-existent tooling. (iii) The Class-2
assignment is routed, not *proven* single-vs-dual for a representative name *at the linter*. (iv)
CASE totality+injectivity (a `next`/`next_sibling`→one camelCase collision is a failure), unbuilt.
(v) Cross-repo CI ownership under F6 (per-repo manifest fragments + vendored corpus) — proposed,
unbuilt, the family's hardest logistics. (vi) The behavioral TS drive of the live pair is still the
intra-Rust `compile_path_native` proxy; the real `serde-wasm-bindgen`/Node crossing is owed.

**Cross-lock.** LOCK-3 owns the provenance manifest; LOCK-6 owns the linter (Q3). Rust-DFA↔TS-`RegExp`
is a Class-3 SUBSTRATE exemption, behavioral-only, compatible with a LOCK-5 ONE-HIR verdict that
forbids a *layout* mirror there. Freeze AFTER LOCK-1 (the ADT the map ranges over) + LOCK-7 (the
tie-break oracle).

**Disposition: ADVANCED · dependent-on-6D · SPEC_ONLY core.** The predicate-derived cap (D3) resolves
the pass-2 arithmetic-inconsistency defect at the design level; the traces + ownership answers are
sound; the PROVE-14 drive is the genuine cross-language backstop the pass-2 proxy lacked. But the
linter that IS the family remains unbuilt, and 6A's own Class-2 finding proves it is
*necessary-not-sufficient* (names green while routing forks) — 6A *contains* 6D's T3 as a mandatory
tier, never standalone. **Not BLOCKED** — the cap is now grep-derivable (no missing primitive equal
to the original problem); the family is buildable, merely unbuilt, and it is the crown the next pass
must build.

---

## 2. Family 6B — SPEC-GENERATED-SKELETON (one spec generates both skeletons) — CROWN BUILT (classify face)

**Thesis.** The isomorphism problem has a declarative half (drift-prone, low value-per-line, high
correctness stakes: byte layout, tape contract, type interfaces, oracle tables) and an imperative
half (idiomatic, V8/LLVM-bound: the decode walk, the SIMD scanner). Generate the declarative half
from one grammar-derived `LayoutSpec`; hand-write the imperative half; draw and MEASURE the boundary.
Drift becomes impossible-by-construction where generated, a regen diff not a code review.

**Architecture — `LayoutSpec` + the discriminator-provenance spectrum (D4), NOW REALIZED (P1).**
PROTO-1 authored the `LayoutSpec` as concrete DATA and the `emit_classify(spec, Backend)` machine:
```
Discriminator = FromSource{arms, rust_default}      # JSON: classify(source[offset])
              | FromSourceThenFlag(byte, flag, default)   # CSS-L4: byte partial, flag completes
              | FromFlagTag{table, zero_is_none}    # Sheets: kind stored in the flag byte, KIND_TABLE
              | Scan                                # general-descent: no per-node kind
FlagSemantics = EscapeBits{sym,value,comment} | DiscriminatorTag   # the polymorphic sparse-plane slot
kinds         = [enum member + stable u16 discriminant + optional TS symbol]
```
The discriminator STRUCTURE (ordered arms, byte ranges, default) is truly shared; per-backend surface
tokens (the `K_OBJ_OPEN` abbreviation, doc-comment text) are carried as spec DATA — the correct home
(a naming layer), so "backend-agnostic" = shared discriminator structure + per-backend surface
strings, NOT a single symbol set (PROTO-1 caveat, honest). All three non-`Scan` dialects ship at HEAD
over one physical SoA tape; **PROTO-1 emitted the JSON and Sheets dialects from this one spec,
byte-identical to the hand faces** — the AoS↔SoA reconciliation is answered by run, not parked.

**Q1 — gen-vs-hand LOC on the tape-layout face, NOW MEASURED POST-GENERATION (PROTO-1).** On the
**classify face** specifically, at `ce51c36fc`, generated ≡ hand byte-for-byte:

| face | hand LOC (was) | hand-authored after generation |
|---|--:|--:|
| TS classify (consts 12 + `classify` switch 15) | 27 | **0** |
| Rust JSON (`JsonNodeKind` enum 17 + `at_cursor` 20) | 37 | **0** |
| Rust Sheets (doc+enum+`KIND_TABLE` 89 + `at_cursor` 12) | 101 | **0** |
| **classify-face total** | **165** | **0** |

Replaced by ONE 375-line machine (~289 shared render logic + ~86 spec DATA for both grammars);
per-grammar spec ≈ 40–45 lines; adding a kind = **1 data line** driving 3 sites across 2 backends.
The broader Rust tape face (byte-layout `config.rs` + full `view.rs`/`value.rs`) was measured
100%-`@generated` at HEAD across three grammars (research-B §3: JSON 691/0, Sheets 283/0, CSS-L4
65+/0). **The remaining projection is the FULL TS decoder** beyond classify: the PROVE-14 `.mjs`
surface is 625 hand LOC (tape 364 + load/child/bench/pointer/verify); PROTO-1 generated the
27-LOC classify FACE of it (the R3-critical monomorphic piece), leaving the walk/leaf/nav body as the
next generation target (Brief-adjacent, §6). **6B moved from "0% measured on the tape face" (pass 2)
to "classify face 165→0 MEASURED byte-identical both backends, monomorphism preserved; full-decoder
face is the remaining scale target."**

**Q2 / AoS↔SoA — DISCHARGED (P1).** One machine expressed both `FromSource` (SoA byte-re-derive) and
`FromFlagTag` (tag-table) from shared data; the pass-2 "demonstrate one-spec-two-dialects or file the
blocker" directive is discharged by the *first* option, with a running byte-identity proof.

**Q3 — idiomatic-consumer adapter (research-B §4).** The generated fused single-pass sink
(`json_typed_direct.rs`, grammar-derived `DirectSchemaSet`; CSS `sink.rs` 16 gen LOC over 886
`generated.rs`) fills a native struct in one pass, cost **negative** vs the faithful-union walk (it
skips union materialization; SPEC §0 row-07 CONFIRMED 0.746–0.786× vs sonic-typed, 1.85–1.92× vs
serde on citm). The "adapter" is the generated filler; it is thin *because no union is built*. Only
`struct→value` re-serialization pays. `LayoutSpec` emits both `MaterializationClass::LazyView`
(faithful union) and `::DirectSink` (idiomatic struct) — the variable is per-consumer, not per-
grammar. Caveat: measured Rust-only; a TS `emit_direct_sink` face is the symmetric obligation,
unbuilt.

**Q4 — shared bytecode instruction stream as a generation target (the one live cross-lock item).**
research-B §5 argues **YES, a type-generation refutation does not bind it**: bytecode carries no
generics to erase (the tape's `u32` cursors and a VM's `u32` opcodes are the same monomorphic
substrate); the `emit_*` machine is layout-shape-agnostic (`offsets[cursor]→node` vs
`code[pc]→instruction`); the binding refutation (full-walk 0.91×) is a perf-non-goal, orthogonal.
**PROTO-1 strengthens this: the same `emit_classify` machine that drives `Discriminator` would drive
`OpcodeDecode` with no new machinery** (swap `KindRecovery` for the opcode dispatch, `payloads` for
an operand arena). Prior art: LPeg/RE2 parsing-VM bytecode, MLIR ODS (one op-def → C++ AND Python
builders), WASM itself. **Routing:** LOCK-4 owns the instruction-set design, LOCK-8 owns the TS
interpreter shape, 6B contributes the generator + the differential oracle (research-B §5's joint
probe: emit a two-opcode stream into a Rust `match pc` and a TS `switch(pc)`, assert identical output
digest — the bytecode analogue of PROVE-14). **This cannot be resolved inside LOCK-6** — owed jointly
to LOCK-4C/LOCK-8C; the seam GESTALT-P collides on first.

**Q5 — ownership (research-B §6).** `LayoutSpec` (type) + the `emit_*` faces → **LOCK-3** (the
backend-agnostic middle + transpositions; where `render` + the three `emit_*` live). The
`MaterializationClass` variable (already an IR-pass product,
`crates/ir/src/types/grammar.rs:195` `HashMap<NodeId, MaterializationClass>`), extended with the
discriminator-provenance axis → **LOCK-2** (the *choice* is a cost decision). Handoff: **LOCK-2
resolves `MaterializationClass` + `LayoutSpec`; LOCK-3 emits both backends** — mirrors the extant
`lower_for_runtime`→`render` seam. Named-as-open in the registry; this is the proposed assignment,
owed to LOCK-3/LOCK-2.

**Mechanism / seams (D5 — confirm at skinny@HEAD).** research-B grounds on
`skinny/crates/codegen/src/runtime_generator.rs:{19-24,26-71,78-87,89-278}` (`LoweredArtifact`,
`emit_from_request`/`lower_for_runtime`, `render`, the three `emit_*`), the config specs
`grammars/{json,sheets,css_l4_declaration_values}/…`, the tape `runtime/src/tape/mod.rs:92-99`.
PROTO-1's machine is a standalone `rustc` binary that reproduces the live hand faces byte-identically;
**landing it into the emit spine (replace `include_str!("json_templates/value.rs")` with
`emit_value(&spec, Backend)`, delete the hand faces, wire `xtask regen`) is the follow-on tranche
step** — the pass-2 seam-relocation obligation stands (confirm paths at skinny@HEAD before landing).

**Gates.** (a) lockstep regen — one spec-line edit + `xtask regen` moves all faces while PROVE-14
byte-exact parity (6 counts + `codeUnits` + `fp 986912547`) holds — **PARTIALLY DEMONSTRATED**
(PROTO-1: `+ "NamedRange"` moved 3 sites, fp held, but on a standalone binary not `xtask`); (b)
`%HaveSameMap` + `%HasFastProperties` on the GENERATED TS decoder under `node --allow-natives-syntax`
— **PASSED** (PROTO-1, pre+post-walk, on the classify face spliced into the hand harness); (c) the
born-RED wasm-plane byte-identity cross-check (GN-4.W4 gate b) — home still absent (§ seam hygiene).

**Exact open questions.** (i) **Scale from the classify FACE to the full decoder** — PROTO-1
generated 27 TS classify LOC → 0; the walk/leaf/nav body (~598 remaining hand TS LOC) is the next
generation target, and the monomorphism gate must re-pass on the *full* generated decoder, not just
the classify fragment spliced into a hand harness. (ii) **Land into `xtask regen` + delete the hand
faces + real-tape fp** — PROTO-1 is a standalone binary; the twitter fp held via a classify-
independent lane, not a full real-tape `doc.walk` (the SoA `tape_dump.rs` binary was not rebuilt, out
of the probe's parsimony cap). (iii) **R2 — the flag plane cannot carry a discriminator tag AND an
escape bit simultaneously** (one `u8` slot); a string-escape leaf inside an operator tower needing a
rule tag collides — the ONE named spec obligation beyond HEAD, resolvable by a spec-declared bit-
partition (tag high bits, escape `GRAMMAR_BIT0`), unbuilt. (iv) The grammar→layout-fact anchor is
*partial*: `classify_materialization`+`compute_payload_layouts` derive a decode-class/tier +
byte-offsets (genuine), but fixed flag-bit *values* (`GRAMMAR_BIT0`) are hand-authored policy →
**Face I is PARTIALLY generated (record-shape + offsets GENERATED; fixed flag-bits LINTED)** — the §5
partition line. (v) Cross-repo regen (F6): the artifact ships inside the letter, regenerated
parse-that-side; the gate does not exist.

**Cross-lock.** LOCK-3 owns the schema names + the `emit_*` faces; LOCK-2 owns `MaterializationClass`
(Q5). Regex face deferred to LOCK-5's ONE-HIR verdict. The Q4 VM stream is owed jointly to
LOCK-4C/LOCK-8C (unresolved). Freeze AFTER LOCK-1 (schema input) + LOCK-7 (oracle source).

**Disposition: ADVANCED · crown BUILT at the classify face (SPEC_ONLY→RAN, PROTO-1) · full-decoder
scale + xtask landing remaining.** The schema face is BANKED-earned; the tape-layout face is now
**MEASURED 165→0 byte-identical on both backends with monomorphism preserved**, and the AoS↔SoA core
is DISCHARGED by run (D4). R3 is NOT refuted on the classify face — generation reproduced the hand
monomorphism exactly. This is the pass's largest crown conversion. Remaining: scale to the full
decoder, land into `xtask`, resolve R2 flag-collision, and the VM cross-lock. **Not BLOCKED** — no
missing primitive; the machine exists and runs.

---

## 3. Family 6C — WASM-CORE (the Rust core compiled to wasm is the one implementation)

**Thesis.** One Rust implementation parses source to the tape inside wasm; TS is bindings + an idiom
layer; TS-native code exists only where V8 measurably wins. The boundary is data, not behavior — four
SoA planes cross once, in bulk, as typed-array views; TS-native walks them (PROVE-14's 4.4×
stop-early *because* the boundary is crossed once). The dividing line is "scan vs walk," and PROVE-14
measured which side each belongs on.

**Why BANKED (confirmed; the unblock experiment ran and FAILED, pass 2).** PROTO-1(pass-2) resolved
R-A NEGATIVE: wasm SIMD128 at 0.34–0.47× native NEON, parity-exact — `simd-scan/wasm.rs` is a genuine
second implementation on the hottest surface, the dual-maintenance the family exists to abolish.
Compounded by F1 (full-walk 0.9×) and R-E (synchronous ESM cannot await `WebAssembly.instantiate`).
6C-as-independent-primary does not advance. **What is BANKED (load-bearing):** the **zero-copy tape
crossing** — 6B generates the byte contract, 6C crosses it once as typed-array views, 6D digests both
sides — confined to the bbnf side. This is the stratification's spine (§5 graft 1). BANKED, not
RETIRED: the crossing survives and the bbnf-side wasm-vs-TS-scalar decision is genuinely still open.

**Pass-3 findings (research-C + PROTO-2 scaffold).**
- **Q1 — the comparator is now SCAFFOLDED and RUNNABLE (PROTO-2), STILL UNRUN (D6/P2).** The Q1
  kernel is the existing portable `u8x16` nibble-LUT arm (`simd-scan/.../emit/simd.rs:85-135`,
  lowering to `v128` under `+simd128`) — realized by PROTO-2 as a standalone `#![no_std]` raw-C-ABI
  probe (`PROTO-2/wasm-scan/src/lib.rs`) with the marshalling tax (`TextEncoder` UTF-16→UTF-8 copy
  into linear memory) driven from JS and counted inside the wasm lane. The paired cold browser harness
  (`PROTO-2/harness.html`: monomorphic TS-scalar `Uint8Array` loop vs `wasmFull`, per-byte ns, 64B→
  64KB synthetic size sweep + real twitter/citm/canada, parity assert, cold single-shot) is authored.
  The design prediction (research-C): the winner is **size-dependent with a crossover** — wasm wins
  on large single docs (marshalling amortized), TS-native scalar can win on batches of small docs
  (per-call encode+copy dominates). **The comparison is NOT made** — no `PROTO-2.md`, no numbers, no
  browser run; a fourth consecutive deferral. But the harness is a build+run away (Brief 1, §6), not a
  design away.
- **Q2 — `memory.grow()` detach: YES, categorically.** Growing detaches the backing `ArrayBuffer`;
  every outstanding view invalidates (even `grow(0)`). **The seed already chose the robust design:**
  `decodeTape` copies each plane out into fresh JS buffers *before* constructing `TapeDoc`, and that
  copy (≈118 KB offset plane for twitter, 18.98% of source) is already inside the timed region while
  the tape still wins stop-early 4.4×. Copy-out is the correct safe default; PROTO-2's static bss
  arena never grows (side-stepping the hazard for the scan probe). The detach is a **counterexample to
  a zero-copy optimization** (shaving the copy silently corrupts a growing batch), not a live bug.
- **Q3 — value.js-4.0.0 divorce landed in-tree (XL-G P1 answered).** `/Users/mkbabb/Programming/
  value.js@4.0.0` (branch `tranche-u`) has **zero** `@mkbabb/parse-that` imports; `subpaths/
  {math,transform}.ts` assert `parse-that-FREE`. The confinement oracle evaporated from value.js; the
  live datum re-homes to **latex-paper** (still imports `Parser/regex/string/any` across
  `bibtex/parser.ts` + 6 `grammar/*.ts`) and keyframes.js (transitive). Flag F6/owner-bound
  (`tranche-u` is WIP; owner has not ratified 4.0.0 as canonical). **Pricing:** one consolidated
  cdylib (one boot, one `Memory`, copy-out per plane, regex vendored + statically linked = layout-
  compatible with XL-C one-regex-home) beats two cold instances (two instantiations, two memories,
  every cross-module datum copies through JS); async instantiation is a one-time boot obligation
  (Chrome-114 removed the 4KB sync limit), amortized by the batch target. Two-instance revives only
  if the owner names a consumer needing the regex engine *without* the generator (LOCK-8's contract).

**Residual open gaps.** (i) **The decision-relevant comparator (wasm-SIMD128 vs TS-native scalar) is
scaffolded-but-unrun a fourth time** — the bbnf-side viability decision (Brief 1, §6). (ii) The
crossing role is PROVE-14-anchored, not re-measured. (iii) R-B memory-grow gate: PROTO-2's static bss
side-steps it; the two-parses-through-one-growing-instance corruption case is still unexercised. (iv)
R-E rests on the now-obsolete "value.js sync sites" (moved to latex-paper, F6-bound). (v) Homing
collision unpriced (`wasm/Cargo.toml` already claims `name="bbnf-wasm"`; proposed `crates/bbnf-wasm`
absent). (vi) node ≠ browser: PROTO-2's harness IS a browser harness (Chrome arm), the correct
instrument — but it has not been run.

**Cross-lock.** 6C mirrors a runtime (tape ABI), not grammar-derived names (LOCK-3's map). Under 6C
the regex exemption dissolves favorably (Rust DFA→wasm, no TS-`RegExp` mirror) unless a measured
V8-wins carve-out keeps small/cold patterns TS-native. **Interlock: 6C.wasm-scan ⇐ 9A.kernel-home;
9B is the cost-bearing fallback.** LIVE x86 `avx2.rs`/`avx512.rs` in `simd-scan` (constitution iv
"x86 dead") is a LOCK-9 surface, routed to gestalt. Freeze AFTER LOCK-1, LOCK-5, LOCK-7, LOCK-9.

**Disposition: BANKED** (crossing-mechanism folded into the 6B/6D tape stratum; bbnf side). Fate
firm; bbnf-side viability decided by an unrun *build* now, not a missing design. **Not
BLOCKED-on-a-hard-primitive** — the `v128` kernel exists and is realized in PROTO-2; the missing piece
is the browser run (Brief 1). The pass-2 framing that 6C needed "wasm SIMD keeping the NEON win" is
corrected (the Q1 kernel is the portable arm, the boundary is the cost).

---

## 4. Family 6D — ORACLE-ISOMORPHISM / DIGEST (the mandatory FLOOR)

**Thesis.** The rust/ts split is held equivalent by a shared conformance corpus + a differential
value-digest ALONE — module tree, tape byte-layout, decoder skeleton, error wording free per
language. Correspondence is proven at runtime by the fold, not statically by names. Null hypothesis:
it wins by default, defeated only if measured drift/maintenance cost exceeds a cap's.

**Architecture — the canonical Merkle value-fold (research-D).** A fold from parse result to a
fixed-width digest, specified once, implemented idiomatically twice. FNV-1a core with a
domain-separation tag byte per kind; `Math.imul == wrapping_mul(u32)` per lane is the equivalence
that makes the two folds bit-exact (proven — the bits lane folded `b99874e8` identically Rust↔V8).
Array = sequential child fold (order IN contract); object = XOR-accumulate of member sub-digests +
count salt (**key order OUT of contract by construction**, proven `baseline == keyperm` by run);
string = tag++length++decoded UTF-16 code-units (astral guard — fold code units, never scalars);
accept/reject a parallel boolean channel; error-class joins only when the Q4 toggle is on.

**D1 — the fold RAN cross-language (§0), the artifact that never ran.** twitter.json folds to `fp
3744114272` and every counter identically; hard-case stress (`9007199254740993`,
`1.0000000000000002`, `5e-324`, `1e400`→inf, `-0`, astral) folds RUST == NODE. Injected-divergence:
static 4/5, fuzzer 5/5, latency 1 input once covered.

**D2 — the number lane FLIPPED to raw bits; UNADJUDICATED contradiction with pass-2.**
`contract-6D.toml` folds **`f64::to_bits()` little-endian** with a canonicalized residue
(`is_nan()`→sentinel `0x9E3779B1`, `-0`→`+0`, both BEFORE the byte fold; `1e400`→inf proven needing
no rule), arguing bits are the *superior* last-ULP guard (a shortest-decimal fold can mask an
adjacent-double error) and that correctly-rounded parse (Eisel-Lemire ↔ V8 StringToDouble) makes bits
a pure function of the decimal — sound and demonstrated. **BUT this reverses pass-2's registry-
credited Q10 correction** ("fold spans/canonical-decimal, NEVER raw bits") — bit→span→bit across
three passes, now with running evidence on the bits side. Reconcilable in principle (bits-with-
canonicalization ≈ the pass-2 concern addressed at the residue), but the checked-in contract commits
to bits while the pass-2 correction forbids them. **A live internal contradiction in the mandatory
floor's frozen contract, owed to GESTALT — a spec-probe settles it (fold the residue + the `1e21`
exponent-threshold both ways, see which diverges); LOCK-6 synthesis marks it OPEN, not resolved.**

**Q3 — marginal added-grammar cost (§4).** Bounded by the *alphabet*, not the grammar: the fold is
six node constructors keyed on the `Value` sum variant; for any grammar over that alphabet
(TOML/CSV/sheets) marginal fold LOC = **0** (re-point the fuzzer, a config line); a genuinely new
variant (CSS dimension, TOML datetime) costs exactly one node-hash constructor per side + one
canonicalization rule if it has a non-JSON residue. **6D's added-grammar cost is O(new variants); the
generator-mirror siblings' is O(new grammar)** — the measurable form of the null-hypothesis outcome.
End-to-end confirmation still owed: fold ≥1 non-JSON grammar's value tree (TOML datetime is the clean
test — but no TOML grammar exists in-tree, R1).

**Q4 — the in/out partition, as checked-in data (`contract-6D.toml`, authored this pass).** IN: the
folded value tree, accept/reject, the error-class enum
(`UnexpectedChar|UnterminatedString|TrailingData|DepthLimit|BadEscape|BadNumber`), number/string/
member fold rules, the UTF-16 code-unit length measure. OUT: error message wording (idiom), error
span byte-offset (layout), physical member order, internal representation. **Error-class toggle ON for
class equality, OFF for message + span** — a named supersession relaxing the intra-Rust
`css_scan_differential.rs` "same offset + message" to error-class-only across languages.
Canonicalization (spec-closed, corpus-unreachable): NaN→sentinel, `-0`→`+0`, `1e400`→inf — closed by
compile-time assertion + one golden vector each, never a corpus test.

**Mechanism / seams.** No invention — consolidation of mature intra-Rust differential machinery
(`checkasm_parity.rs` FFmpeg model + `BBNF_SIMD_INJECT_BUG`; `css_scan_differential.rs` clean-room
scalar reference; `bbnf-bench/src/parity.rs` plane-equality — the *layout* differential 6D discards).
The pass-3 gap closed: the Rust-side fold now exists (research-D, D1). Substrate change owed:
**widen u32→u64** (a u32 `fp` collides at ~77k inputs by the birthday bound; a fuzzer at 10⁶+ hits
it) — recorded in `contract-6D.toml [meta] digest="u64"`, still spec text not a diff. Feeds GN-4.W5
four-way parity close.

**Gates.** (a) 100% digest agreement across Rust eager/stop-early/typed + TS `TapeDoc` + a
third-party reference — PROVE-14 parity as STANDING law; (b) the in/out partition as data (authored);
(c) three-way accept/reject/error-class partition so the gate is stable.

**Exact open questions.** (i) **Both verdict NUMBERS still not fully closed** — the pass measured
proxies (added-grammar marginal ≈ 0 fold LOC; injected-divergence catch rates); the **R5
debuggability comparison** (time-to-root-cause under a bare digest mismatch — "something differs" —
vs mirror-maintenance-cost; the axis where 6D is *dominated*: a root-digest mismatch does not say
"escape decoder off-by-one at line X") is the single measurement the "reasonable point" verdict turns
on, and it is unrun. (ii) **The bits-vs-spans number-lane contradiction (D2)** — unadjudicated,
routed to GESTALT. (iii) The u64 widening is spec text, not a diff; the fuzzer at 10⁶+ inputs unrun.
(iv) LOCK-7's N+1 reference flagging a *seeded* both-engines-wrong case (the oracle proves agreement,
not correctness) unrun. (v) The coverage-guided fuzzer teeth (the empirical case from §0) — the
generator itself is unbuilt; gate-(a) still exercises mostly the easy direction.

**Cross-lock.** A digest-only family needs no name map. Regex → behavioral conformance (same match
span/captures/accept-reject over a shared corpus), compatible with LOCK-5 ONE-HIR without an
allow-list freeze. **DAG (binding): freeze strictly AFTER LOCK-1 (alphabet = grammar-derived `Value`
ADT / `DirectTypeKind` sum) and LOCK-7 (tie-breaker = bbnf-in-bbnf reference, the N+1th version that
adjudicates both-wrong)** — recorded in `contract-6D.toml [dag]`.

**Disposition: ADVANCED · the mandatory FLOOR.** The cross-language fold RAN (D1) — the pass-2
"never-run mechanism" is discharged at corpus scale and in a minimal instance; the contract is
checked-in data (Q4); the injected-divergence numbers ran. Genuine, large advances. But
**pure-6D-as-whole-answer stays BLOCKED for the tape** ("layout free per language" negates the
byte-identical SoA layout the zero-copy stop-early win requires — 6D wins as verification substrate,
loses as the whole answer for the tape), the number-lane representation is internally contradicted
(D2), the R5 debuggability verdict number is unrun, and the fuzzer teeth are unbuilt. 6D is confirmed
the floor; its *cost/benefit as the chosen floor* is measured on proxies, not closed.

---

## 5. The incompatibility map (which families cannot coexist, and precisely why)

The families are one mandatory floor (6D) and three candidate structural caps (6A/6B/6C) that
conflict **only on a shared surface**. Once surfaces are partitioned the conflicts dissolve into a
stratification; drawing that partition is the next pass's central deliverable, and it now rests on
one MEASURED number (6B's tape-face gen-vs-hand, classify face, PROTO-1) plus one still-owed number
(6A's ratio *from the real linter*, not the python differ — Brief 2) over the load-bearing
6D-is-a-mandatory-floor claim (D1 advanced it; R5 closes it).

**Genuine pairwise incompatibilities (same surface):**

1. **6A ⊥ 6B — exclusive per item.** Generate-vs-hand-write-and-lint cannot both govern the same
   item: a 6B-generated pair has no independent hand twin for 6A's name-map to check — the linter
   would compare the generator's two outputs, a tautology (Y.13). They coexist ONLY by partitioning
   the surface, and pass-3 draws the line with a MEASURED anchor now on the 6B side: the schema + the
   N-grammar tape/**classify** faces are GENERATED (6B, PROTO-1 byte-identical both backends); 6A's
   genuine non-tautological load is **Class-2 (the hand-authored taxonomy + the dual-authored
   `classify` routing that PROTO-1 does NOT certify — it certifies the byte-switch is generated, not
   that the two grammars' *semantics* agree) + the hand-copied grammar-derived constants**
   (`GRAMMAR_BIT0`, LINTED not generated — Face I partial). The partition still needs 6A's linter
   BUILT (Brief 2) to draw its side. On the combinator **type** algebra 6B is REFUTED (ts-rs flatten)
   → 6A + 6D there; **but 6B-as-layout-gen (the LOCK-4C VM stream) is a live re-opening the type-gen
   refutation does not touch** (§2 Q4, strengthened by PROTO-1's machine generalizing to `OpcodeDecode`)
   — OPEN, owed to LOCK-4/LOCK-8.

2. **6C ⊥ 6A and 6C ⊥ 6B — on absorbed surfaces.** Where 6C absorbs a surface into the wasm core
   there is no second implementation to mirror (6A) or generate to (6B). But PROTO-1(pass-2)'s
   R-A-negative means the absorbed surface does **not** include the hot scan kernel (it stays a dual
   implementation, `simd-scan/wasm.rs` / PROTO-2's `v128` probe) — 6C absorbs only the *crossing*, not
   the *scan*.

3. **6C ⊥ the parse-that consumer contract (family-vs-constraint, hard).** Synchronous ESM
   `all()`/`dispatch()` cannot await `WebAssembly.instantiate` (R-E) → confines 6C to the bbnf side.
   Softened at the boundary (one-time boot, not per-call async tax — Chrome-114) but the confinement
   stands. The value.js-4.0.0 divorce (research-C Q3) *confirms* the confinement can be dissolved by
   divorce, and *evacuates* the confinement oracle from value.js to latex-paper.

4. **6D-pure ⊥ the tape plane (family-vs-measured-requirement).** "Layout free per language" negates
   the byte-identical SoA layout the zero-copy stop-early win (F2/F3) requires. 6D-as-whole-answer
   conflicts with the tape's measured requirement; 6D-as-floor does not. The null hypothesis is
   defeated *for the tape* and survives *as verification*.

5. **[internal to 6D] number-lane bits ⊥ spans (D2).** `contract-6D.toml` folds raw `f64::to_bits()`
   LE (+ canonicalized residue); pass-2's registry-credited Q10 correction forbids raw bits. Both now
   have arguments; the bits side has running evidence (D1). **Unadjudicated** — a spec-probe settles
   it, GESTALT ratifies. Not cross-family, but a real contradiction in the floor's frozen contract.

**Compatibilities (the stratification the incompatibilities resolve into):**

| surface | primary mechanism | verification |
|---|---|---|
| tape layout (4 SoA planes) | **6B** generates the byte contract, **6C** crosses it zero-copy (bbnf side) | 6D digest + T0 byte gate |
| classify / decoder skeleton / schema face (N-grammar) | **6B** — classify face MEASURED 165→0 byte-identical both backends (PROTO-1); full-decoder scale + xtask landing remaining | 6D + `%HaveSameMap` (PASSED, classify face) |
| fixed flag-bit values / ~15 layout constants | **linter** (6A/pairing-linter — generating them over-builds, Q3) | 6D |
| value-materialization / walk | **TS-native** (6C-tempered "where V8 wins"); skeleton optionally 6B | 6D |
| combinator **type** algebra | **hand-written** (6B-type-gen refuted); **6A** name-map | 6D behavioral |
| combinator **layout** (IF LOCK-4C VM lands) | **6B-as-layout-gen** (shared instruction stream; PROTO-1's machine generalizes) — **OPEN, owed to LOCK-4/8** | 6D behavioral |
| module tree / names / idiom residue | **6A** trichotomy (Class-2 name-map + Class-1 provenance + Class-3 budget) — LINTER UNBUILT (Brief 2) | 6D |
| everything (behavior) | — | **6D** — the mandatory floor (fold RAN, D1) |

**Cross-pollination (named grafts).** (1) 6B ⊗ 6C — the generated tape byte-contract crossed
zero-copy (PRIMARY, the stratification's spine): 6B without a crossing is a spec with no consumer; 6C
without a generated contract is two hand-transcribed alphabets. Pass-3: the 6B leg is now BUILT at
the classify face (PROTO-1); the 6C crossing is PROVE-14-anchored; the wasm-scan run (Brief 1)
decides the bbnf-side home. (2) 6A ⊗ 6D — 6A's Class-2/T3 corpus IS 6D's differential corpus: 6A is
"6D plus a structural tax" by construction; the PROVE-14 drive is the genuine cross-language backstop.
(3) 6B ⊗ 6D — the generated oracle table: 6B generates the digest comparator into BOTH a Rust fixture
and a TS test, so GN-4.W5's self-stub (`get==get`) becomes unwriteable. Pass-3: the 6B oracle face is
still 0% built (PROTO-1 built the classify face, not the oracle face).

**BLOCKED-candidate summary.** 6C-as-primary: **BANKED** (refuted maximal; confined by R-E; hot-kernel
perf premise refuted by pass-2 R-A) — *not* blocked on a missing primitive: the `v128` kernel exists
and is realized (PROTO-2), the remaining question is an unrun browser measurement (Brief 1). 6D-as-
whole-answer: **BLOCKED for the tape** (layout freedom negates byte identity). 6A and 6B are
**ADVANCED**, mutually exclusive per surface, each strictly dependent on the 6D floor. **6B's central
product is now BUILT** at the classify face (PROTO-1) — the pass converts it from SPEC_ONLY to RAN;
**6A's central product (the linter) is now the single largest remaining SPEC_ONLY crown** (Brief 2).
Neither 6A nor 6B is BLOCKED: 6A's cap is grep-derivable (D3), 6B's R2 collision is a spec-declared
bit-partition.

---

## 6. The two sharpest uncertainties for the next pass

The plane's verdict rests on drawing the partition line (§5). Pass-3 converted 6B's crown from
SPEC_ONLY to RAN (PROTO-1) and ran 6D's cross-language fold (D1). The two remaining unbuilt/unrun
crowns — 6C's decision (harness authored, run owed a fourth time) and 6A's linter (never built, the
registry's #1 6A directive, three passes undischarged) — are the two sharpest, most decision-relevant,
now-runnable uncertainties. Both convert the largest remaining SPEC_ONLY items to BUILT/MEASURED and
draw the two sides of the partition the 6B anchor already grounds.

- **Brief 1 (6C, `bbnf-worktree`) — RUN the already-authored wasm-SIMD128 vs TS-native-scalar
  comparator.** The `v128` kernel (`PROTO-2/wasm-scan/src/lib.rs`) and the paired cold browser harness
  (`PROTO-2/harness.html`) exist; only the build + browser run is owed (fourth deferral). Decides the
  tape-crossing stratum's spine (the 6B⊗6C graft) and the bbnf-side wasm-core home; the size-crossover
  prediction (wasm wins large single docs, TS-scalar wins batches of small) is directly falsifiable.
- **Brief 2 (6A, `bbnf-worktree`) — BUILD the `mirror.toml` manifest linter on the one live pair and
  drive the TS surface behaviorally.** The `syn` declaration extractor + `dist/*.d.ts` extractor +
  `cap = Σ predicate-sites` + RED-on-synthetic + the real `serde-wasm-bindgen`/Node crossing of
  `ts/path.ts` — the never-built mechanism that IS 6A, converting the single largest remaining
  SPEC_ONLY crown to BUILT and drawing 6A's side of the §5 partition.

*(Full self-contained briefs returned in the structured output. Two items are surfaced as owed to the
GESTALT rather than to a LOCK-6-solo brief: the **LOCK-4C VM combinator-layout fork** — owed jointly
to LOCK-4/LOCK-8, resolvable only by research-B §5's cross-lock two-opcode probe, now strengthened by
PROTO-1's machine generalizing to `OpcodeDecode` — and the **6D number-lane bits-vs-spans
contradiction (D2)**, settled by a spec-probe and ratified at GESTALT. 6B's remaining work — scale the
emit machine from the classify face to the full decoder + land into `xtask regen` — is an
engineering follow-on of a BUILT-and-RAN machine, not an uncertainty, so it is not a brief.)*
