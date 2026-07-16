# LOCK-6 — THE ISOMORPHISM PLANE (rust/ts split): FAMILY SPECS (synthesis, pass 3)

*Synthesis, Fable-tier, pass 3. One cogent spec per family while the routes remain incompatible,
then the incompatibility map. The plane is not four rivals: it is **one mandatory behavioral FLOOR
(6D) and three candidate structural CAPS (6A/6B/6C)** that conflict only where their surfaces
overlap. The deliverable is the **partition line (§5)**, drawn by two reasonable-point numbers —
6A's clean-map ratio (MEASURED 47/61/0) and 6B's gen-vs-hand LOC on the **tape-layout face** — over
the load-bearing claim that 6D is a mandatory floor (structural caps necessary-not-sufficient).
Elegance earns nothing: a family stalled at a missing primitive equal in difficulty to the original
problem is BLOCKED-candidate.*

*Binding cross-lock frame (facts, XL-A..H, do not re-litigate): substrate is **skinny@HEAD**
(the only living implementation / measurement substrate); designs target the greenfield workspace;
top-level `crates/` is legacy (archives at GN-0). Grammar tier holds Regex opaque (`StringId`),
engine tier holds rich `Hir` (XL-C). The fast TS face is the BUILD-TIME EMITTER; runtime decoders
survive only as isomorphism/correctness ORACLES (XL-D). LOCK-3 owns the middle-IR tower +
decision-driver seat (XL-B, working). Every number names its HEAD/base (XL-H).*

---

## 0. What pass 3 moved (verified deltas the specs now carry)

Pass 2 left every crown *mechanism* SPEC_ONLY and named the RE-EMERGENT prototype-allocation defect
(both probes measured softer targets than the sharpest uncertainty). Pass-3 research moved the
**feasibility** frontier on three families and left the **mechanism** frontier where it was on two:

- **D1 — 6D's cross-language fold RAN (research-D §2), the artifact that had NEVER run in any form.**
  Two running instances, not spec text: (a) a std-only Rust recursive-descent parser and a V8 arm
  fold `twitter.json` to **byte-identical `fp 3744114272`** and every counter; (b) a ~90-LOC
  `fold.rs`/`fold.mjs` pair folds astral/`-0.0`/`f64::MAX`, with `baseline == keyperm` (object order
  proven out-of-contract *by run*) and `1ulp` diverging identically cross-language. The
  injected-divergence table (§3) ran: **static corpus 4/5, coverage-fuzzer 5/5**, detection latency
  1 input once the class is covered (class-4 bool-collapse escapes forever on a bool-free corpus —
  the empirical case for the fuzzer). This is the single largest SPEC_ONLY→RAN conversion of the pass.
- **D2 — 6D's number lane FLIPPED BACK to raw bits (research-D §1 + `contract-6D.toml`).** Pass-2's
  registry-credited Q10 correction was "fold source-byte number *spans* / RFC-8785 canonical decimal,
  **NEVER** raw bits." Pass-3 folds **`f64::to_bits()` little-endian** and argues bits are the
  *superior* last-ULP guard (a shortest-decimal fold can mask an adjacent-double error) *provided* the
  residue is canonicalized (`[canonicalize]`: NaN→sentinel, `-0`→`+0` before the byte fold). This is
  a real, **unadjudicated internal contradiction** in the mandatory floor's spec — bit→span→bit
  across three passes — now with running evidence on the bits side but in tension with the pass-2
  correction. Surfaced §4, routed to gestalt.
- **D3 — 6A's cap is re-derived defensibly (research-A Q1).** The arithmetically-inconsistent scalar
  `2·T+I` is **gone**, replaced by `cap = Σ_{c∈I} |source-tree sites matching class c's predicate|`
  — computed from the tree by grep, never from the exemption file. I is resolved: **5 idiom classes**
  (TYPEREP, OWNERSHIP, KERNEL, SUBSTRATE, ORACLE-DIR); CASE excluded as a total injective transform
  (zero budget). A well-formed map has one exemption per predicate-site → `#exemptions = cap`
  identically (reproduces the count without being fit to it); a synthetic extra with no backing site
  fails its predicate → RED. The pass-2 gap-1 defect is dissolved at the *design* level.
- **D4 — 6B's AoS↔SoA "hard core" is dissolved into a discriminator-provenance spectrum
  (research-B §2).** The tape is **always** SoA (`offsets` + sparse `flag_cursors`/`flag_values`);
  "AoS vs SoA" was never a storage question. The real axis is **discriminator provenance**, and the
  sparse flag plane is one polymorphic slot carrying whichever provenance a grammar needs. Three
  dialects already ship at HEAD over the identical physical tape: JSON `FromSource` (kind from
  `source[offset]`), Sheets `FromFlagTag` (kind stored in the flag byte, recovered by `KIND_TABLE`),
  CSS-L4 `FromSourceThenFlag` (byte partial-discriminant completed by the flag). The pass-2
  "parked-definitional" reconciliation is answered *by the tree*, not deferred.
- **D5 — the seams research-B grounds on are `skinny/crates/…`; the tree canon is contested.** research-B
  cites `skinny/crates/{codegen,runtime}/…`; pass-2 FAMILY-SPECS grounded on repo-root `crates/`
  (legacy); the REGISTRY seam-hygiene notes a `crates/core` consolidation. XL-A makes **skinny@HEAD**
  the measurement substrate, so research-B's grounding is defensible — but any 6B prototype must
  **confirm the emit_* seam locations at skinny@HEAD first** (the pass-2 seam-relocation obligation
  stands; D5 does not discharge it).
- **D6 — the thrice-asked 6C comparator is STILL unrun.** research-C reframes it favorably (the Q1
  wasm kernel is the *existing* portable `u8x16` nibble-LUT arm at `simd-scan/.../emit/simd.rs:85-135`,
  lowering to `v128` under `+simd128` — no new emitter, only a build), confirms Q2 (`memory.grow`
  detach) and Q3 (value.js-4.0.0 divorce landed in-tree), but the decision — **wasm-SIMD128 vs a
  TS-native scalar scan, both browser-deployable** — is not measured a third consecutive pass.

---

## 1. Family 6A — MIRROR-TREE (hand-authored both sides + a CI isomorphism linter)

**Thesis.** Both trees stay hand-written and idiomatic per language; a checked-in machine-readable
manifest is the single source of truth for what mirrors what; a CI linter proves it holds against
extracted *declaration surfaces* (never bodies). Kotlin-Multiplatform `expect`/`actual` reconstructed
across two repos, the linter standing in for the single compiler no cross-repo toolchain provides.
**Scoped:** the structural mirror covers only faces bearing a *layout or oracle contract* (the tape
planes, the flag channel, the `Value` variant set, oracle tables); engine internals (combinator
algebra, regex automata, SIMD kernels) are **out of domain**, conceded to 6D behavioral-only. This
is the honest locus of "reasonable point," and it is exactly where PROVE-14 already delivers
byte-exact cross-language evidence — not a retreat, the drawn boundary (research-A §0).

**Architecture — the Q2 trichotomy routes the linter (pass-2 advance, carried).** Three disjoint
classes, each a different tautology status and a different lawful check:
- **Class 1 — grammar-derived names** (field/rule names, kind discriminants, plane order, the flag
  bits). ONE source, minted by the emitter. Name-checking is tautological. **Route → PROVENANCE**
  (both sides read the identical generated sidecar; hand-authored fork → RED) — anchored *and*
  non-tautological because the proposition is "single-source held."
- **Class 2 — hand-authored-twice taxonomy** (the `PathErrorReason`-class enums/structs authored on
  both sides; the dual-authored `classify`/`JsonNodeKind` byte→kind switch). The ONLY class where a
  name-map is non-tautological, and **where drift LIVES** (the `nope` case: 8 variant names
  correspond perfectly while the routing forked). **Route → NAME-MAP** (set-equal variant/field
  sets; TS switch exhaustive over the union).
- **Class 3 — idiom residue** (`#[wasm_bindgen]` exports, ownership types, SIMD kernels, native
  `RegExp`, lifetime-shape). No twin. **Route → EXEMPTION BUDGET.**

**Q1 — the cap re-derived from the tree (D3, the pass-3 advance that dissolves gap 1).**
`cap = Σ_{c ∈ I} |source-tree sites matching predicate[c]|`, grepped from **source**, never from the
exemption file. `T` = two-sided `[[pair]]` rows in `mirror.toml`; `I = 5` frozen idiom classes in
`idiom-classes.toml` (a reviewed constitutional edit to add one). Properties, each argued: reproduces
the live count without a fitted constant (one exemption per predicate-site ⇒ `#exemptions = cap`);
RED on a synthetic extra (no backing site ⇒ predicate fails ⇒ `#exemptions > cap`); defended growth
is legal only by adding a real divergence site (a new `*Builder`, a new `_simd.rs` raises `cap` by
exactly the sites added). The load-bearing invariant is a **tight predicate per class** (a loose
predicate inflates `cap` and hides drift) — grep-verified against the tree in CI, reviewed on change:
the one bounded place human judgment enters.

**Q2 — two representative names traced (research-A §Q2).** (1) The four planes
(`offsets`/`flag_cursors`/`flag_values`/`source`) = **SINGLE source**: Rust `Tape` fields → wasm
export ABI → CASE-transform → TS `TapeDoc` fields; a bijection with one authoring site, class CASE,
zero budget. (2) `classify`/`JsonNodeKind` = **DUAL-authored, oracle-enforced**: the tape stores no
discriminator; Rust's generated `classify` and TS's hand `classify` re-implement the same byte→kind
switch independently, joined only by fp parity — the mirror-tree's honest limit and the seam to 6D.
(bonus) `STRING_NEEDS_DECODE = 0x01 = GRAMMAR_BIT0` = **grammar-derived, hand-copied** into a TS
literal that silently rots on a bit renumber — the whole argument for the Q3 provenance linter.

**Q3 — linter ownership (research-A §Q3).** Split along where the name is *born*: **LOCK-3 owns the
provenance MANIFEST** (a regen byproduct listing every grammar-derived symbol + canonical
value/layout — its consumer is the linter, satisfying Y.13); **LOCK-6 owns the name-map LINTER +
`mirror.toml`** that consumes it and asserts Rust-emission = manifest, TS-mirror = manifest,
structural budget holds. Forbids the name-gate anti-pattern (Constitution vi): provenance-equality
against a generated manifest, never a hand-maintained allowlist. Prior art: tree-sitter
`node-types.json`, Cap'n Proto schema→descriptor, rustc `sourcegen` up-to-date check.

**Mechanism / seams.** `syn` over the Rust mirror declarations; TS surface from built `dist/*.d.ts`;
the sidecar manifest; checked-in `mirror.toml` + `idiom-classes.toml` + `type-map.toml`. The live
behavioral drive is **PROVE-14**: Rust `parse_tape` builds the tape → dumps the four planes → TS
`TapeDoc` decodes → `verify.mjs` folds a 32-bit fingerprint against a `JSON.parse` walk (320 cold
children, `fp 986912547`, `codeUnits 304329` identical). This is a genuine Rust→TS drive (not the
pass-2 Rust↔Rust `floor_probe` proxy), and it is the backstop 6A *needs* for the dual-authored
`classify` no structural map can certify. Seam census (parse-that): OWNERSHIP `bump_slab.rs`,
`span_parser/`; KERNEL `parsers/scan/*_simd.rs`; SUBSTRATE `rust/regex/` ↔ TS `leaf.ts:317-369`
native `RegExp`; CASE `combinators/methods/{sep_by,minus}.rs` ↔ `parser.ts`.

**Gates.** (a) Class-2 mapped-set 0%-tolerance RED; (b) allow-list monotonicity RED; (c) impedance
cap `#exemptions ≤ Σ predicate-sites`; (d) provenance gate (manifest ≠ TS literal → RED); (e)
file-tree correspondence SOFT-warn (trees genuinely diverge: Rust dir-modules ↔ TS flat `parser.ts`;
concept granularity mirrors, file granularity does not — the linter operates on concept↔{paths}).

**Exact open questions.** (i) **The mechanism is STILL UNBUILT** — the `mirror.toml` manifest linter
(syn extractor + `dist/*.d.ts` extractor + `cap = Σ sites` + RED-on-synthetic) never ran; research-3
is spec + census + the PROVE-14 drive, not the linter. The registry's #1 6A directive is undischarged.
(ii) The green/red PAIR at the linter level (T1/T2 green while T3 goes RED) is asserted about
non-existent tooling. (iii) The Class-2 "only non-tautological" assignment is routed, not *proven*
single-vs-dual for a representative name at the linter (research-A traces names by hand; the linter
must). (iv) CASE totality+injectivity is a gate (a `next`/`next_sibling`→one camelCase collision is a
failure), unbuilt. (v) Cross-repo CI ownership under F6 (per-repo manifest fragments + vendored
corpus) — proposed, unbuilt, the family's hardest logistics.

**Cross-lock.** LOCK-3 owns the provenance manifest; LOCK-6 owns the linter (Q3). Rust-DFA↔TS-`RegExp`
is a Class-3 SUBSTRATE exemption, behavioral-only, compatible with a LOCK-5 ONE-HIR verdict that
forbids a *layout* mirror there. Freeze AFTER LOCK-1 (the ADT the map ranges over) + LOCK-7 (the
tie-break oracle).

**Disposition: ADVANCED · dependent-on-6D · SPEC_ONLY core.** The predicate-derived cap (D3) resolves
the pass-2 arithmetic-inconsistency defect at the design level; the trace + ownership answers are
sound; the PROVE-14 drive is the genuine cross-language backstop the pass-2 proxy lacked. But the
linter that IS the family remains unbuilt, and 6A's own Class-2 finding proves it is
*necessary-not-sufficient* (names green while routing forks) — 6A *contains* 6D's T3 as a mandatory
tier, never standalone. **Not BLOCKED** — the cap is now grep-derivable (no missing primitive equal
to the original problem); the family is buildable, merely unbuilt.

---

## 2. Family 6B — SPEC-GENERATED-SKELETON (one spec generates both skeletons)

**Thesis.** The isomorphism problem has a declarative half (drift-prone, low value-per-line, high
correctness stakes: byte layout, tape contract, type interfaces, oracle tables) and an imperative
half (idiomatic, V8/LLVM-bound: the decode walk, the SIMD scanner). Generate the declarative half
from one grammar-derived `LayoutSpec`; hand-write the imperative half; draw and MEASURE the boundary.
Drift becomes impossible-by-construction where generated, a regen diff not a code review.

**Architecture — `LayoutSpec` + the discriminator-provenance spectrum (D4, the pass-3 advance).**
The spec exists implicitly today as the per-grammar `config.rs` (the byte-layout descriptor:
`STRUCTURAL_BYTES`, `STRING_NEEDS_DECODE = GRAMMAR_BIT0`, literal tables, the backend-shape policy);
6B promotes it from a Rust-string-template output to a **backend-agnostic data structure** two
emitters (Rust and TS) both consume. The one axis the live `LoweredArtifact` (`SinkOnly | Scan`)
lacks is **discriminator provenance**:
```
Discriminator = FromSource(byte→kind)          # JSON: classify(source[offset])
              | FromSourceThenFlag(byte, flag, default)   # CSS-L4: byte partial, flag completes
              | FromFlagTag(KIND_TABLE)         # Sheets: kind stored in the flag byte
              | Scan                            # general-descent: no per-node kind
FlagSemantics = EscapeBits | DiscriminatorTag   # the polymorphic sparse-plane slot
```
All three non-Scan dialects **already ship at HEAD** over one physical SoA tape (research-B §2):
JSON's flag plane is a 1-bit escape marker; Sheets writes a `u8` rule-order tag into the *same*
slot; CSS-L4 reads byte-then-flag. One `LayoutSpec` expresses all three because they are three
projections of one flag plane — the AoS↔SoA reconciliation is answered by the tree, not parked.

**Q1 — gen-vs-hand LOC on the tape-layout face (research-B §3, the RIGHT face this pass).** Measured
on the tape face (byte layout `config.rs` + per-node decoder `value.rs`/`view.rs`), all `@generated`:
JSON **691/0**, Sheets **283/0**, CSS-L4 **65+/0** — the **Rust tape face is ~100% generated across
three grammars**. The TS side is **0% generated / 703 hand LOC** (PROVE-14's `.mjs`), whose
`classify` hand-mirrors `JsonNodeKind::at_cursor` and whose `needsDecode` binary-search hand-mirrors
`Tape::flags_at` — the exact two-hand-copies-of-one-law drift 6B exists to kill. **Caveat (the
registry's live complaint, only partly closed): the 703→0 is a PROJECTION, not a measured
post-generation number — the TS `emit_classify` face is still 0% BUILT.** 6B moved from "0% measured
on the tape face" to "Rust side confirmed 100%-generated, TS drift quantified at 703 hand LOC, TS
emitter unbuilt." The crown *product* is still SPEC_ONLY (Brief 1).

**Q3 — idiomatic-consumer adapter (research-B §4).** For a native struct filled by `parse_into::<T>`
via the generated fused single-pass sink (`json_typed_direct.rs`, grammar-derived `DirectSchemaSet`),
the adapter cost is **negative on the hot direction** — direct-to-struct beats the faithful `JsonValue`
union walk (it skips union materialization; SPEC §0 row-07 CONFIRMED 1.27–1.34× vs sonic-typed). Only
struct→value re-serialization pays. Caveat: measured Rust-only; a TS `emit_direct_sink` face is the
symmetric obligation, unbuilt.

**Q4 — shared bytecode instruction stream as a generation target (the one live cross-lock item).**
research-B §5 argues **YES, a type-generation refutation does not bind it**: bytecode carries no
generics to erase (the tape's `u32` cursors and a VM's `u32` opcodes are the same monomorphic
substrate); the `LayoutSpec`/`emit_*` machine is layout-shape-agnostic (`offsets[cursor]→node` vs
`code[pc]→instruction`); the binding refutation (full-walk 0.91×) is a perf-non-goal, orthogonal.
Prior art: LPeg/RE2 parsing-VM bytecode, WASM itself as a shared instruction stream. **Routing:**
LOCK-4 owns the instruction-set design, LOCK-8 owns the TS interpreter shape, 6B contributes the
generator + the differential oracle. **This cannot be resolved inside LOCK-6** — it is owed jointly
to LOCK-4/LOCK-8 (research-B §5's proposed joint probe: emit a two-opcode stream into a Rust
`match pc` and a TS `switch(pc)`, assert identical output digest — the bytecode analogue of
PROVE-14). It stays the seam GESTALT-P collides on first.

**Q5 — ownership (research-B §6).** `LayoutSpec` (type) + the `emit_*` faces → **LOCK-3** (the
backend-agnostic middle + transpositions; where `render` + the three `emit_*` live). The
`MaterializationClass` variable (extended with the discriminator-provenance axis) → **LOCK-2** (the
*choice* is a decision, the live `LoweredArtifact` selected by a shape probe). Handoff: **LOCK-2
resolves `MaterializationClass` + `LayoutSpec`; LOCK-3 emits both backends** — mirrors the extant
`lower_for_runtime`→`render` seam exactly. Named-as-open in the registry; this is the proposed
assignment, still owed to LOCK-3/LOCK-2.

**Mechanism / seams (D5 — confirm at skinny@HEAD first).** research-B grounds on
`skinny/crates/codegen/src/runtime_generator.rs:{19-24,26-71,78-87,89-278}` (`LoweredArtifact`,
`emit_from_request`/`lower_for_runtime`, `render`, the three `emit_*`), the config specs
`grammars/{json,sheets,css_l4_declaration_values}/…`, and the tape `runtime/src/tape/mod.rs:92-99`.
The pass-2 seam-relocation obligation stands: any prototype confirms these paths at skinny@HEAD (XL-A)
before building, and replaces `include_str!("json_templates/value.rs")` with `emit_value(&spec,
Backend)` — generating from **data**, not a second `include_str!` TS template (R5).

**Gates.** (a) lockstep regen — one spec-line edit + `xtask regen` moves all faces while PROVE-14
byte-exact parity (6 counts + `codeUnits` + `fp 986912547`) holds; (b) `%HaveSameMap` +
`%HasFastProperties` V8 shape probe under `node --allow-natives-syntax` on the GENERATED TS decoder
(the 4.4× stop-early depends on single-hidden-class monomorphism — asserted by grep in PROVE-14,
never run); (c) the born-RED wasm-plane byte-identity cross-check (GN-4.W4 gate b).

**Exact open questions.** (i) **The TS emitter — the crown product — is 0% BUILT** (Brief 1); the
703→0 is projected, `%HaveSameMap` never ran. (ii) **R2 — the flag plane cannot carry a discriminator
tag AND an escape bit simultaneously** (one `u8` slot). A grammar with a string-escape leaf *inside*
an operator tower needing a rule tag collides — the ONE named spec obligation beyond HEAD, resolvable
by a spec-declared bit-partition (tag high bits, escape `GRAMMAR_BIT0`), unbuilt. (iii) The
grammar→layout-fact anchor is *partial*: `classify_materialization`+`compute_payload_layouts` derive
a decode-class/tier + byte-offsets (genuine), but the fixed flag-bit *values* (`GRAMMAR_BIT0`) are
hand-authored policy → **Face I is PARTIALLY generated (record-shape + offsets GENERATED; fixed
flag-bits LINTED)** — the §5 partition line, measurement-confirmed. (iv) Cross-repo regen (F6): the
artifact ships inside the letter, regenerated parse-that-side; the gate does not exist.

**Cross-lock.** LOCK-3 owns the schema names + the `emit_*` faces; LOCK-2 owns `MaterializationClass`
(Q5). Regex face deferred to LOCK-5's ONE-HIR verdict. The Q4 VM stream is owed jointly to
LOCK-4C/LOCK-8C (unresolved). Freeze AFTER LOCK-1 (schema input) + LOCK-7 (oracle source).

**Disposition: ADVANCED · SPEC_ONLY crown-product.** The schema face is BANKED-earned (settled
generation — stop re-measuring it). The tape-layout face is now Rust-side-measured-100%-generated and
the AoS↔SoA core is dissolved (D4) — genuine advances. But the TS emitter is 0% built, R3 survives on
the TS face until the 703→0 is *measured* not projected, and the R2 flag-collision + the VM cross-lock
are unresolved. **Not BLOCKED** — the R2 collision is a spec-declared bit-partition, not a missing
primitive equal to the original problem.

---

## 3. Family 6C — WASM-CORE (the Rust core compiled to wasm is the one implementation)

**Thesis.** One Rust implementation parses source to the tape inside wasm; TS is bindings + an idiom
layer; TS-native code exists only where V8 measurably wins. The boundary is data, not behavior — four
SoA planes cross once, in bulk, as typed-array views; TS-native walks them (PROVE-14's 4.4×
stop-early *because* the boundary is crossed once). The dividing line is "scan vs walk," and PROVE-14
measured which side each belongs on.

**Why BANKED (confirmed; the unblock experiment ran and FAILED, pass 2).** PROTO-1 resolved R-A
NEGATIVE: wasm SIMD128 at 0.34–0.47× native NEON, parity-exact. `simd-scan/wasm.rs` is a genuine
second implementation on the hottest surface — the dual-maintenance the family exists to abolish.
Compounded by F1 (full-walk 0.9×) and R-E (synchronous ESM cannot await `WebAssembly.instantiate`).
6C-as-independent-primary does not advance. **What is BANKED (load-bearing):** the **zero-copy tape
crossing** — 6B generates the byte contract, 6C crosses it once as typed-array views, 6D digests both
sides — confined to the bbnf side. This is the stratification's spine (§5 graft 1). BANKED, not
RETIRED: the crossing survives and the bbnf-side wasm-vs-TS-scalar decision is genuinely still open.

**Pass-3 findings (research-C).**
- **Q1 reframed favorably but STILL UNRUN (D6).** The Q1 kernel is NOT the NEON kernel recompiled —
  it is the **existing portable `u8x16` nibble-LUT arm** the generator already emits for wasm32
  (`crates/core/src/generate/regex/emit/simd.rs:85-135`, the `#[cfg]` wasm arm routes to
  `find_next_structural_from`), which lowers to `v128` under `+simd128` — one existing cross-arch
  parity-tested scanner, only a build needed. The decisive cost is **the boundary, not the kernel
  width**: the UTF-16→UTF-8 encode+copy `parse(src:&str)` pays every call (the wasm-bindgen-loses-to-JS
  datum) → the winner is size-dependent with a crossover (wasm wins on large single docs, TS-native
  scalar can win on batches of small docs). **The comparison is not made** — a third consecutive pass.
- **Q2 — `memory.grow()` detach: YES, categorically.** Growing detaches the backing `ArrayBuffer`;
  every outstanding view invalidates. **But the seed already chose the robust design:** `decodeTape`
  copies each plane out into fresh JS buffers *before* constructing `TapeDoc` — that copy is already
  counted in the timed region and the tape still wins stop-early 4.4×. Copy-out is the correct safe
  default; the detach hazard is a **counterexample to a zero-copy optimization** (shaving the 118 KB
  copy would produce silent corruption on a growing batch), not a live bug.
- **Q3 — value.js-4.0.0 divorce landed in-tree (XL-G P1 answered).** value.js src has **zero**
  `@mkbabb/parse-that` imports; the CSS grammar is hand-rolled recursive descent (0 combinators);
  `subpaths/{math,transform}.ts` assert `parse-that-FREE`. The confinement oracle evaporated — value.js
  now exercises ≈0 sync sites; the live confinement datum must re-home to **latex-paper** (still
  imports `Parser/regex/string/any` across `bibtex/parser.ts` + 6 `grammar/*.ts`) and keyframes.js
  (transitive). Flag F6/owner-bound. **Pricing:** one consolidated cdylib (one boot, one `Memory`,
  copy-out per plane) beats two cold instances; async instantiation is a one-time boot obligation
  (Chrome-114 removed the 4KB sync limit), amortized by the batch target.

**Residual open gaps.** (i) **The decision-relevant comparator (wasm-SIMD128 vs TS-native scalar) is
unrun a third time** — the bbnf-side viability decision (Brief 2). (ii) The crossing role is
PROVE-14-anchored, not re-measured. (iii) R-B memory-grow gate unbuilt (exercise the detach + quantify
copy-out < ¼ stop-early). (iv) R-E rests on the now-obsolete "value.js 59 sync sites" (moved to
latex-paper, F6-bound). (v) Homing collision unpriced (`wasm/Cargo.toml` already claims
`name="bbnf-wasm"`; proposed `crates/bbnf-wasm` absent).

**Cross-lock.** 6C mirrors a runtime (tape ABI), not grammar-derived names (LOCK-3's map). Under 6C
the regex exemption dissolves favorably (Rust DFA→wasm, no TS-`RegExp` mirror) unless a measured
V8-wins carve-out keeps small/cold patterns TS-native. **Interlock: 6C.wasm-scan ⇐ 9A.kernel-home;
9B is the cost-bearing fallback** (if LOCK-9 unifies the kernel home, 6C's dual-maintenance
re-attributes to LOCK-9). LIVE x86 `avx2.rs`/`avx512.rs` in `simd-scan` (constitution iv "x86 dead")
is a LOCK-9 surface, routed to gestalt. Freeze AFTER LOCK-1, LOCK-5, LOCK-7, LOCK-9.

**Disposition: BANKED** (crossing-mechanism folded into the 6B/6D tape stratum; bbnf side). Fate
firm; bbnf-side viability undecided a third pass. **Not BLOCKED-on-a-hard-primitive** — the missing
piece is an unrun *measurement* (v128 kernel exists), not an absent primitive; the pass-2 framing that
6C needed "wasm SIMD keeping the NEON win" is corrected by research-C (the Q1 kernel is the portable
arm, not NEON).

---

## 4. Family 6D — ORACLE-ISOMORPHISM / DIGEST (the mandatory FLOOR)

**Thesis.** The rust/ts split is held equivalent by a shared conformance corpus + a differential
value-digest ALONE — module tree, tape byte-layout, decoder skeleton, error wording free per
language. Correspondence is proven at runtime by the fold, not statically by names. Null hypothesis:
it wins by default, defeated only if measured drift/maintenance cost exceeds a cap's.

**Architecture — the canonical Merkle value-fold (research-D §1).** A fold from parse result to a
fixed-width digest, specified once, implemented idiomatically twice. FNV-1a core with a
domain-separation tag byte per kind; `Math.imul == wrapping_mul(u32)` per lane is the equivalence
that makes the two folds bit-exact. Array = sequential child fold (order IN contract); object =
XOR-accumulate of member sub-digests + count salt (**key order OUT of contract by construction**);
string = tag++length++decoded code-units; accept/reject a parallel boolean channel; error-class joins
only when the Q4 toggle is on. Merkle (any divergence flips the root) + a structured six-count coarse
channel (escape requires a *double* collision).

**D1 — the fold RAN cross-language (§2), the artifact that never ran.** Corpus scale: Rust std-only
parser and V8 arm fold `twitter.json` to **`fp 3744114272`** and every counter identically; hard-case
stress (`9007199254740993`, `1.0000000000000002`, `5e-324`, `1e400`→inf, `-0`, astral `"a😀b𝕏"`)
folds **RUST fp 1291459692 == NODE fp 1291459692**. Minimal-contract instance: `baseline == keyperm`
(object order proven out-of-contract *by run*), `1ulp` diverges distinct-but-cross-identical (the
divergence detector is itself language-faithful). Q2 injected-divergence table: **static 4/5,
fuzzer 5/5**, latency 1 input once covered (class-4 bool-collapse escapes forever on a bool-free
corpus — the empirical case for a coverage-guided grammar fuzzer over a curated corpus).

**D2 — the number lane FLIPPED to raw bits; UNADJUDICATED contradiction with pass-2.** research-D §1
+ `contract-6D.toml` fold **`f64::to_bits()` little-endian** (`number_fold = "f64 LE-byte mix"`) and
argue bits are the *superior* last-ULP guard — a shortest-decimal fold can mask an adjacent-double
error — *provided* the residue is canonicalized (`[canonicalize]`: `is_nan()`→sentinel `0x9E3779B1`,
`-0`→`+0`, both BEFORE the byte fold; `1e400`→inf proven needing no rule). The argument that valid
JSON's correctly-rounded parse (Eisel-Lemire ↔ V8 StringToDouble, both round-to-nearest-ties-even)
makes bits a pure function of the decimal is sound and *demonstrated*. **BUT this reverses pass-2's
registry-credited Q10 correction** ("fold spans/canonical-decimal, **NEVER** raw bits") — the third
representation flip across three passes (bit→span→bit), now with running evidence on the bits side.
The two are reconcilable in principle (bits-with-canonicalization ≈ the pass-2 concern addressed at
the residue) but the checked-in contract commits to bits while the pass-2 correction forbids them.
**This is a live internal contradiction in the mandatory floor's spec, owed to GESTALT** — a
spec-probe can settle it (fold the residue + the `1e21` exponent-threshold both ways, see which
diverges), but LOCK-6 synthesis marks it OPEN, not resolved-by-fiat.

**Q3 — marginal added-grammar cost (§4).** Bounded by the *alphabet*, not the grammar: the fold is
six node constructors keyed on the `Value` sum variant; for any grammar over that alphabet
(TOML/CSV/sheets) marginal fold LOC = **0** (re-point the fuzzer, a config line); a genuinely new
variant (CSS dimension, TOML datetime) costs exactly one node-hash constructor per side + one
canonicalization rule if it has a non-JSON residue. **6D's added-grammar cost is O(new variants); the
generator-mirror siblings' is O(new grammar)** — the measurable form of the null-hypothesis outcome.
End-to-end confirmation still owed: fold ≥1 non-JSON grammar's value tree (TOML datetime is the clean
test).

**Q4 — the in/out partition, as checked-in data (`contract-6D.toml`, authored this pass).** IN: the
folded value tree, accept/reject, the error-class enum (`UnexpectedChar|UnterminatedString|
TrailingData|DepthLimit|BadEscape|BadNumber`), number/string/member fold rules, the UTF-16
code-unit length measure. OUT: error message wording (idiom), error span byte-offset (layout),
physical member order, internal representation. **Error-class toggle ON for class equality, OFF for
message + span** — a named supersession relaxing the intra-Rust `css_scan_differential.rs` "same
offset + message" to error-class-only across languages. Canonicalization (spec-closed,
corpus-unreachable): NaN→sentinel, `-0`→`+0`, `1e400`→inf — closed by compile-time assertion + one
golden vector each, never a corpus test.

**Mechanism / seams.** No invention — consolidation. In-tree differential machinery is mature but
intra-Rust: `checkasm_parity.rs` (FFmpeg model, misalignment sweep, `BBNF_SIMD_INJECT_BUG`,
strict/non-strict split = the must/may partition), `css_scan_differential.rs` (clean-room scalar
reference), `bbnf-bench/src/parity.rs` (plane-equality — the *layout* differential 6D discards). The
pass-3 gap closed: the Rust-side fold now exists (research-D). Substrate change owed: **widen u32→u64**
(a u32 `fp` collides at ~77k inputs by the birthday bound; a fuzzer at 10⁶+ hits it) — spec text, not
yet a diff. Feeds GN-4.W5 four-way parity close.

**Gates.** (a) 100% digest agreement across Rust eager/stop-early/typed + TS `TapeDoc` + a
third-party reference — PROVE-14 parity as STANDING law; (b) the in/out partition as data (authored);
(c) three-way accept/reject partition so the gate is stable.

**Exact open questions.** (i) **Both verdict NUMBERS still not fully closed** — the pass measured two
proxies (added-grammar marginal ≈ 0 fold LOC; V8-idiomatic-optimization marginal = 0 gated surface),
neither of which *defeats* the null hypothesis but neither *closes* it: the debuggability
time-to-root-cause vs mirror-maintenance-cost comparison (R5, the axis where 6D is dominated — a bare
root-digest mismatch says "something differs," not "escape decoder off-by-one at line X") is the
single measurement the "reasonable point" verdict turns on, and it is unrun. (ii) **The bits-vs-spans
number-lane contradiction (D2)** — unadjudicated. (iii) The u64 widening is spec text, not a diff;
the fuzzer at 10⁶+ inputs (collision-free) unrun. (iv) LOCK-7's N+1 reference flagging a *seeded*
both-engines-wrong case (R2 — the oracle proves agreement, not correctness) unrun. (v) The
teeth (coverage-guided grammar fuzzer + delta-debug shrink) are the empirical case from §3 but the
generator itself is unbuilt; gate-(a) still exercises mostly the easy direction.

**Cross-lock.** A digest-only family needs no name map. Regex → behavioral conformance (same match
span/captures/accept-reject over a shared corpus), compatible with LOCK-5 ONE-HIR without an
allow-list freeze. **DAG (binding): freeze strictly AFTER LOCK-1 (alphabet = grammar-derived `Value`
ADT / `DirectTypeKind` sum) and LOCK-7 (tie-breaker = bbnf-in-bbnf reference, the N+1th version that
adjudicates both-wrong)** — a 2-version differential detects disagreement only. Recorded in
`contract-6D.toml [dag]`.

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
stratification; drawing that partition is the next pass's central deliverable, and it rests on two
numbers (6A's ratio, MEASURED; 6B's tape-face gen-vs-hand *with the TS emitter built*, Brief 1) plus
the load-bearing 6D-is-a-mandatory-floor claim (D1 advanced it; R5 closes it).

**Genuine pairwise incompatibilities (same surface):**

1. **6A ⊥ 6B — exclusive per item.** Generate-vs-hand-write-and-lint cannot both govern the same
   item: a 6B-generated pair has no independent hand twin for 6A's name-map to check — the linter
   would compare the generator's two outputs, a tautology (Y.13). They coexist ONLY by partitioning
   the surface, and pass-3 sharpens the line with measured facts: 6A's Class-1 (grammar-derived) is
   PROVENANCE-checked or 6B-generated (same source); 6A's genuine non-tautological load is **Class-2
   (the hand-authored taxonomy + the dual-authored `classify`) + the hand-copied grammar-derived
   constants** (`GRAMMAR_BIT0`, LINTED not generated — Face I partial, §2 (iii)); the schema + the
   N-grammar tape/classify faces are GENERATED (6B, Rust-side confirmed 100%). The partition needs
   6B's TS-emitter number (Brief 1) to fully draw. On the combinator **type** algebra 6B is REFUTED
   (ts-rs flatten) → 6A + 6D there; **but 6B-as-layout-gen (the LOCK-4C VM stream) is a live
   re-opening the type-gen refutation does not touch** (§2 Q4) — OPEN, owed to LOCK-4/LOCK-8.

2. **6C ⊥ 6A and 6C ⊥ 6B — on absorbed surfaces.** Where 6C absorbs a surface into the wasm core
   there is no second implementation to mirror (6A) or generate to (6B). But PROTO-1's R-A-negative
   means the absorbed surface does **not** include the hot scan kernel (it stays a dual
   implementation, `simd-scan/wasm.rs`) — 6C absorbs only the *crossing*, not the *scan*.

3. **6C ⊥ the parse-that consumer contract (family-vs-constraint, hard).** Synchronous ESM
   `all()`/`dispatch()` cannot await `WebAssembly.instantiate` (R-E) → confines 6C to the bbnf side.
   Softened at the boundary (one-time boot, not per-call async tax — Chrome-114) but the confinement
   stands: a synchronous combinator built at import time cannot touch wasm before boot resolves. The
   value.js-4.0.0 divorce (research-C Q3) *confirms* the confinement can be dissolved by divorce, and
   *evacuates* the confinement oracle from value.js to latex-paper.

4. **6D-pure ⊥ the tape plane (family-vs-measured-requirement).** "Layout free per language" negates
   the byte-identical SoA layout the zero-copy stop-early win (F2/F3) requires. 6D-as-whole-answer
   conflicts with the tape's measured requirement; 6D-as-floor does not. The null hypothesis is
   defeated *for the tape* and survives *as verification*.

5. **[NEW, internal to 6D] number-lane bits ⊥ spans (D2).** research-D + `contract-6D.toml` fold raw
   `f64::to_bits()` LE (+ canonicalized residue); pass-2's registry-credited Q10 correction forbids
   raw bits (fold spans / canonical-decimal). Both now have arguments; the bits side has running
   evidence. **Unadjudicated** — a spec-probe settles it, GESTALT ratifies. Not cross-family, but a
   real contradiction in the floor's frozen contract.

**Compatibilities (the stratification the incompatibilities resolve into):**

| surface | primary mechanism | verification |
|---|---|---|
| tape layout (4 SoA planes) | **6B** generates the byte contract, **6C** crosses it zero-copy (bbnf side) | 6D digest + T0 byte gate |
| decoder skeleton / classify / schema face (N-grammar) | **6B** (Rust side confirmed ~100% generated; TS emitter unbuilt — Brief 1) | 6D |
| fixed flag-bit values / ~15 layout constants | **linter** (6A/pairing-linter — generating them over-builds, Q3) | 6D |
| value-materialization / walk | **TS-native** (6C-tempered "where V8 wins"); skeleton optionally 6B | 6D |
| combinator **type** algebra | **hand-written** (6B-type-gen refuted); **6A** name-map | 6D behavioral |
| combinator **layout** (IF LOCK-4C VM lands) | **6B-as-layout-gen** (shared instruction stream) — **OPEN, owed to LOCK-4/8** | 6D behavioral |
| module tree / names / idiom residue | **6A** trichotomy (Class-2 name-map + Class-1 provenance + Class-3 budget); NOT code-shape | 6D |
| everything (behavior) | — | **6D** — the mandatory floor |

**Cross-pollination (named grafts).** (1) 6B ⊗ 6C — the generated tape byte-contract crossed
zero-copy (PRIMARY, the stratification's spine): 6B without a crossing is a spec with no consumer; 6C
without a generated contract is two hand-transcribed alphabets. Pass-3 caveat: the 6B leg's TS emitter
is still 0% built — the spine is a hypothesis until Brief 1. (2) 6A ⊗ 6D — 6A's Class-2/T3 corpus IS
6D's differential corpus: 6A is "6D plus a structural tax" by construction. The pass-3 PROVE-14 drive
is the genuine cross-language backstop the pass-2 proxy lacked. (3) 6B ⊗ 6D — the generated oracle
table: 6B generates the digest comparator into BOTH a Rust fixture and a TS test, so GN-4.W5's
self-stub (`get==get`) becomes unwriteable. Pass-3 caveat: the 6B oracle face is 0% built.

**BLOCKED-candidate summary.** 6C-as-primary: **BANKED** (refuted maximal; confined by R-E; hot-kernel
perf premise refuted by PROTO-1's measured R-A) — but *not* blocked on a missing primitive equal to
the original problem: research-C corrects the framing (the Q1 kernel is the existing portable `u8x16`
arm, not a "wasm SIMD that keeps NEON"), so 6C's remaining question is an unrun measurement (Brief 2),
not an absent primitive. 6D-as-whole-answer: **BLOCKED for the tape** (layout freedom negates byte
identity). 6A and 6B are **ADVANCED**, mutually exclusive per surface, each strictly dependent on the
6D floor, and both SPEC_ONLY on their central *product* (6A's linter unbuilt; 6B's TS emitter unbuilt
+ the 703→0 projected-not-measured). Neither 6A nor 6B is BLOCKED: 6A's cap is grep-derivable (D3),
6B's R2 collision is a spec-declared bit-partition (§2 (ii)).

---

## 6. The two sharpest uncertainties for the next pass

The plane's verdict rests on drawing the partition line (§5), and the RE-EMERGENT prototype-allocation
defect (both prior passes measured softer targets than the sharpest uncertainty) is the failure to
avoid. Pass-3 discharged the softest of the four pass-2 non-negotiables (6D's cross-language fold RAN;
the AoS↔SoA core dissolved; the 6A cap re-derived). The two hardest, most decision-relevant,
still-runnable uncertainties — the two that convert the largest remaining SPEC_ONLY crowns to BUILT
and draw the partition — are the briefs below.

- **Brief 1 (6B, `bbnf-worktree`) — build the TS `emit_classify` face and measure gen-vs-hand on the
  tape face on BOTH sides post-generation.** The crown product, 0% built through three passes; it
  draws the 6A⊥6B partition, closes the AoS↔SoA-expressibility item empirically (one spec, two
  dialects), and its `emit_*` machine is the same one the LOCK-4C VM fork would use — so building it
  de-risks the one live cross-lock item.
- **Brief 2 (6C, `pt-scratch`) — the wasm-SIMD128 vs TS-native-scalar comparator, cold-paired in a
  browser engine.** Skipped three consecutive passes; now trivially runnable (the `v128` kernel
  exists; the wired harness exists, needs cold-paired recast); it gates the tape-crossing stratum
  (the stratification's spine) and the bbnf-side wasm-core viability.

*(Full self-contained briefs returned in the structured output. Two items are surfaced as owed to the
gestalt rather than to a LOCK-6-solo brief: the LOCK-4C VM combinator-layout fork — owed jointly to
LOCK-4/LOCK-8, resolvable only by research-B §5's cross-lock two-opcode probe — and the 6D number-lane
bits-vs-spans contradiction (D2), settled by a spec-probe and ratified at GESTALT.)*
