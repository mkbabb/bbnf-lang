# LOCK-6 — THE ISOMORPHISM PLANE (rust/ts split) · pass-3 FAMILY-SPECS

*Synthesis (Fable-tier). Distilled from pass-3 research-{A,B,C,D,priorart} (re-run 18:57–19:06,
POST-dating the 16:27 REGISTRY), all prior pass artifacts, and the CROSS-LOCK ledger (XL-A…XL-H,
conveyed here as settled fact, not re-litigated). This file SUPERSEDES the stale 15:26 synthesis
the registry flagged (which called 6A "unbuilt" and 6C "unrun" — both FALSE at HEAD).*

**The lock is not four rival answers.** It is **one mandatory behavioral FLOOR (6D)** beneath **three
candidate structural CAPS (6A/6B/6C)** that conflict *only where their surfaces overlap*. The
deliverable is not a winner — it is the **partition line** (§5), drawn by two reasonable-point
NUMBERS now both grounded in running evidence: 6B's gen-vs-hand LOC on the tape-layout face, and
6A's relational `cap` on the hand-mirror face. Elegance earns nothing; a family stalled at a
missing primitive as hard as the original problem is marked BLOCKED-candidate (§7).

**Substrate law carried (XL-A, XL-H, M2).** skinny@HEAD `ce51c36fc` is the MEASUREMENT SUBSTRATE
(the only living implementation); greenfield (GN-1) is the tree-of-record-to-be; ports scope
organ-level. Two families do not yet share a tree: 6B/6C/6D ground on **skinny@HEAD**; 6A's live
pair grounds on **top-level `crates/{core,bbnf-path,bbnf-path-ts}`** (LEGACY, archives at GN-0;
`skinny/crates/bbnf-path` does not exist — verified). Every number below names its tree.

---

## §1 — Family 6A · MIRROR-TREE + isomorphism LINTER · ADVANCED · dependent-on-6D · crown BUILT+RAN

**Architecture.** Hand-maintained mirrored surfaces (the Constitution clause (i) *licenses* twins —
combinators, resolver taxonomies — that no schema-first tool may generate), gated by a linter that
verifies, at the **exported-symbol** level, that every authored spine name has an identity mirror or
a *legitimate relational exemption* on the other side, within a **derived** budget. Not file-for-file
(refuted: the parse-that pair collapses a dozen Rust `span_parser/` files into one `parser.ts`), and
not name-by-name allow-listed (defeated by suffix-gaming).

**Mechanism (the relational exemption budget — the load-bearing advance).** Spine cardinality
`T` = authored taxonomy names present in *both* an owned/serde form and a TS form (live pair
T=4: `OwnedPathSegment`, `TypedPath`, `PathError`, `PathErrorReason`). `I` = a **closed, enumerated**
registry of idiom classes, each a *relational rewrite rule* `r: T → name`, not a bare suffix (live
pair I=5: I1 serde-transport `*Payload` ×4, I2 native-target `_native` shim ×2, I3 language-native
`Error` subclass ×1, I4 constructor-syntax `path!`↔tag ×2, I5 marker ZST↔string ×4). A non-identity
name `b` on side B is legit iff `b = t` (identity mirror) **or** `b = r(t)` for some `r∈I, t∈T`;
`cap = |legit(B)|`, and **RED ⇔ ∃ b ∉ legit(B)**. Because rules require the pre-image `t∈T`,
`BogusPayload` (no canonical `Bogus`) and `FooView` (no rule) both RED — the suffix-gaming hole a flat
allowlist (semver-checks) or a pure generator (UniFFI/protobuf) never faces, because 6A must gate
Constitution-*forbidden* twins. Growth is lawful ONLY by (i) an identity mirror (not an exemption) or
(ii) a new idiom class entering the reviewed `I` registry — never by a new exemption at fixed `I`.

**Provenance seam (Q3 — owned cross-lock).** Two gates hide under "the linter": (1) the **provenance
manifest** (`name → {single-source | authored}`) is a fact only the IR tower knows — grammar-derived
names (`lex_path`, `PathSegment`, registry field `name`s) are isomorphic *by construction* and skip
the mirror check; **OWNER: LOCK-3**, emitted as a `cargo xtask regen` byproduct under clean-regen
discipline (tree-sitter `node-types.json` precedent). (2) The **name-map/exemption linter** reads
TS/serde idioms I1–I5 — knowledge that must NOT live in the backend-agnostic middle (backend-
agnostic-types law); **OWNER: LOCK-6**, consuming LOCK-3's manifest. Clean producer/consumer seam.

**What RAN (PROTO-6A-linter, legacy tree, base-pin `ce51c36fc` DESCENDS-OK).** A `syn`-2 binary
syn-extracts Rust decls + `tsc`-emitted `path.d.ts` names, computes `cap` and `exemptions`
*independently*, reproduces `13 = 4+2+1+2+4` **without fitting**, REDs `BogusPayload`/`FooView` **2/2**,
and drives `ts/path.ts` through a REAL `wasm-pack --target nodejs` + `serde-wasm-bindgen` + Node
crossing (AGREE=2 / DIVERGE=3). Fresh research-A additionally drove the *pure* TS renderer
`PathError.format` vs Rust `Display` over 13 inputs: **11/13 PASS, 2 FAIL** — the **alternatives-
escaping** divergence (Rust `{:?}`-escapes, TS raw-interpolates) — a **name-map-green / behavior-red**
rust⊥ts fork, latent under grammar-legal field names (identifiers never carry `"`/`\`), catchable only
by an *adversarial-corpus* oracle. This is a cleaner rust⊥ts datum than the confounded one below, but
it lives on the render seam 6A DECLARES out-of-contract and routes to the 6D companion oracle.

**Integration seams.** Spine: `crates/core/src/path/{ir,error}.rs`; Rust twin `bbnf-path-ts/src/schema.rs`;
TS `bbnf-path-ts/ts/path.ts`; single-source lexer `bbnf_regex::lex_path` (both frontends, cannot
drift); oracle `tests/isomorphic_path_error.rs`; the live provenance FORK `bbnf-path-ts/src/fixture.rs:53-90`
(hand-builds `StructRegistry::new()` with literal `"statuses"` shadowing the emitted sidecar — a name
that *should* be single-source has **two live origins at HEAD**; the LOCK-3 provenance pass REDs it day one).

**Gates it carries.** (a) `exemptions == cap` GREEN on lawful growth, RED on any unbudgeted name;
(b) the linter parses `ts/path.ts` declarations (not only the Rust twin — the TS union `PathErrorReason`
is gated by *nothing compiled* today); (c) consumes the LOCK-3 provenance manifest end-to-end;
(d) landed in `xtask`/CI.

**Exact open questions.** [1] **The running gate carries a fit-to-count clause** — `green = exemptions==cap && cap==13`
with `let expected=13usize` (`mirror-lint/src/main.rs:318,342`); on lawful growth both rise to 14,
`==cap` holds but `==13` REDs a legitimate change. **The `==13` literal is probe scaffolding that must
NOT survive** — production gate is `exemptions == cap` ALONE. [2] the DIVERGE=3 floor-necessity datum
is **CONFOUNDED (rust⊥rust)** — canonical `check_path_against_registry` leaves `Named(_)` intact
(`UnregisteredRule`) while the cdylib resolves eagerly (`UnknownField`); a Rust semantic fork surfaced
*through* wasm, not the Class-2 rust⊥ts drift the family advertises. [3] **BIFURCATED RUST REFERENCE
(ill-posed target, cross-lock):** the two Rust frontends are themselves not isomorphic — "Rust↔TS
isomorphism" is undefined until an owner/LOCK names WHICH Rust behavior is canonical (routed LOCK-1/3/7
+ owner; 6D cannot adjudicate — agreement≠correctness → LOCK-7 N+1). [4] legacy-only substrate (M2);
[5] I=5 class-closure asserted, not stressed (no sixth-idiom attempt run); [6] `makePathTag` swallows
the typed `PathError` and rethrows raw — the typed presentation surface is untested (an incidental live
bug the behavioral drive surfaced).

---

## §2 — Family 6B · SPEC-GENERATED-SKELETON · ADVANCED · crown BUILT at the classify FACE

**Architecture.** One machine-readable `LayoutSpec`, itself grammar-derived, generates BOTH language
skeletons of the *tape-layout face* — the Rust decoder view and the TS `TapeDoc` — over one
hand-authored four-plane substrate `Tape{source, offsets, flag_cursors, flag_values}` (233 LOC, paid
once for all grammars; marginal layout cost per grammar ≈ 0). Where generated, drift is impossible by
construction (a `regen∘regen` fixpoint + a byte-identity gate reduce it to a boolean). AoS↔SoA is
DISSOLVED (the tape is always SoA); the "hard core" was never a storage question but a
**discriminator-provenance axis** over one layout.

**Mechanism (grounded on shipping code, corrected of record).** The thesis is already TRUE and shipping
for every non-JSON grammar: `general_descent/{view.rs 431, emit.rs 452}` (883 LOC, present — verified)
*programmatically writes* the Sheets decoder — `KIND_TABLE`, the `rule_kind` accessor keyed on
`flags_at(cursor).bits()`, and `as_f64/as_bool/as_tag` **derived from `->` projections, never from
hard-coded rule names**. The JSON view, by contrast, is a **488-LOC hand TEMPLATE** spliced verbatim
(`runtime_generator.rs:122-130`) — "generated" there means *copied*, not emitted. The spec needs three
axes: `KindRecovery ∈ {FromSourceByte(classify), FromFlagTag(kind_table), FromSourceThenFlag(CSS-L4)}`,
`FlagPlaneDensity ∈ {Sparse(grammar_bits), DenseTag}`, `PayloadDecode[] ∈ {SpanAsF64, SpanAsBool,
LiteralTag, ArenaBytes}`. The entire Rust-side mechanism is: **add a `FromSourceByte` arm + a `Sparse`
mode to `general_descent`, and the 488-LOC JSON template folds in as ≈40 lines of `LayoutSpec` data.**

**Correction of record (M1 / F-NUMBERING).** `emit_classify`/`LayoutSpec` **do NOT exist in the skinny
tree** (verified). The machine survives only as a scratch probe (`pass-3/proto-1-code/emit_classify.rs`
+ `gen_{json,sheets}.{rs,ts}` + `mono_check.mjs`) with **NO base-pinned run-record** and its harness
`gen_tape.mjs` **MISSING** (verified) — the `%HaveSameMap` PASS is not reproducible from committed
artifacts. The FAMILY-SPECS-of-record claim that this "ran as PROTO-1" is unsubstantiated (`PROTO-1.md`
is the 6C run). The in-production KERNEL is `general_descent`; the named spec machine is UNBUILT.

**What is genuinely BUILT vs the drift surface.** On the general (Sheets) path the decoder skeleton is
genuinely emitted (per-grammar hand-LOC ≈ 0). The **TS mirror is 0% generated (364 hand LOC)** — the
sole real drift surface, proven byte-exact only by hand transliteration (PROVE-14, fp `986912547`). 6B's
crown is exactly to (a) fold JSON into the general emitter and (b) emit `tape.ts` from the same spec.

**Integration seams.** Substrate `runtime/src/tape/mod.rs:92-115` (NB: tree consolidated under
`crates/core/src/runtime/tape/mod.rs` — a seam-relocation precedes any landing); emitter
`codegen/src/general_descent/{view,emit}.rs`; JSON template `codegen/src/json_templates/view.rs` (488,
to delete); the `MaterializationClass` seat `passes/src/lib.rs:1127` (`MaterializationDescriptor{kind:
TapeKind,…}`, producer `derive_materialization_plan`, drivers `decision_csp.rs`/`backend_egraph.rs`);
TS target `ts/tape/src/tape.ts` + `bbnf-wasm` four-plane `TapeExport`.

**Ownership split (Q5, cross-lock).** LOCK-2 CHOOSES the `MaterializationClass` (SoA-derive vs AoS-store;
LazyView vs DirectSink; Sparse vs DenseTag — a cost decision, seat exists at `decision_csp`/
`backend_egraph`); **LOCK-3 OWNS** the `LayoutSpec` type + the `emit_*` tower (`general_descent`
generalized). Lifting emitter *selection* from a hard `render` branch into a data variable REMOVES a
codepath (no god module).

**Gates it carries.** (a) generated `json/view.rs` **byte-identical** to the committed template
(deletes 488 LOC); (b) `tape.ts` emitted from the same spec decodes the crossed planes to fp `986912547`
(full) / `3431570937` (partial); (c) **V8 monomorphism on the GENERATED decoder** under
`node --allow-natives-syntax` (`%HaveSameMap`, `%HasFastProperties`, `%GetOptimizationStatus` optimized)
— the R3 gate, informative ONLY on non-byte-identical output; (d) `regen∘regen` byte-identity, CI-wired.

**Exact open questions.** [1] **CLASSIFY FACE ONLY** — the probe generated the R3-critical monomorphic
*slice* (27 TS + 138 Rust classify LOC → 0); the **~598-LOC full decoder is ungenerated**, and the mono
gate PASSED on the **byte-identical (trivially shape-identical) direction** — the gate becomes real only
on the full generated decoder where output is NOT byte-identical to a known-good hand version (research-B
R4: a generated boxed-union `classify` or a V8-un-lowerable `switch` silently deopts and the 4.4×
evaporates). [2] **PayloadDecode axis unmodeled** — a spec carrying only discriminator recovery
under-generates the three rule-keyed decoders (R3). [3] R2 flag-plane collision (one `u8` cannot carry a
discriminator tag AND an escape bit — needs a spec-declared bit-partition, unbuilt). [4] not landed in
`xtask regen`; [5] Face-I fixed flag-bit VALUES (`GRAMMAR_BIT0`) are hand-authored policy → LINTED, not
generated (the §5 partition seam); [6] TS `emit_direct_sink` symmetric face owed (Q3's negative-cost
fused-sink win is Rust-only); [7] the Q4 **shared bytecode instruction stream** — the `general_descent`
`write!`-skeleton generalizes to `OpcodeDecode` with no new machinery, but GO/NO-GO is LOCK-4C (VM vs
closure) + LOCK-8C (interpreter under V8), both unmeasured (cross-lock, not a within-6B item).

---

## §3 — Family 6C · WASM-CORE · BANKED · reinforced by a RUNNING falsification

**Architecture (as banked).** NOT "the Rust core is the one implementation" — prior art settles the
delivery shape as **native-core / two-binding** (SWC/Biome: napi for Node ≈1.75–2.5× faster than the
same logic as wasm; wasm for browser only). A single-wasm-core surrenders ~43–60% on the Node consumer
surface (value.js/latex-paper/keyframes run under Node/vitest) AND strips the §0 NEON differentiator
(WASM SIMD128 is 128-bit-only and scalarizes non-`q` NEON). The surviving 6C contribution is the
**zero-copy tape crossing** — 6B generates the byte contract, 6C crosses it once as typed-array views,
6D digests both sides — confined to the bbnf side.

**What RAN (PROTO-6C-wasmscan, skinny@HEAD).** The `simd-scan/src/wasm.rs` (433 LOC) v128 kernel is
parity-exact on all 12 docs and beats TS-scalar **4.7–7.3× on `scanOnly`** — but `TextEncoder`
UTF-16→UTF-8 marshalling is **72–90% of `wasmFull`** and swamps the win below ~2 MB (a *content*
artifact, not a size law). **wasm-core-home is FALSIFIED for JS-string sources; TS-native-home holds.**
research-C's own Q1 prediction ("wasm wins skip-heavy, loses dense") is falsified by its own probe: the
axis is *marshalling*, not skip-heaviness — the kernel wins everywhere on `scanOnly`.

**The one surviving win (the §5 spine tension — UNMEASURED).** If bytes are **already resident in wasm
linear memory** (produced/owned in wasm, no per-call marshal), the `scanOnly` column governs and v128
wins **4.7–7.3× at every size**. The graft decision reduces to **"where do the bytes live?"** — JS-string
source ⇒ TS-native (MEASURED loss); wasm-resident bytes ⇒ wasm-core (PREDICTED win, unmeasured). This is
economically self-tensioned: resident bytes require the whole parse→tape pipeline in wasm, i.e.
6C-as-primary — which is refuted for the browser fetch/editor source. The lane is meaningful only for the
**batch/server face** (corpus resident in linear memory).

**Two priced correctness hazards.** (1) `memory.grow()` **detaches** the backing `ArrayBuffer` (even
`grow(0)`); parse #1's captured `TapeDoc` views silently detach when parse #2 grows — a false-parity PASS
on ASCII leaves and silent corruption elsewhere. Mitigations: **copy-out** (`.slice()` the four planes —
tape is 18.98% of source on twitter, ~119 KB, cheap vs a 631 KB parse → stop-early ≥2× survives) or
**pre-reserve** (feasible because the tape is offset-only). Re-fetching `.buffer` alone is
correctness-*hazard*, not a solution. (2) Consolidation: one `bbnf-wasm` cdylib (single instantiate,
single linear memory, shared `simd-scan` kernel = LOCK-9 one-kernel-library made physical) vs two cold
instances (double compile + double copy-in) — unpriced.

**Confinement collapse (XL-G P1 realized at HEAD).** value.js is now **4.0.0** and has **divorced
parse-that entirely** (zero imports; its own 9 sync CSS entries); keyframes@6.0.0 is value.js-only. The
R4-era "dozens of sync sites" census is **obsolete**; the ONLY remaining live parse-that consumer is
**latex-paper** (`^0.7.1`, 8 grammar files, zero scattered `.parse()` sites — one grammar-compile entry).
The async-wasm-vs-sync-`.parse()` objection has **largely evaporated**. FLAG (F6/owner-bound): this rests
on value.js@4.0.0 HEAD; an owner could re-marry — revalidate at gestalt.

**Gates it carries / open questions.** [1] measure the **wasm-resident-bytes lane** (the ONLY regime the
kernel win survives; re-draws the §5 spine); [2] exercise the grow-hazard two-parse case (confirmed by
spec, not by a running crash; ≥2× copy-out margin inherited, not re-measured under grow); [3] price the
homing collision (`wasm/Cargo.toml` already claims `name="bbnf-wasm"`; `crates/bbnf-wasm` absent); [4] LIVE
x86 `avx2.rs`/`avx512.rs` in `simd-scan` vs constitution-iv "x86 dead" → LOCK-9 (`6C.wasm-scan ⇐
9A.kernel-home`); [5] **XL-D tension (new, cross-lock):** the TS-native-home verdict rests on the PROVE-14
**runtime tape-decoder walk** — XL-D demotes *runtime combinator decoders* to oracles and crowns the
build-time emitter as the fast face. The tape-decoder-walk is a *value-navigation over a pre-built tape*,
NOT a combinator interpreter — so it is consistent with XL-D as the fast **value** face; but FAMILY-SPECS
must name the distinction before GESTALT consumes it.

---

## §4 — Family 6D · ORACLE-ISOMORPHISM · ADVANCED · the mandatory FLOOR

**Architecture.** Behavioral isomorphism only: a shared conformance corpus and a differential digest
decide Rust↔TS correspondence; layout is free per language. A **null hypothesis** — it must be *defeated*
by measured maintenance/drift cost, or it wins. It is the mandatory FLOOR beneath all three caps
(structural name/layout parity is **necessary-not-sufficient** for behavioral parity — proven live).

**What RAN (research-D, three JSON corpora, cold).** Two INDEPENDENT parsers (Rust hand-RD, ~110 LOC;
V8 native `JSON.parse`, ~40 LOC) fold ONE spec and compare as u64: FNV-1a-64, strings over **UTF-16 code
units**, numbers `f64→to_le_bytes(8)` with **NaN→`0x7ff8000000000000`** canonical, objects order-
independent XOR, arrays sequential. **Bit-identical on all three** (twitter `38fcfcbf…`, canada
`ac209fa4…`, citm `816a4170…`) — exercising astral strings (137,128 UTF-16 units), 111,126 f64 numbers
(Rust Eisel–Lemire ≡ V8 `StringToDouble`, both correctly-rounded ⟹ bit-identical bytes *by construction*),
and 10,937 nested objects. Two independent parsers agreeing bit-for-bit is the N-version property PROVE-14
lacked — it certifies **semantic materialization**, not layout.

**Detection economics (measured, the null-hypothesis datum).** Static single-corpus catch = **2/5**
(feature-poor: canada catches only `drop`+blanket-`ulp`, *silently passes a broken engine* on
escape/astral/bool) to **5/5** (feature-rich, un-guaranteeable). Coverage-guided grammar-fuzzer =
**5/5 in ≤16 inputs** (astral rarest at 5, `\n`-escape at 16, structural at 1) — because the generator
spans the alphabet. The fuzzer's whole value is *guaranteeing* every feature is emitted, plus the named
mantissa-boundary obligation for a genuine last-ULP bug (fuzzer-reachable only if the generator targets
the round-to-even tie).

**Marginal cost per added grammar = O(new value variants), not O(rules).** For any grammar in the
6-variant JSON alphabet (TOML, CSV, Sheets, NDJSON) the fold cost is **0 LOC**; a genuinely new variant
costs `~3 LOC × 2` (one node-hash constructor + one tag byte per side) — the transport never changes.
Contrast the generator siblings whose schema face scales ~1 line/rule (json 9 / sheets 37 / css_l4 184).
That gap **is** the null-hypothesis defeat/win datum: 6D amortizes across grammars where 6A/6B re-pay.
Prior art: **toml-test** is a ready-made, adoptable differential corpus for GN-5.W2's new TOML grammar —
6D scales per-grammar at *near-zero* marginal, sometimes *already-written-by-someone-else*.

**The contract, checked-in data (`contract-6D.toml` / research-D `isomorphism-contract.yaml`).**
IN-CONTRACT (RED on mismatch): folded value digest; structural counts (independent coarse channel — a
divergence must collide in *both* to escape); accept/reject verdict; **error CLASS** (grammar-derived
enum, NOT message, NOT span). OUT-OF-CONTRACT (free): message wording, error span byte-offset, tape
layout, allocation/shape. MAY-EITHER (recorded): duplicate-key order, number canonical text-form. This
is a **named supersession** of `css_scan_differential.rs`'s "same offset + message" — correct *within one
language*, must relax to error-class-only *across* languages.

**Integration seams.** Fold generalized from `runtime/tape/mod.rs` four public planes + PROVE-14
`ts-tape/tape.mjs:34-68`; the differential harness pattern in-tree at `bbnf-simd/tests/checkasm_parity.rs`
(FFmpeg checkasm verbatim — dual run, misalignment sweep, `BBNF_SIMD_INJECT_BUG` sensitivity knob,
`BBNF_SIMD_STRICT` = the must-match/may-either split); alphabet home `direct_schema.rs:52` (`DirectTypeKind`)
+ `grammars/json/value.rs:12` (`JsonNodeKind`).

**DAG constraint carried.** The fold alphabet **IS** LOCK-1's grammar-derived Value ADT (a new sum variant
is a new fold arm) → freeze strictly AFTER LOCK-1. The tie-breaker for a both-engines-wrong case **IS**
LOCK-7's meta-circular oracle (the N+1-th version, JEST model) → freeze strictly AFTER LOCK-7.
`freeze_after: [LOCK-1, LOCK-7]`.

**Gates it carries / exact open questions.** [1] **LIVE in-file contract contradiction (the number lane):**
`contract-6D.toml` commits `number_fold = "f64 LE-byte mix"` (**bits**); pass-2's registry-credited
correction forbade raw bits; research-D fresh REHABILITATES bits *with two canonicalization rules*
(NaN→sentinel, signed-zero=distinguish) and shows the bits fold is bit-identical over 111,126 numbers AND
*avoids* the decimal-text-form divergences (`≥1e21` threshold, subnormal expansion) a text fold would incur —
because both sides parse to the same f64. Three positions have floated in three passes (bit→span→bit); the
"frozen" file picks bits. **It cannot freeze until a spec-probe folds the residue + the `1e21` threshold
BOTH ways** (routed to GESTALT for ratification). [2] **R5 debuggability UNRUN — the verdict-deciding
number:** the ONE axis where 6D is dominated (time-to-root-cause under a bare digest mismatch — "something
differs" — vs mirror-maintenance cost) is unmeasured; the "reasonable point" verdict the whole plane turns
on is asserted only on the easy (affirmative) direction. [3] fuzzer teeth REASONED, not RUN (§0 "5/5" is a
fixed injected table; the coverage-guided generator is unbuilt). [4] `digest="u64"` is spec text, not a
diff — the fold ran at u32 (birthday collision ~77k inputs; at fuzzer scale gate-(a) silently passes on
divergent input). [5] agreement≠correctness hole now CONCRETE (6A's bifurcated Rust reference is exactly
the both-engines-disagree case 6D cannot adjudicate; LOCK-7 N+1 unrun). [6] the K=0 fleet-economy claim
rests on folding ≥1 non-JSON grammar's value tree — no TOML/CSV grammar exists in-tree, so it is *reasoned*,
not measured.

**BLOCKED note.** *pure-6D-as-whole-answer is BLOCKED for the tape:* "layout free per language" negates the
byte-identical SoA layout the zero-copy stop-early win requires. 6D wins as verification **substrate**,
loses as the whole answer for the tape stratum. 6D-as-FLOOR is not blocked — it is mandatory.

---

## §5 — THE STRATIFICATION (the plane's central artifact — the partition line)

The families are **complementary by surface**, incompatible only at overlaps. The partition line, drawn:

| Plane / surface | Governing family | Isomorphic? | Evidence (tree) |
|---|---|---|---|
| Tape/value **byte-layout ABI** | **6B** (generate) | YES, byte-exact | PROVE-14 fp `986912547`; `general_descent` emits Sheets skeleton (skinny) |
| **Decoder skeleton** (classify/`rule_kind`/typed accessors) | **6B** (generate) | YES on general path; JSON still a template | classify FACE byte-identical (probe); ~598-LOC full decoder ungenerated |
| **Data-type / diagnostic payload skeleton** | **6B** (generate) | YES | wasm-bindgen `--typescript`/tsify; `bbnf-path-ts` `TypedPath`/`*Payload` (legacy) |
| **Hand-authored taxonomy + provenance + idiom residue + fixed flag-bit VALUES** | **6A** (lint) | mirror + linter | PROTO-6A `cap==13` (legacy); Face-I values LINTED-not-generated |
| **Behavior** (values, accept/reject, error-CLASS) | **6D** (floor) | YES | fold bit-identical 3 corpora (skinny); `isomorphic_path_error.rs` exhaustive-variant gate |
| **Resolution POLICY / logic** | none — 6D pins the agreed surface | NO by default | `Named` eager-vs-intact; `PathError.format` escaping 11/13 |
| **Value-tape crossing** (large payload) | **6C** (zero-copy views) | offset/handle only | PROVE-14; PROTO-6C — pays ONLY for wasm-resident bytes |
| **Diagnostic crossing** (small payload) | full serde-wasm-bindgen marshal | YES, idiomatic | `bbnf-path-ts` payload round-trip — the payload-size-bimodal law |
| **Hot-path kernels** (NEON vs V8-monomorphic) | none — per-language idiom | NO, measured | wasm 0.34–0.47× native NEON; SIMD128 scalarizes non-`q` |
| **Regex engine** | LOCK-5 functor (XL-C) | NO (two domain-fit types) | Rust bespoke HIR vs TS native `RegExp` |
| **TS type-INFERENCE plane** (`ExtractValue`, typed combinator algebra) | LOCK-8B, hand-authored | NO | ts-rs conditional-type limit — not derivable from Rust |
| **Delivery** | native-core / two-binding | split | napi (Node, keeps NEON) + wasm (browser, scalar tax) |

**The clean partition:** **6B generates the declarative half** (schema + classify + tape byte-contract +
data-type skeleton); **6A lints the hand-authored half** (Class-2 taxonomy + Class-1 provenance + Class-3
idiom residue + the ~15 fixed flag-bit VALUES); **6D floors both** (behavior, necessary-not-sufficient);
**6C crosses the value-plane zero-copy** (bbnf-side, resident-bytes only). Both anchor numbers now have
running evidence (6B classify-face byte-identity; 6A `cap==13`), but **the spine (6B⊗6C) is
measured-TENSE** and one anchor (6A's) is on the doomed legacy tree.

---

## §6 — INCOMPATIBILITY MAP (which routes cannot coexist, and precisely why)

**INC-1 · 6C-as-primary ⊥ a cheap 6B⊗6C zero-copy crossing (the sharpest, measured).** The §5 spine
"6B generates the byte contract, 6C crosses it zero-copy" is economically self-tensioned: the crossing of
a **JS-string source** is a NET LOSS below ~2 MB (marshalling 72–90% of `wasmFull`), and bbnf browser
source arrives as a JS string. The crossing pays ONLY when bytes are already resident in wasm — which
requires the whole parse→tape pipeline in wasm = **6C-as-primary, REFUTED** (0.34–0.47× native NEON;
full-walk 0.9×). Cheap crossing ⇒ 6C-primary ⇒ refuted. The two cannot both hold; resolved by scoping the
resident-bytes lane to the **batch/server face** only (browser/editor stays TS-native).

**INC-2 · pure-6D "layout free per language" ⊥ the byte-identical SoA tape.** 6D declares layout free; the
4.4× stop-early crossing requires a byte-identical offset-tape both sides. A whole-answer 6D would forbid
the very layout that produces the win it is meant to protect. Resolved: 6D is the FLOOR, never the tape
cap; pure-6D-as-whole-answer is BLOCKED for the tape (§7).

**INC-3 · 6A hand-mirror ⊥ 6B generation at the SAME surface.** Where 6B CAN generate a name/skeleton, a
6A hand-twin + linter is redundant and constitution-forbidden (drift-impossible-by-construction beats
lint-after-the-fact). Where the constitution FORBIDS generation (combinators; independently-authored
resolver taxonomies), 6B cannot reach and 6A is the only option. They cannot BOTH govern one surface — the
partition line (§5) is exactly the boundary. Not a contradiction to resolve; a boundary to *draw*, and it
now is: the LOCK-3 provenance manifest is the arbiter (single-source → 6B/skip; authored → 6A/lint).

**INC-4 · "wasm is the one implementation" ⊥ native-core / two-binding delivery.** A single-wasm-core
surrenders ~43–60% on the Node consumer surface (SWC's measured napi reason) and strips the §0 NEON
differentiator. "The Rust core compiled to wasm is the one implementation" (the literal 6C charter) and
"keep the napi Node fast path + wasm browser only" cannot both be the delivery. Resolved toward
native-core / two-binding (prior art unanimous); 6C's literal maximal form is retired, its crossing banked.

**INC-5 · shared hot-kernel strategy ⊥ the V8/LLVM law split.** WASM SIMD128 (128-bit, scalarizes non-`q`
NEON) cannot carry the native NEON differentiator across the boundary; V8-monomorphic hand shapes and NEON
kernels are different arts. No shared hot-path strategy is possible; per-language idiom is forced (measured,
not asserted). This is a *permanent* non-mirror, not an open question.

**INC-6 · the bifurcated Rust reference makes the 6A/6D TARGET ill-posed (not a family conflict — a
substrate red).** The two Rust frontends (offline checker vs eager resolver) are not isomorphic to *each
other*, so "Rust↔TS isomorphism" has no defined target until an owner/LOCK names the canonical Rust
behavior. Neither 6A (can't see it) nor 6D (agreement≠correctness) can adjudicate. Routed LOCK-1/3/7 + owner.

**Non-conflict recorded:** 6D's number-lane bits⊥decimal is a within-6D in-file contradiction, NOT a
cross-family incompatibility (settled by a GESTALT spec-probe; research-D fresh leans bits-with-canon).

---

## §7 — BLOCKED-CANDIDATE LEDGER

- **6C-as-independent-primary → BLOCKED (settled, banked).** The surviving kernel win requires
  wasm-resident bytes, which requires the whole parse→tape pipeline in wasm — a missing primitive (a source
  that originates in wasm, absent for the browser fetch/editor case) **as hard as the original problem**. 6C
  is BANKED (its zero-copy crossing folds into the 6B/6D tape stratum, bbnf-side); the resident-bytes lane
  is the one unmeasured sub-question of a settled verdict.
- **pure-6D-as-whole-answer-for-the-tape → BLOCKED (INC-2).** Structurally cannot deliver the byte-
  identical layout the stop-early win requires. 6D-as-FLOOR is NOT blocked — mandatory.
- **6A, 6B, 6D (surviving forms) → NOT BLOCKED.** Each has its mechanism built or shipping and its exact
  remaining gaps enumerated; none stalls at a missing primitive. 6A is held near floor by defects (fit-to-
  count clause, confounded datum, legacy substrate), not by a wall. 6B's crown is a FACE awaiting the
  full-decoder scale (a bounded extension of shipping code, not a missing primitive).

---

## §8 — CROSS-LOCK DAG CARRIED (for GESTALT; do not re-litigate inside the lock)

1. **BIFURCATED RUST REFERENCE (new, INC-6)** → LOCK-1/3/7 + owner: WHICH Rust behavior is canonical.
2. **tape-decoder-walk: fast-face or oracle? (new, XL-D tension)** → name the value-navigation-vs-combinator-
   interpreter distinction before GESTALT consumes the lock. The tape walk is a value face (compatible with
   XL-D's build-time-emitter-is-the-fast-*parser*-face verdict).
3. **LOCK-4C / LOCK-8C — 6B-VM-stream vs hand-code (carried, strengthened).** `general_descent` generalizes
   to `OpcodeDecode` with no new machinery; GO/NO-GO = VM-vs-closure (4C) + interpreter-under-V8 (8C), both
   unmeasured. GESTALT-P collides here first; adjudicate jointly.
4. **LOCK-3 / LOCK-2 — `LayoutSpec` + `emit_*` + `MaterializationClass` (carried).** LOCK-3 owns the emit
   spine + provenance manifest; LOCK-2 owns the class CHOICE (a cost decision). Assign before either gestalt.
5. **6D number-lane bits ⊥ decimal (carried)** → GESTALT spec-probe (fold residue + `1e21` both ways).
6. **CLOSED, carried:** LOCK-9 kernel double-home + LIVE x86 in-tree (`6C.wasm-scan ⇐ 9A.kernel-home`);
   LOCK-5 Rust-DFA↔TS-`RegExp` behavioral routing (compatible with XL-C two-domain-fit); every family
   freezes strictly AFTER LOCK-1 (alphabet) + LOCK-7 (tie-breaker).

---

## §9 — THE TWO SHARPEST PASS-4 PROTOTYPE BRIEFS

*Chosen to resolve the §5 SPINE — the plane's central artifact and its weakest-evidenced row.*

### Brief 1 — 6C · the wasm-resident-bytes lane (re-draw the §5 spine) · kind: bbnf-worktree

**Question.** Does the v128 `scanOnly` kernel win survive when the source is already resident in wasm
linear memory (no per-call `TextEncoder` marshal), at bbnf batch/server doc sizes — the ONLY regime the
6B⊗6C zero-copy crossing pays?

**Method.** Worktree base-pin `ce51c36fc` (XL-H). Build `bbnf-wasm` cdylib exposing (a) `scan_resident(len)`
over bytes ALREADY written into a pre-reserved linear-memory arena (no marshal in the timed region), and
(b) `scan_marshalled(Uint8Array)` (TextEncoder+copy timed — the PROTO-6C lane). TS-native scalar `scan.ts`
is the third arm. Corpus `{twitter.json 631KB}` ∪ a batch of N∈{1,8,64} KB-scale CSS/JSON docs. Cold,
paired (§0/09 — two-arm window or it is disinformation).

**Commands.** `cargo build --target wasm32-unknown-unknown -p bbnf-wasm --release`; `node --expose-gc
harness.mjs` over doc-size × batch-count, reporting ms/MB per arm + the crossover.

**Pass/fail.** GREEN if `scan_resident` beats `scan.ts` **≥2×** at KB-scale batch sizes (kernel win
survives resident ⇒ the tape stratum CAN home in wasm for the batch/server face; spine re-drawn as
bimodal). RED if `scan_resident` fails 2× at realistic bbnf sizes (win is large-doc-only; the crossing
never pays for bbnf ⇒ 6C's banked crossing is browser-dead, batch-only). Either verdict re-draws the spine.

### Brief 2 — 6B · full-decoder V8-monomorphism gate on NON-byte-identical generated TS · kind: bbnf-worktree

**Question.** Does a `LayoutSpec`-GENERATED full `TapeDoc` decoder preserve V8 monomorphism, or does
generation emit a boxed-union `classify` / un-lowerable `switch` / post-ctor field assignment that
silently deopts and evaporates the 4.4× stop-early (R3/R4)? The classify-face gate PASSED only on the
byte-identical (trivially shape-identical) direction — uninformative.

**Method.** Worktree base-pin `ce51c36fc`. Reconstruct the missing `gen_tape.mjs` (M1) and extend
`emit_classify.rs` (`pass-3/proto-1-code/`) to emit the FULL 364-LOC `tape.ts` decoder
(walk/leaf/nav/pointer/load) from a `LayoutSpec`, deliberately NOT transcribing the hand source —
generate freshly. Load the generated decoder under `node --allow-natives-syntax`; drive twitter.json's
crossed four planes.

**Commands.** `rustc -O emit_classify.rs -o emit && ./emit > tape.gen.ts`; `node --allow-natives-syntax
mono_check.mjs` asserting on the hot `classify`/`nextSibling`/`get` path: `%HaveSameMap(a,b)` across
instances, `%HasFastProperties`, `%GetOptimizationStatus` optimized-not-deopted; plus fp parity.

**Pass/fail.** GREEN if the generated decoder is monomorphic on all hot sites AND folds to fp `986912547`
(full) / `3431570937` (partial) — 6B's crown extends from the classify FACE to the load-bearing decoder,
generation is drift-free AND V8-safe, TS drift surface closes (364→0). RED if any hot site megamorphizes
or deopts, or the fp diverges — generation costs more than the drift it prevents on the decoder face, and
6B collapses toward a linter-only null hypothesis there (the R4 falsifier fires).
