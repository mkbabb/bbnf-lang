# SK-V20 SPEC — The Totality Re-Founding (cutover + the TW wave manifest)

Date: 2026-06-10. HEAD: `36ca10646`. Status: CERTIFIED by the 32-agent research pass
(`GESTALT-SYNTHESIS.md` is the evidentiary base; `DEFERRED-LEDGER.md` and `PRECEPTS-RECAP.md` are
normative companions). This SPEC supersedes the SK-V19 SPEC's wave manifest; each SK-V19 wave's
disposition is recorded in §2. This document is the executable plan for the totality implementation.

## §0 — The charter (binding, restated once)

A fully generalized parser generator that bests every SOTA parser for its targeted structured-data
languages, by a perfected union of: (1) a backend-agnostic core — zero grammar-specific and zero
backend-specific code in generic crates; (2) a specializing emitter in the dav1d lineage — reference
Rust + cost-model-selected grammar-agnostic aarch64 kernels + generated checkasm parity/microbench
harness bound to liveness; (3) full type resolution — HM core over grammar rules, CSP for
layout/strategy, egraph for typed rewrite + cost extraction, every `->` reaching the emitter; (4) a
first-class Value API — lazy + eager over ONE tape, one grammar-derived path alphabet,
direct-to-struct, with every comparison-asymmetry disclosed at the claim site. NO workarounds, NO
stubs, NO legacy code; aarch64/Apple-M only; cold benchmarks only; clean regen only; ONE codegen path.

## §1 — Cutover (normative)

- **Cutover commit 0** (doc-only): the D-G pre-block reconcile (REDRESS 51/53/246/247) onto the
  committed ledger. On landing: tag skinny `sk-v19-frozen`; the skinny tree becomes
  architecture-read-only — retained solely as differential oracle + checkasm/microbench harness;
  archived under `restart/skinny/archive/` after the 9-fleet fold completes. No new architecture is
  ever born in skinny again.
- **The cutover gate** is `xtask cutover-gate` (a runtime artifact): exit 0 iff the Minimum Shippable
  Totality Slice is green — JSON + Sheets end-to-end on the SoA tape in `crates/core`;
  field-for-field independent oracles (sonic-rs/serde_json for JSON; the typed Sheets walk); cold
  per-parse same-run bench at host_loadavg < 1.0 with no regression vs the pre-fold core baseline;
  Locks 1, 10, 14 (widened) at zero. This gate is the TW2/TW4 close condition.
- **Process surgery** (operating discipline from this tranche forward): one challenge wave,
  advance-on-zero-REJECT (§3Z retired); astral ΩA–ΩF retired (SIGNOFF + CRUD-LOG remain); no planning
  cycle opens while the chronic ledger is non-empty; every wave closes on a green runtime gate in
  `crates/` — never on a document; non-implementation passes are capped at three per cycle.

## §2 — Disposition of the SK-V19 W0–W6 manifest

| SK-V19 | Disposition | Where |
|---|---|---|
| W0 | AMEND — re-anchor the classification gate on `ManifestStrategyEntry` (the strategy.rs:134-189 refs were deleted by W6a) | TW1 entry |
| W1 | AMEND — corrected counts: 3 structural builder families (OpenFrame ×3 / Frame / SimpleStructBuilder×5-cohort); delete surface ≈ 1,080 hits | TW1 |
| W2 | ABSORB verbatim — port skinny `tape/` wholesale; FUSED with W1 in one fenced landing | TW1 |
| W3 | AMEND+EXTEND — adds the rich keyed/recursive navigation derivation (the json_templates un-courier rule) | TW2+TW3 |
| W4 | ABSORB — subsumed into TW3 (the projection fence rides the value plane) | TW3 |
| W5 | SUPERSEDE the cost-model leg — the skinny CSP/e-graph theatre is retired, NOT ported; net-new measured KernelCostTable + unification with core's csp_strategy engine | TW4 |
| W6a/b/c/e | DONE (`f4633d468`, `36ca10646`) — but landed over a stub; the gate must RE-PROVE green after TW2/TW4 under the widened scan | TW0 widens; TW4 re-proves |
| W6d | StructLayout→Layout rename (960 hits / 75 files) | TW1 tail |

## §3 — The TW wave manifest

**TW0 — Gates, hygiene, honesty (no architecture; every item is a deletion, a two-line fix, a
disclosure, or a gate).**
Cutover commit 0 (D-G). Lock-15 fix (`lto = "fat"`, `panic = "abort"`). Lock-4 fix (egraph→csp via the
passes bridge). Lock-12 archive decision (ser/gorgeous). W5d deletion: avx2/avx512/wasm out of
`crates/simd-scan` + the unconditional AVX2 bodies out of `emit_support_module` (aarch64-only,
everywhere). Inline-test relocation (7 core + 2 skinny). MASTER-PLAN receiver-row reconcile + stale-§13.1
x86 table deletion. Bench honesty: JSON FRAMING.md (plane separation); thermal-mixing qualifiers
co-located at MASTER-PLAN/BENCH claim sites; H1-LEDGER "equal-depth" sentence corrected; H1 quiet
re-capture (loadavg < 1.0, N=200, both sides); full-tree-touch bench plane added (the lazy-claim honesty
bound). Oracle hygiene: dead `color_channel_parity_rgb_family` delete-or-activate; named-color rekey to
the W3C table; `eprintln!` soft-fails → hard asserts. Lock-14 widening: roster derived from
`[workspace.metadata.bbnf-strategy]`; scan ALL generic surfaces (ir, analysis, passes-equivalents, the
codegen crate, xtask); per-surface re-inject falsifiers; the skinny 5,283-line firewall is retired at
freeze (the live structural gate is the only gate pattern going forward). `generated_loc_budget`
enforced in `regen --check`. LOCKS.md scaffolding-prose removal.
*Close gate:* all listed locks/tests green; the disclosure docs committed; the quiet bench session
captured and ledgered.

**TW1 — The substrate landing (fused W0+W1+W2+W6d).**
Port skinny `crates/runtime/src/tape/` (SoA, offset+flag planes) into `crates/core` as the ONE tape.
Rewrite the cursor against SoA columns (never adapt the AoS cursor). Delete, in the SAME fenced commit,
gated on parity: AoS `TapeRec`/`TapeStructBuilder`, the three eager builder families for the slice
grammars (then fleet-wide), `tests/tape_substrate.rs`. StructLayout→Layout rename lands at the tail.
Topology moves that the substrate landing forces: retire the empty skinny `simd-scan` husk; begin the
`core` split (frontend / codegen / regex / runtime) along the seams the landing exposes.
*Close gate:* 9-grammar regen byte-clean; JSON+Sheets oracles field-for-field green; Lock 1
(exactly-one-encoding) green; no bench regression vs pre-fold baseline (cold, same-run, quiet).

**TW2 — The one generator (the C fold at fleet scale).**
Promote skinny `BackendExpr` to THE backend-agnostic IR; introduce the real `Emitter` trait keyed 1:1 on
its variants (no-op defaults deleted — every emitter implements every method); convert
runtime_generator's String emission into the RustEmitter. Fold CSS into BackendExpr lowering (delete the
css_scan raw-AST walk — the third codegen path). Collapse the xtask fork: ONE grammar-driven regen entry
(regen_css.rs + regen_simple_runtime.rs's four RuntimeStyle arms dissolve into TOML-driven projection
data; Cmd census derived from `xtask/runtime-projections/`). Complete `general_descent`: TapeEmit/
SpanMark/DirectBuild/RegexProgram-kind/AltMode::Dispatch all live (no silent no-ops). Un-courier
json_templates: derive value/view/parser emission from recognizer shapes (the one remaining cheap skinny
proof, executed against the frozen tree as differential oracle, landed in core). Structural-predicate
dispatch replaces the `has_rule("value")` name gate; literal tables derive from ValueProject metadata;
recognizer alphabets derive from the grammar (the JSON-alphabet filter deleted). Regex: one home — the
canonical parse-that engine via path patch; the skinny name-collision crate and the ad-hoc FIRST scanner
deleted; HIR-structural classification replaces pattern-string matching. Emitter regression: trace the
css_l4.rs 108k-line growth; PATH_PLAN literal arrays → packed wire encoding; budgets enforced.
*Close gate:* ONE emit path serves all 9 grammars (the three emit bodies gone); `xtask cutover-gate`
green for the slice; generated budgets met; widened Lock-14 green over the now-scanned codegen surface.

**TW3 — The value plane (lazy + eager + path + direct-to-struct, grammar-derived).**
ValueRef/Cursor over the ONE tape, fleet-wide; eager materialization as a generic fold over the lazy
cursor (the per-grammar value.rs hand-written accessors retire). The view emitter derives container
navigation from grammar structure (keyed groups → get/pairs; repetition → values/index; alternation →
variant). ONE path/query API with the grammar-derived segment alphabet (Field/Index/Variant + CSS
combinator segments + Sheets range segments); the four JSON-pointer-shaped implementations collapse;
bbnf-path reads the registry, not a JSON sidecar. Direct-to-struct: serde `Deserialize` bridge over the
lazy cursor (the dead ser::Deserializer and its hardcoded JSON delimiter set deleted). The CSS depth
closure: value-component tape events (FunctionOpen/Close, ComponentBoundary, typed ValueLeaf) emitted at
boundaries the balanced scanner already finds; leaf typing via the proven `classify_projection`
machinery; the named-color projection lowered to a derived perfect-hash; the parity oracle flipped to
the full token sink it already computes. The StructRegistry/FieldSource compile-time projection fence
(absorbed W4).
*Close gate:* every `->` in all 9 grammars reaches the emitter (`typed_materialization_invariant` flips
to built); JSON value_from_ref byte-equal; the full-depth CSS oracle green; the >SOTA ratio re-stated
honestly on summary AND full-tree-touch planes.

**TW4 — The decision engine + the kernel plane (the dav1d chain of consequence).**
One engine: skinny's BackendShape (the 5-shape canon) unified with core's csp_strategy into ONE
extraction over (BackendShape × KernelStrategy); the tautological CSP and the cosmetic e-graph rewrite
are retired, not ported. The cost model is DATA: a checked-in, versioned KernelCostTable —
(KernelId, Uarch) → measured cycles/64B — populated by the checkasm/PMU harness; perf_cost is the lead
axis; every hardcoded threshold (8/16 kernel bounds, alt_branch≥8, tiny_string_cap) moves into it.
Kernel unification: ONE structural classification scheme (lo/hi nibble two-LUT); ONE movemask
(`vshrn_n_u16::<4>`; the Lock-16 single-pack co-gate lands); the CSS scan classifies each 64-byte block
once with carried bracket/quote state; branchless bit-drain via a class-lane action table. The SOTA-gap
leaves: grammar-emittable Eisel-Lemire number decode (retires the serde_json number path); NEON bulk
string unescape keyed on HasEsc (the existing uXXXX quartet kernel wired); tokenize-once CSS declaration
bodies. Liveness discipline: checkasm generated per-kernel from the KernelPlan, asserting every planned
kernel is REACHED on real corpus bytes, strict by default; every dead kernel (JSON neon::scan behind
GrowOnly, unescape_uxxxx, the two dead asm! blocks, the scalar passthrough stub in the NEON dispatch
slot, the lying ScanBackend::Scalar label) is wired-or-deleted. The asm line: intrinsics by default;
`asm!`/`global_asm!` admitted only for a cargo-asm-proven mis-schedule, a missing intrinsic, or
cross-chunk spill control — each with its proof attached. The Lock-16 manifest: extractor-emitted,
byte-reproducible, one row per selected leaf.
*Close gate:* `xtask cutover-gate` green (this is the gate's second leg); the widened Lock-14 RE-PROVEN
over the full system (the W6 green stops being green-over-a-stub); the compound >SOTA sentence becomes
true unqualified on the gated planes — cold, same-run, quiet, with live SIMD on the JSON path.

**TW5 — Type resolution (honesty first, then the core).**
W0 honesty: strike "rank-1 HM / AC-3 / bidirectional" from README/ARCHITECTURE/HANDOFF; the existing
pass is renamed structural type projection. Then the build: MapExpr synthesis (bottom-up over the scalar
operator set; annotations CHECK, never replace); HostSigTable from @host declarations with real
signature unification (BBNF-HOST-SIGNATURE-MISMATCH can finally fire); `TypeDesc::Sum` (typed tagged
unions — HeterogeneousAltJoin-as-diagnostic retires) and `TypeDesc::Record(Row)` for struct projection;
then the genuine HM core — type variables, unification, principal schemes per rule — over the grammar
function surface. CSP remains the layout/strategy solver; egraph remains typed-rewrite + cost
extraction; the type lattice re-homes off the csp-solver. The CSS-named prefix-stripping escape hatch
(absoluteLengthUnit/namedColor) deletes with the Sum landing.
*Close gate:* the 9 fleet grammars type end-to-end with zero `unwrap_or(Span)` defaults; the host
signature gate fires on a falsifier; inference-composes-never-loses verified per grammar.

**TW6 — Language + fleet completeness.**
MapExpr byte-fold primitives (ByteAt/Fold + the certified lexical set) — the two remaining host
hatches (hex-color, JSON-string decode) move into the auditable grammar surface; emitter dispatch keyed
on declared host CAPABILITY, never host-fn name. `type_annotation` extended to named tuples and tagged
variants (Sheets cell_ref/range_end stop degrading to Span; CSS An+B and at-rule body types land —
the AU.2.6/AU.6.7 TODO cluster closes). `@assoc` directive replaces the hardcoded `^`→Right.
Land-or-delete: the closure/GrammarCall/multi-param surface and the @host directive get a production
consumer or get deleted. Oracle completion: CSV vs the `csv` crate (field-for-field), Math/BNF/EBNF/CSS
Pretty structural oracles; the EBNF `#[ignore]` prettify fix; bootstrap/tailwind byte-canonical CSS
parity behind a grammar-derived normalizer.
*Close gate:* zero grammars without an independent oracle; zero live host hatches not expressible as
grammar; the language-surface census shows no unused "universal abstraction" code.

## §4 — Wave ordering and the D11 coupling

TW0 → TW1 → TW2 → {TW3 ∥ TW4} → TW5 → TW6, with one binding coupling carried from D11: the generator
un-fork (TW2's C fold) regresses to lightningcss-class speed unless the hot-leaf primitives are
cost-gated — so TW2 may land with the *structure* un-forked while the named (a)–(d) primitives remain
behind the existing static dispatch, and TW4 then replaces that dispatch with the measured cost model.
The cutover-gate runs at TW2 close (slice green, static kernels) and again at TW4 close (full system,
cost-model kernels); >SOTA preservation is asserted at both.

## §5 — The forward prompt (the actionable output of this research pass)

> Execute the SK-V20 SPEC from TW0. Cutover commit 0 first (the D-G reconcile; tag skinny
> `sk-v19-frozen`). Then TW0 in full — every gate, deletion, and disclosure; no item is optional. Then
> the fused TW1 substrate landing as ONE fenced commit gated on parity. Then TW2–TW6 in §4's order,
> closing every wave on its named runtime gate in `crates/` — never on a document. The chronic ledger
> (DEFERRED-LEDGER §1) must reach empty by TW4 close. NO quick solutions, NO workarounds: idiomatic,
> gestalt approaches; architectural transpositions for elegance, simplicity, and performance are
> necessary and desirable. NO legacy code: every new path deletes its predecessor in the same commit.
> Zero grammar-specific code in any generic crate, verified by the widened Lock-14 with per-surface
> falsifiers. aarch64/Apple-M only. Cold, same-run, quiet benchmarks only; every comparison asymmetry
> disclosed at the claim site. The emitter is the specializer: grammar-agnostic kernels, cost-model
> selection, checkasm bound to liveness, intrinsics before asm, proof before asm. Every `->` reaches
> the emitter. One tape, one generator, one path alphabet, one regex engine, one decision engine.
> Maximal parallelism and workflow usage; commit before parallelizing; worktrees for overlapping
> writes. Do not relinquish control until the active wave's close gate is green or a genuine fork
> requires the user.

## §6 — Outcome scale

A — TW0–TW4 land, cutover-gate green twice, unqualified >SOTA true on gated planes, chronic ledger
empty. S — honest residual, named per wave, with the gate that blocks it. N — a wave's close gate
cannot go green without a contrivance: surfaced, never paper-closed.
