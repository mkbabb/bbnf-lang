# Dig Shard 11 — AZ-IV: The Union Tranche and the 3000-Commit Direct-Projection Failure

Archaeological excavation of `docs/tranches/AZ-IV/` — 108 files, ~17,700 lines of
prose, ~2.5 MB, the largest single tranche directory in the pre-restart corpus.
This shard establishes what AZ-IV was, why it grew so large, what shipped versus
died, the root causes of divergence it diagnosed, and the deferred work worth
resurrecting. Evidence is cited as `file:line`; git claims are cross-checked
against the repository history where cheap.

## 1. What AZ-IV Was, and Why 2.5 MB

AZ-IV was the **union tranche** — the last fully-executed tranche of the original
bbnf-lang campaign before the restart. It opened 2026-05-01 from the AZ-III
`TERMINAL_WITH_CARRIES` handoff (base `master 01c15564`) and closed 2026-05-02
`complete_with_misses` at `master cb14970f` (`docs/tranches/AZ-IV/FINAL.md:3-5`).
Its charter was to be the tranche that ended chronic deferral by absorbing
everything at once: the AZ-III carry burn-down, the third-hardening-pass
overfit-elimination items, the typed `path!` macro and lazy bail-out parse
(formerly tranche BA), the per-grammar value-API consolidation (formerly BB), the
TS template-literal binding, and a complete failing-test redress
(`AZ-IV.md:1-7`). It codified this as 14 invariants, 23 hard gates, 33
"non-routable carries", and a 7-wave table (`AZ-IV.md:28-117`).

**Why it grew to 2.5 MB** is itself the central finding, not a curiosity. The
directory decomposes as ~1.4 MB of markdown (38 files) plus ~1.0 MB of `.txt`
evidence artefacts (nextest logs, regen dumps, fat-LTO bench outputs; the
failing-test census alone is 192 KB). The markdown is a *cascade of audit
cohorts*: 6 hardening agents (Pauli/Meitner/Wegener/Mencius/Locke/Socrates), 6
loss-prevention agents (Aquinas/Lagrange/Ohm/Averroes/Banach/James), 6
mid-tranche hardening agents (Cantor/Heisenberg/Babbage/Fermat/Diophantus/Boole,
in `audit/HARDENING-2026-05-01-*.md`), 6 mid-tranche audits (`AUDIT-A..F`), 4
deep audits (`DEEP-A..D`), 8 extended deep audits (`DEEPX-1..8`), 4 post-close
audits (`POST-CLOSE-A..D`), plus per-wave evidence. `DEEPX-6` counts **16+ audit
agents on AZ-IV alone** and names the trend explicitly: audit-agent count grew
from 0–2 per tranche (AT–AV) to 4–6 (AX–AZ-III) to 16+ (AZ-IV)
(`audit/DEEPX-6-tranchehist.md:150-171`). The 2.5 MB is the physical footprint of
"union-tranche overload triggers a post-close audit cascade to reconcile what was
promised against what landed" — the meta-pathology this shard documents.

## 2. What Was Attempted — the Waves

The 7-wave shape (`AZ-IV.md:107-117`, `FINAL.md:15-23`):

- **W0 — Truth And Canonical Regen**: strict `xtask regen --check` green 9/9;
  manifest-driven strategy binding; egraph `Map { fn_id }` preservation;
  derive/bootstrap residue eradication; csp-solver re-vendor; bbnf-bootstrap
  cache-nuke fix; dev-iteration baseline; failing-test census.
- **W1 — Grammar Generality + Test Redress**: eliminate 7 `from_rule_name`
  impls, `(layout.kind, rule_name)` builder dispatches, the `EmitStrategy`
  9-arm allowlist; delete `view/color` shim, `recover_*` byte-recovery; drive
  failing tests to zero; Sheets/CSS/JSON/BBNF parity green.
- **W2 — Path IR + Typed `Path<G,T>` + AscentStrategy**: compile-time `path!`
  proc-macro; `path_check` IR pass with inline-trace sidecar; bespoke path lexer
  via bbnf-regex HIR; variant-select step; wildcard `Iter<Item=T>`.
- **W3 — Lazy Bail-Out Parse**: path-driven recognizer on JSON/CSS L4/Sheets/BBNF
  that skips subtrees the path does not visit.
- **W4 — Optimization Substrate Activation**: globalize CSP authority; consume
  SIMD; delete unconsumed `RuleSet` + `egraph::ruler::*`; DTA/dfa cleanup;
  PatternAnnotations migration.
- **W5 — TS Binding + Value-API + Substrate Audit**: `bbnf-path-ts` cdylib;
  per-grammar value-enum dedup; permanent `substrate_audit.rs` CI test.
- **W6 — Measurement And Close**: fat-LTO matrix; samply contract; FINAL.

The wave sequence sits atop the AZ-I → AZ-II → AZ-III direct-to-struct arc that
preceded it; AZ-IV explicitly absorbed the carries all three left unresolved
(`AZ-IV.md:45-65`, the Carry Ledger).

## 3. What Shipped vs What Died on the Vine

The close was honest: of 23 hard gates, `FINAL.md:27-51` records 10 MET, 9
MET_WITH_MISSES, 4 ROUTED. Cross-checked against git, the substrate genuinely
landed:

**Genuinely shipped (verified against commits):**
- Strict regen 9/9 green (`7959e6cb`); a REGEN triumvirate fired on a
  lowering-quartet scope reveal and was absorbed within W0
  (`PROGRESS.md:13,51`).
- csp-solver canonical re-vendor at `3aab34e8b` — the src/ tree byte-mirrored
  against `csc411@b70098676`; all 22 shared files diff-clean; the 5
  csc411-authoritative additions ride along; `.rustfmt.toml` ignores the mirrored
  tree so `cargo fmt` cannot diverge from the canonical source (verified via
  `git show 3aab34e8b`). This closed the long-standing bbnf-lang-vs-csc411 split.
- `bbnf_derive` eradicated across parse-that + wasm; cycle-2 bootstrap wall =
  1.88% of cycle-1 (cache-honesty gate met, `92ce2cb1`+`3aab34e8`+`d36055aa`).
- egraph `Map { fn_id }` cost-extractor pin, red-then-green named test
  (`4373a49d`).
- `path!` proc-macro with compile-time grammar-aware diagnostics (`8ad209f0`);
  `path_check` IR pass + inline-trace sidecar (`f76e4d6a`/`a89cb083`);
  variant-select resolver (`3947c269`); wildcard `WildcardIter` (`d87d7cc0`);
  AscentStrategy trait + 3 impls (`5b74aab2`).
- Lazy `parse_with` cursor threaded through ~264 dispatcher signatures with
  per-iteration `cursor.match_field/match_index` consult (`5cd6e5d9`+`33184651`+
  `715747db`); 19/19 parse_with tests pass on 3/4 grammars.
- `-1032 LOC` deletion of `egraph::ruler::*` + `RuleSet` (`8fa9df03`);
  `dfa_codegen→regex_scan_adapter`, `dta→grammar_facts` renames (`63ade841`);
  PatternAnnotations DELETED (`8ba31a82`).
- `bbnf-path-ts` cdylib + wasm-bindgen (`3adf9d03`); permanent CI-gated
  `substrate_audit.rs` enumerating 886 pub substrates (`407cdc8e`).
- Failing tests driven 142 → 0 via a 3-lane W1 redress (W1-CLOSE ledger); +55 net
  new tests W0→W2 (`PROGRESS.md:14`, `FINAL.md:114`).

**Died on the vine / routed forward as misses:**
- The one thing the whole AZ-I→AZ-IV arc promised — **direct-to-struct
  projection** — did not land (see §4).
- `bbnf_get_twitter` closed at **4196×** sonic-rs (target ≤5×), because the
  value-API entry point calls eager `JsonParser::parse` then walks the tree,
  never routing to the lazy `parse_with` that was built in the same tranche
  (`FINAL.md:44`, `audit/DEEP-B-performance-profile.md:135-178`).
- The AU performance floor regressed **18/19 rows BELOW**, driven by AZ-IV's own
  W5 arena/builder template registry indirection (`FINAL.md:43`).
- TS Node-execute landed **RED** — object/array `value` is a span over input
  bytes, not an aggregated array (`7c5e68bf`, `FINAL.md:48`).
- The permanent substrate-audit test closed the tranche **firing red**: 32
  zero-caller substrates surfaced and none were cleaned inside AZ-IV
  (`FINAL.md:41`, `bd72a784`).
- 2 Sheets Flat-shape lazy tests `#[ignore]`-marked (`715747db`); Tailwind
  regex_scan timeout still unprofiled and routed to a successor
  (`FINAL.md:63`).

**Aftermath (what actually happened next):** BA (direct-projection codegen) was
fully *specified* — `DEEPX-8` wrote `BA/BB/BC` top-level plans plus 7 wave specs
each (`audit/DEEPX-8-fullspec.md:1-40`; the directories exist). One opening
verification commit landed (`095dd7667 docs(ba): add BA.W-1 opening
verification`), and then the entire campaign was abandoned: `9dde66ab1 chore(tranches):
archive pre-restart BA/BB/BC; clean slate for re-draft` and `6098a4b52 docs(tranches):
land BA/BB/BC restart drafts (Phase 2)` on 2026-05-03. **BA direct-projection was
never executed.** The 3000-commit indirection was never retired; it was
inherited by the restart. This is the single most important fact for SK-V21: the
architectural defect AZ-IV diagnosed with samply precision is still latent.

## 4. Root Causes — Where and How Divergence Began

AZ-IV's own audit cohort is the richest root-cause archaeology in the corpus. The
findings, with evidence:

### 4.1 The primary defect: typed projection is computed then thrown away

`project_types` populates a `StructRegistry` at codegen time carrying rich
`BoxedEnum`/`Tuple`/`HeterogeneousAltJoin` typings, but **the parser never
consumes it**. Every emitted parse fn constructs a runtime literal
`__layout: StructLayout { rule_type: TypeDesc::Span, fields: vec![] }` at ~10
emission sites; type inference output is discarded at the parse boundary
(`audit/DEEP-A-struct-projection-assay.md`, summarized `audit/SYNOPSIS-2026-05-03.md:13`).
For 5 grammars (BNF, EBNF, CSV, CSS Pretty, Math) the `SimpleStructBuilder`
deposits `V::unit()` for every typed leaf — the starkest
`feedback_typed-materialization-invariant` violation in the codebase
(`audit/DEEPX-7-sotapath.md:194`).

### 4.2 The runtime cost: `Vec<OpenFrame>::clone` at 86.07% inclusive

Because direct projection is not emitted, the parser does runtime
checkpoint-and-rollback over an untyped slab. `<JsonStructBuilder as
StructBuilder>::checkpoint` deep-clones the entire in-flight `Vec<OpenFrame>` on
every speculative branch. A samply trace (25,963 samples, fat-LTO, 7-artefact
contract) attributes **86.07% of inclusive samples** to `Vec<OpenFrame>::clone`
(`audit/DEEP-B-performance-profile.md:16-21,254,344-405`). The generated JSON
parser calls `checkpoint()` at 26 sites per `parse_wrap_JsonParser_value`, invoked
once per JSON value (twitter has 27,259 values), each cloning a stack whose
`OpenFrame` arms hold `Vec<JsonValue>`/`Vec<JsonPair>` that clone recursively —
quadratic in nesting depth × checkpoint frequency
(`DEEP-B:279-295`, sites `generated/json.rs:1876-2026`,
`runtime/json/builder.rs:243-251`). DEEP-B proved these are one defect at two
altitudes: DEEP-A named the architectural shape (typed projection unwired), DEEP-B
named the runtime mechanism (clone-on-checkpoint), and the same mechanism recurs
across CSS L4 (1,407 checkpoint sites), Sheets, and BBNF (217 sites)
(`SYNOPSIS-2026-05-03.md:15`).

### 4.3 The 3000-commit archaeology: why every reform renamed the shim

`DEEPX-5-projfail-archaeology.md` answers the user's question — *"why has project
to struct failed to land for nearly 3000 commits?"* — by reconstructing 7 eras
(`audit/DEEPX-5-projfail-archaeology.md:29-268`):

- **Era 0** (`f419b6d3`): `BumpArena<T>` per-type direct emission — *the closest
  the project ever came to direct-to-struct*.
- **Era 1** (AA/AB/AC): the tape thesis subsumes it; the slab is deleted at
  `031cff59`.
- **Era 2** (AY/B5): `OpenFrame` is born tape-internal (`feffe271`), the W5
  close-stamping experiment is deleted after a -27% twitter regression
  (`a13840a0`), then the discipline migrates to user-space as `ValueBuilder`
  (`4f42f6bb`).
- **Era 4** (AZ-I): `StructBuilder` trait + `JsonStructBuilder` + `OpenFrame`
  stack land (`85cf83e7`); the "direct" in `StructDirect` is a misnomer — it
  routes through a runtime builder with a runtime stack (`DEEPX-5:152-163`).
- **Era 5** (AZ-II cutover.O): the tape crate dies (`6a6ca1fd`), StructDirect is
  "canonical" *in name*, but the builder-trait + OpenFrame + layout-literal
  substrate survive untouched.
- **Era 7** (AZ-IV): W5.3 dedup (`0744c9f9`) *cements* the indirection by
  promoting it from per-grammar copy to one parameterised
  `SimpleStructBuilder<'p,V,C>` template — making deletion require every typed
  enum to change shape simultaneously (`DEEPX-5:326-353`).

The verbatim archaeological tell: the cutover.O commit bodies (`e7306d6d`,
`f7ed4c74`, `6a6ca1fd`, `6effcb0b`) all say *"narrows the live codebase toward the
struct-only projection path"* — every commit *narrows toward*; none *arrives at*
(`DEEPX-5:459-462,617-622`). The structural mechanism: **substrate-first /
consumer-forward discipline crossed with the no-orthogonal-codepaths invariant**.
Substrate lands first ("every existing test still passes"); consumer routes
forward; once the substrate is canonical, deleting it requires editing every
consumer's typed shape at once — which `feedback_preserve_rich_ast` forbids except
in a lockstep wave no tranche cap accommodates. *The indirection is structurally
self-protecting once it is generic* (`DEEPX-5:558-568`).

### 4.4 Chronic deferral: 13 of 15 items deferred ≥3 tranches

Boole's genealogy tallied deferral counts across B5+AZ-I+AZ-II+AZ-III+AZ-IV and
found **13 of 15 items deferred ≥3 tranches** (`audit/HARDENING-2026-05-01-boole.md:500-519`).
Three meta-causes (`boole:521-533`): (1) "substrate-with-consumer" routinely
declared MET on substrate alone; (2) sibling/cross-repo work has no gate; (3)
stale-but-reachable code lacks a deletion deadline. Boole's response — the
Non-Routable Carries mechanism — was the AZ-IV innovation designed to break the
cycle (`boole:535-579`).

### 4.5 The fictional-tranche escape hatch (the smoking gun)

The Non-Routable mechanism *failed at its own close*. `POST-CLOSE-C-carries.md`
found AZ-IV routed three MASKED-DEFERRALs (F2 sonic 4196×, F5 TS RED, AF AU floor
8+ tranches) to a fictional successor letter **"AZ-V"** that appears 4 times in
close-state docs and **0 times in any trajectory/plan doc** — no `docs/tranches/AZ-V/`
directory exists (`audit/POST-CLOSE-C-carries.md:359-373,424-428`). This directly
violates `GESTALT.md:188` ("a non-routable item that cannot land without changing
the thesis triggers a triumvirate scope-reveal review of the thesis itself, not a
new tranche letter"). The AU floor is the longest-running perf gate in the project
(8+ tranches, AU→AZ-IV); AZ-IV's own W5 substrate *caused* the regression and then
deferred the fix to a phantom letter — "Y added, X still present" shim hell
(`POST-CLOSE-C:277-303`).

### 4.6 Profile-driven evidence retreats faster than it advances

`DEEPX-5:593-604`: when measurement said "this is the bottleneck," the project
retreated from the experiment that caused it (AY.W5's -27%, deleted) or routed the
indirection forward (AZ-IV.W6.1's 86.07%, routed to AZ-V). When measurement was
missing (AZ-I through AZ-III never benched under fat-LTO against the AU floor), the
indirection silently grew. The AZ-III deferral of fat-LTO measurement to "BB.close"
is the specific enabler (`POST-CLOSE-C:87`).

### 4.7 Dev-iteration walls

Boole's wall-time matrix (`boole:19-46`): `cargo check -p bbnf-bootstrap` >130 s
(killed at 23 min cap) because `scripts/bootstrap-bbnf.sh:28` deletes
`target/.bbnf-cache/` before every run, defeating the content-keyed cache; strict
regen RED on 7/9; workspace nextest 292 s; fat-LTO bench ~10 min/harness. The close
ceremony exceeded 15 minutes of cold compute (`boole:40-45,49-62`).

## 5. Deferred / Abandoned / Lost Work Worth Resurrecting

The following are the load-bearing designs, measurements, and negative results
that must not be re-learned. Most are latent — specified, never executed.

### 5.1 The single-mechanism fix (the highest-value resurrection)

`DEEPX-7-sotapath.md` states it once (`audit/DEEPX-7-sotapath.md:10-14`):
**direct-projection codegen consuming the `StructRegistry` that `project_types`
already populates, with cheap-checkpoint speculative discipline, eliminating the
runtime arena/builder template indirection.** It closes *every* chronic (4196×
get gap, 18/19 AU floor, TS RED, substrate audit, Sheets Flat-shape) as one
defect viewed at different altitudes. Three composing moves:
1. **Direct-projection codegen** — emit per-grammar `<Grammar>Document` typed
   struct + `<Grammar>Value` typed enum from `StructRegistry`; parse fns write
   typed fields into a single `bumpalo::Bump`; `JsonValue::Object/Array` switch
   from `JsonObjectId(u32)` handles over `Vec<Vec<…>>` to `&'p [JsonPair<'p>]`
   slices (`DEEPX-7:68-93`, DEEP-B rec 5).
2. **Cheap checkpoint** — replace `type Checkpoint = Vec<OpenFrame>` (deep clone)
   with `struct Checkpoint { stack_depth, arena_count, pending_key }`; rollback
   truncates rather than restores (partial frames are garbage on Err); bump arena
   truncate is O(1) (`DEEPX-7:247-261`, DEEP-B:499-518). Predictive first-byte
   dispatch where alphabets are disjoint (JSON: `"`=string, `[`=array, `{`=object,
   `t/f`=bool, `n`=null, digit/`-`=number) eliminates speculation entirely for JSON
   (`DEEPX-7:263-274`). Estimated ≥80% inclusive-sample reduction (DEEP-B).
3. **`get<T>` reroutes through `parse_with`** — eager `parse(input)` becomes
   `parse_with(input, &EMPTY_PATH)`; one codepath; closes the 4196× gap by two
   orders of magnitude (DEEP-B rec 2).

### 5.2 `->`-less type inference (the generalization key)

`project_types` already produces a `TypeDesc` for every rule, annotated or not
(`registry.rs:114`, `unwrap_or(TypeDesc::Span)` fires only on cycle-break failure
which provably does not occur in the fleet). The gap is *consumption*, not
inference. The fix is an inverse-layout-audit IR pass: for every structural
`TypeDesc` without a `StructLayout`, generate one by extending the existing
projection logic — **no new heuristic**. The consequence: **`->` becomes a naming
hint, not a typing hint**; a grammar can be written with zero `->` annotations and
still project typed output (`DEEPX-7:278-304`, per-shape projection table at
`DEEPX-7:288-296`). This is the true generalization the SK-V21 charter demands;
resurrect it verbatim.

### 5.3 The sonic-class `get` API with compile-time grammar-aware diagnostics

The "better than SOTA" anchor (`DEEPX-7:310-348`, `DEEP-SYNTHESIS.md:56-84`):
`path!(Json, "statuses", 0, "text")` validates against `StructRegistry` at
proc-macro time, infers the terminal `TypeDesc`, and returns
`TypedPath<Json, &'static str>`; invalid paths fail at `cargo build` with a
`syn::Error` naming the segment, the resolved struct type, and valid alternatives.
Superior to sonic-rs's runtime-`None` `pointer!` (no grammar, forces `.as_str()`
coercion), simdjson's manual cursor, and lightningcss's CSS-only typed values.
Wildcard returns a zero-allocation `Iter<Item=T>` — *the path expression IS the
iterator*. The anchoring claim: bbnf has the grammar; competitors do not.

### 5.4 SOTA ingredient extraction (already done — do not re-survey)

`DEEPX-7:18-62` distilled sonic-rs (backed-slice `Value` arena, pointer-walk skip),
simdjson OnDemand (stage-1 structural index, path-as-iterator, lazy leaf
materialisation via a `Decision::ProjectLeaf(reader_fn)` cursor variant),
lightningcss (typed leaves at parser level, per-property typed structs), and
yyjson (in-place payload placement, one bump arena, bounded allocation). Each is
mapped to a concrete bbnf integration point through `StructRegistry`. SK-V21's
frontier researchers should refresh against 2026-07 SOTA but need not re-derive the
integration architecture.

### 5.5 Negative results that must not be re-learned

- **The tape thesis was a dead end.** Three eras (tape, builder, dedup template)
  each re-architected around the indirection and framed it as the direct-projection
  landing. Do not re-introduce a tape/view/fused substrate (`DEEPX-5:547-568`).
- **Dedup of the indirection ≠ deletion.** W5.3's parameterised template made the
  fault *harder* to delete. Any "dedup template" whose commit body acknowledges
  outliers (JSON/CSS L4/Sheets/BBNF resisted the template) is the warning sign;
  trigger thesis review (`DEEPX-5:644-651`).
- **Substrate-first / consumer-forward is the disease.** Lockstep substrate +
  consumer landing in one wave is mandatory for the direct-projection retirement;
  both land or neither (`DEEPX-5:608-614`).
- **Profile under fat-LTO at every wave close, not tranche close** — the AZ-III
  deferral is what let the W5.3 regression stay invisible until W6.1
  (`DEEPX-5:636-642`).

### 5.6 Concrete deletion targets (already enumerated)

`POST-CLOSE-A-legacy.md` gives 24 file:line findings routed into 3 commits: the
`LegacyPath`/`LegacySegment` re-lowering shim duplicated across 4 grammars
(`runtime/{json,bbnf,css_l4,google_sheets}/parse_with.rs`, ~80 LOC + one
allocation/parse); the `__EAGER_EMPTY_PATH: LazyLock<TypedPath<markers::Json,…>>`
hardcoded to `Json` for *all 9 grammars* (a cross-grammar leak surviving the W1
overfit scan because the scan excludes `generated/**`,
`emitter/grammar.rs:414-425`); the HRTB `for<'__c> PathSchema<'__c>` across ~264
dispatcher signatures (collapse via eager/lazy cursor split); dead
`backend/strategy/{ref,seq,repeat}_strategy.rs`, `graph/scc.rs`
`calculate_*_deps_scc`, `generate/serialize`, `path/ascent.rs` (277 LOC,
bench-only consumer), `rewrites/path_seed.rs` (no production saturation consumer)
(`audit/POST-CLOSE-A-legacy.md:14-112`). These are the cleanup that BA.W0 was
specified to absorb (`DEEP-SYNTHESIS.md:161-165`).

## 6. Meta-Process Pathologies

`DEEPX-6-tranchehist.md` is the definitive process forensics
(`audit/DEEPX-6-tranchehist.md:100-235`):

1. **Union-tranche overload → post-close audit cascade.** AZ-IV absorbed
   BA+BB+AZ-III+TS+test-redress into one tranche and produced 16+ audit agents to
   reconcile what could and could not land inside its authoring window. The three
   dominant chronics (F2 sonic, AF floor, F5 TS) share *one* mechanism
   (direct-projection) that could not land in a 7-wave tranche already at capacity.
   The right call would have been a plan-time split (`DEEPX-6:116-127,177-184`).
2. **The B-prelude annex is the project's reliability shape.** B0–B7 all closed
   cleanly (GENUINE/GENUINE_partial), ~1 day each, ~95% promise-close ratio, via
   bounded charter + explicit successor pre-condition + hard scope cap
   (`DEEPX-6:129-139`). The AY/AZ series is the innovation shape; the failure mode
   is only ever *union framing without a plan-time split*.
3. **`terminal_with_carries` with a named destination is honest; with a fictional
   destination is the failure.** AV, AX, AY-I did honest scope cuts; AZ-IV's "AZ-V"
   mentions were the dishonest variant (`DEEPX-6:141-147`).
4. **"Substrate-with-consumer" declared MET on a permanent CI test is a forcing
   function, not a closure.** AZ-IV closed with the substrate-audit test firing red
   on 32 items — the exact pattern the non-routable language was meant to prevent
   (`POST-CLOSE-C:388-401`).
5. **Close-criterion softening at the close.** "100% pass" quietly became "100%
   modulo 4 environmental fails"; the discipline lever weakened by exactly the slack
   the framing was supposed to deny (`POST-CLOSE-C:403-415`).

Recommended discipline for successors (`DEEPX-6:173-235`, `SYNOPSIS-2026-05-03.md:52-57`):
single-thesis scope (reject union framing); non-routable carries close in-tranche
or trigger triumvirate *at plan time*; audit cap 6 plan-time + 3 hardening + 1
synthesis = 10 max; promise→pure-MET ratio ≥80% or thesis review before FINAL;
every routed carry names a real successor letter (CI-gate the existence check).

## 7. Bottom Line for SK-V21

AZ-IV was the honest, exhaustive, well-instrumented terminal diagnosis of the
original campaign — and its own diagnosis proves the campaign never solved its
founding problem. The `->` typed-projection promise (GESTALT §2.4) was iterated
across 10 tranches and delivered in none (`DEEPX-5:514-530`). The samply-precise
root cause (`Vec<OpenFrame>::clone`, 86.07%) and the single-mechanism fix
(direct-projection + cheap checkpoint + `get→parse_with`) were fully specified in
BA/BB/BC and **never executed** — the restart archived them 2026-05-03. SK-V21
inherits a codebase whose parse path is still the indirection-laden builder-trait +
OpenFrame stack, whose value API is 4196× off sonic-rs, and whose AU floor is 18/19
below. The resurrection priority is unambiguous: **the DEEPX-7 single mechanism and
the DEEPX-8 BA wave shape are the closest thing the corpus has to a proven,
measured, un-executed plan** — but the meta-lesson (single-thesis scope, lockstep
substrate+consumer, fat-LTO at every wave close, no fictional successors) is what
determines whether SK-V21 executes it or renames the shim for the fourth time.
