# DEEPX-5 — Project-to-Struct Archaeology

> Read-only git archaeology at master `40e1835d` (2758 commits across all
> refs; 2583 on master). Worktree
> `/Users/mkbabb/Programming/bbnf-wt-deepX-5`, branch `deepX-projfail`.
> `CARGO_TARGET_DIR=/Users/mkbabb/Programming/bbnf-wt-deepX-5/target/deepX-5`.
> Read-only. One document landed.

## Mandate

User question (verbatim): *"Why has project to struct failed to land for
nearly 3000 commits?"*

The user's intuition: direct projection — the architectural promise that
*every `->` in the grammar projects directly to a typed record at emission
time, without an intermediate untyped phase* (GESTALT §2.4) — should have
landed many times across the project's history but did not. This audit
proves the intuition by reconstructing the era timeline; the architectural
substrate has been re-architected at each major tranche, the *promise* has
been emitted nine times in commit messages, and the *runtime indirection*
(arena slab + builder open-frame stack + speculative-rollback `Vec` clone)
has survived every reform.

DEEP-A and DEEP-B already named the present-tense mechanism (the
`StructLayout` runtime literal, `OpenFrame` stack, `Vec<OpenFrame>::clone`
checkpoint). This audit asks: *why did each prior reform fail to retire
that mechanism?*

## Era Timeline (with commit hashes)

### Era 0 — Pre-tranche prelude (commits 1 to ~250, oldest 50)

The earliest commits (`0b1e1f3e initial working commit`, `2be8f34f
partially working. again.`, `e7dcfff5 error reporting :D`) establish the
codebase as a parser-combinator generator. Commit `b0acdfde feat:
delimiter-driven flat scanner for Wrap(Repeat(Alt)) patterns` is the first
performance discipline; `f419b6d3 feat: monolithic arena codegen,
BumpArena, cold-only benchmarks` introduces the **monolithic arena** —
`BumpArena<T>`, "direct recursive functions with zero combinator overhead"
— the first attempt at direct emission. The pattern at this point: the
emitter writes typed structs directly; arena holds child references; no
`StructLayout`, no builder trait, no runtime stack. **Direct-to-struct
exists in this era**, but is built on `BumpArena<T>` (per-type), not on
the grammar registry.

### Era 1 — Tranche AA / AB / AC: tape-first emitter (~commits 13411847 → 094ce8e8 → 568244cb)

Tranche AA introduced the tape concept: `13411847 docs(tranche): add Tranche
AA plan + baseline cleanup`. Tranche AA.7 deliberately *deferred* "TaggedUnion
narrowing behind the tape"; AA.9 *subsumed* "direct-to-slab scratch behind
the tape"; AA.11 *folded* "structural bitmap into the tape format"; AA.12
*deferred* "perfect-hash dispatch behind the tape". This is the era where the
tape-as-substrate thesis was authored as a forcing function; AA.7/9/11/12 are
the explicit retirements of *direct* mechanisms in favour of the tape.

Tranche AB.2a (`094ce8e8 feat(core): Tranche AB.2a — tape-first emitter
substrate`) lands `Parsed<View>` and `tape_prelude.rs`; "every existing
test still passes; generated parsers still emit the eager-AST shape". The
substrate-first / consumer-second discipline starts here. AB.0 introduces
`MaterializationClass`; AB.1 routes through CSP joint strategy.

Tranche AC.2 (`031cff59 refactor(rust-backend): delete slab alloc
substrate (Tranche AC.2)`) deletes the original slab — **the first
direct-to-struct surface dies** — in favour of the tape.

**Did direct-to-struct land in Era 1?** No. The tape thesis explicitly
subsumes it. The slab arena from Era 0 is deleted at AC.2 and replaced
by a tape-with-views surface.

### Era 2 — Era IV / V: tape construction and refinement (~AY tranches)

Tranche AY.W5 (`feffe271 build(tape): TapeBuilder::open_compound +
close_compound (AY.W5.a.2)`) is **the first appearance of `OpenFrame`** —
not in a builder, but in the tape. `pub fn open_compound(...) ->
TapeOffset; pub fn close_compound(compound_offset, span_hi)` with `Vec<OpenFrame>
open_stack`. The open-frame discipline that survives to HEAD is birthed
here as a tape-side construct.

AY.W5 also introduces `note_push` per direct child of every open compound
— "two column writes on every direct child". Cumulative AY.W4→W6 twitter
regression: -27% MB/s. This is the **first profile-driven retreat from
the tape's own optimisation**.

`a13840a0 refactor(tape): retire W5-era open_stack + note_push +
SIB_SKIP_STAMPED_BIT (AY-II.W0.a)` deletes the W5 experiment wholesale
("Substrate rollback per AY-II.W0.a — the AY.W5 write-time close-stamping
experiment is deleted wholesale").

`4f42f6bb feat(runtime): introduce ValueBuilder + fused Parsed::to_value +
retire navigate_tape (AY-II.W0.c)` introduces the **first ValueBuilder
with `OpenFrame`-style discipline in user space** — `begin_compound /
end_compound / push_leaf / rollback_to primitives mirror TapeBuilder
open/close discipline; frame stack + arena truncate atomically on
ValueCheckpoint rollback`. This is where the begin/end-compound rollback
discipline migrates from tape-internal to user-facing-runtime.

`e090f2cb feat(tape,core): unify substrate as Tape<R>; delete FusedBuilder
+ FusedOutput + ValueFramesOutput (B5.W1)` collapses two welded halves
into one. The pre-direct-to-struct landing has the runtime-stack
discipline already deeply integrated into the tape itself.

**Did direct-to-struct land in Era 2?** No. The ValueBuilder is the
ancestor of `JsonStructBuilder`; it inherits the begin/end-compound +
checkpoint/rollback discipline that becomes the AZ-IV.W6.1 hot path
(`Vec<OpenFrame>::clone` at 86.07% inclusive samply samples).

### Era 3 — AZ planning: tape abrogation thesis (`67758c18`, `f9afa87d`, `f321ce99`)

The thesis that direct-to-struct is canonical and the tape must die was
authored at `f9afa87d docs(az): AZ plan — direct-to-struct + tape
dissolution thesis + invariants + hard gates` and refined through the
AZ split: `ecd12792 docs(tranches): physical split AZ → AZ-I + AZ-II`.
AZ-I would land struct-direct emission for JSON/CSS/Sheets while keeping
tape for BBNF; AZ-II would delete tape entirely.

`70bdf428 docs(az-i): rewrite for JSON/CSS/Sheets scope — tape remains
for BBNF` and `46d69a5b docs(gestalt, risk): AZ-II is required, not
optional — remove softening language` — the softening-resist phrase
that was already required by 2026-04 is the audit's first signal that
the project knew direct-to-struct was being deferred.

**Did direct-to-struct land in Era 3?** No. The thesis lands as a
*plan*; mechanism is routed to AZ-I/AZ-II.

### Era 4 — AZ-I: per-shape struct-direct emitters (~Feb 2026, ~85cf83e7 → e0e0af30 → 401b3e65)

The AZ-I.W2 wave is the first concrete attempt:

- `8f5e50f4 docs(az-i): W2 emitter-rewire plan — per-grammar codegen-time
  dispatch` — the EmitStrategy plan (5-redress-agent decomposition).
- `e0e0af30 feat(ir): land StructRegistry substrate (AZ-I.W1.A)` — **first
  appearance of `StructRegistry`**, the registry now consulted-but-not-read.
- `43ea56bb feat(ir): project_types populates StructRegistry; probe
  accepts &StructRegistry (AZ-I.W1.A)` — the projection pass starts.
- `1c6f00d0 feat(emitter): land EmitStrategy enum + per-grammar resolver
  (AZ-I.W2.RA)` — the **`StructDirect` variant first appears** as one of
  two EmitStrategy variants (StructDirect / TapeDirect).
- `41dd776e feat(emitter): dual-emit struct-direct bodies for
  Object/Array/AltDispatch (AZ-I.W2.RB)` — the first `begin_compound(&__layout)`
  / `end_compound(handle)` emission.
- `85cf83e7 feat(runtime): land StructBuilder trait + JSON value substrate
  (AZ-I.W2.A)` — the StructBuilder trait + `JsonStructBuilder<'p>` with
  in-flight `OpenFrame` stack. This is the **first appearance of the
  user-runtime `OpenFrame`** that survives to HEAD (`runtime/json/builder.rs:60-87`
  per DEEP-A).
- `e0388ed8 feat(runtime/json): JsonDocument accessor API mirroring Parsed
  surface (AZ-I.W2-act.A)` — JsonArena/Ids first appear.
- `192efcd3 feat(runtime/css_l4): typed-value enum family + arena +
  builder + document (AZ-I.W2-act.B3)` and `a8657537 feat(runtime/google_sheets):
  SheetsValue + arena + builder + document substrate (AZ-I.W2-act.B2)`.

**Did direct-to-struct land in Era 4?** *Partially*. JSON / Sheets /
CSS L4 ship a `StructDirect` body — but the emission still uses a
`__layout: StructLayout = { rule_type: TypeDesc::Span, fields: vec![] }`
*runtime literal* (`571e0f46 fix(emitter): alt_dispatch struct-direct
layout literal + lifetime`); the dispatch goes through a builder trait
(`begin_compound(&__layout)` / `end_compound(handle)`), and the runtime
stack of `OpenFrame`s is exactly the per-compound `Vec` allocator the
audit chain identifies as load-bearing. **The "direct" in
`StructDirect` is a misnomer**: the emission still routes through a
runtime builder with a runtime stack; it is *struct-arenaed*, not
*struct-direct*.

The `from_rule_name(&str) -> Kind` impls (per-non-JSON grammar) appear in
this era — `911ee70f feat(runtime): bnf + ebnf + css_pretty struct-direct
substrates`, `6b2f3ca7 feat(runtime/math): struct-direct substrate +
EmitStrategy resolver-arm`, `57e017de feat(runtime/csv): struct-direct
substrate + EmitStrategy resolver-arm`, `ec7a0fa1 feat(runtime): bbnf
struct-direct runtime + resolver-arm + RuntimeView`. **Each grammar gets
its own per-grammar literal-rule-name match** — exactly the surface AZ-IV.W4.4
T1 retires (43c313f9), three tranches and ~12 months later.

### Era 5 — AZ-II: cutover.O series; tape deletion; `StructDirect` "wins"

The AZ-II.cutover.O series is the most concentrated direct-projection work:

- `cc162869 docs(az-ii): add historical cutover A-O agency wave specs` —
  cutover phases A through O.
- `94620aaa Gate StructDirect legacy view emission` — first `StructDirect`
  gate.
- `e7306d6d fix(emitter/struct-direct): purge generated view residue`
  (cutover.O3) — generated view helpers deleted.
- `f7ed4c74 fix(emitter/struct-direct): remove tape offset plumbing`
  (cutover.O4) — `Parsed<R>` and `TapeDirect` removed from emission.
- `6effcb0b fix(emitter/shape-tape): delete shape tape branches`,
  `de522995 fix(emitter/inline-tape): delete inline tape emitters`,
  `15bd381a fix(emitter/wrap-tape): delete wrap tape dispatch` — tape
  emission paths deleted.
- `6a6ca1fd fix(runtime/tape): delete tape crate` — **the tape crate dies**.
- `43f0795b docs(AZ-II): cutover.M Phase 3 close — FINAL.md + post-AZ-II.json
  reflect 8/9 fleet StructDirect activation` — 8 of 9 grammars on
  StructDirect (EBNF deferred per the cutover.M cap).

**Did direct-to-struct land in Era 5?** *In name*, yes; *in mechanism*,
no. The tape died. The 9 grammars route through StructDirect emission.
But the JsonStructBuilder, OpenFrame stack, builder-trait dispatch, and
`StructLayout` runtime literal all *survive* — they were never the
abrogation target. The phrase *"narrows the live codebase toward the
struct-only projection path"* in commit body templates (e7306d6d,
f7ed4c74, 6a6ca1fd, 6effcb0b — verbatim duplicate text) is the
archaeological signature: the deletion pattern targets the **tape**
exclusively, not the **builder-trait + OpenFrame stack** that replaced
it.

### Era 6 — AZ-III: terminal close; named carries; perf gaps survive

`c3d4e029 docs(az-iii.final): convert FINAL.md to terminal close
(TERMINAL_WITH_CARRIES)` and `5211b953 docs(trajectory): refresh
REMAINING-TRAJECTORY, BA, BB, GESTALT, codegen-paths for AZ-III outcome`.

AZ-III's contribution is the *audit infrastructure* that surfaces the
indirection cost: `c4b5f666 docs(az-iv.audit/heisenberg): legacy and
naming surface census`, `a60189d6 docs(az-iv.audit/babbage): substrate
activation matrix`. The 32-zero-caller substrate residue (POST-CLOSE-A
finding) accumulates here.

**Did direct-to-struct land in Era 6?** No. The audit surfaces gather
but the emitter remains routed through builder.

### Era 7 — AZ-IV: union tranche; promised but not delivered

AZ-IV (~`db8b00ad docs(az-iv): seed planning canon`) opens with the
explicit promise to absorb the third hardening pass plus all chronic
non-routable carries. Plan documents reference *"every Named rule
projects to a typed record at emission time"* (the GESTALT §2.4
generalised promise, AZ-IV invariants 6 + 7).

What landed:

- W1 — `0ffbd754 refactor(runtime/discriminator-from-rule-id): rule-id-keyed
  kind dispatch + leak_static_str delete (AZ-IV.W1.1+W1.7)`. The seven
  `from_rule_name(&str) -> Kind` impls are replaced by `from_rule_id(u32)
  -> Kind` integer-literal lookups. The seam moves from string-literal
  to integer-literal — but the seam survives.
- W4.4 — `43c313f9 refactor(ir/registry+runtime/arena): T1 lift compound-kind
  from rule_id to layout (AZ-IV.W4.4)`. T1 transposition: rule-name strings
  drawn from the registry now key the seven per-grammar match expressions.
  *"Rule ids drift between regen passes; rule names are the durable
  contract surface — the registry IS the lookup."* The per-grammar
  literal-name match arms are finally retired ~12 months after their
  Era 4 appearance.
- W5.3 — `0744c9f9 refactor(runtime/arena-builder-template)/aziv-w5.3:
  dedup slab + frame skeleton across BNF/CSV/EBNF/CssPretty/Math`. The
  W5.3 dedup commit body claims to dedup *"the slab discipline (push /
  resolve / count / truncate) and the open-frame stack + checkpoint /
  rollback + deposit logic"*. **What it actually did**: it generalised
  the indirection — `arena_template::CompoundSlabArena<C>` and
  `builder_template::SimpleStructBuilder<'p, V, C>` parameterise the
  Open-frame discipline, so all five simple grammars now share *one
  template* rather than six per-grammar copies. The **discipline survived
  the dedup**: per-compound `Vec<OpenFrame>` push, per-`begin_compound`
  layout clone, per-checkpoint stack clone — all preserved.

The W5.3 commit also acknowledges (verbatim): *"JSON / CSS L4 / Sheets /
BBNF stay distinct — their arena or builder shape diverges (two-slab
JSON; six-slab CSS L4; kind-only Sheets; bounded BBNF). Forcing them
onto the template would either change the typed *Value signatures
(forbidden by the empty-return rule) or multiply the template's variant
axes until the dedup eats itself."* This is the archaeological signature
of the failure mode: **dedup of the indirection** rather than **deletion
of the indirection**.

**Did direct-to-struct land in Era 7?** No. AZ-IV.W6.1 bench evidence
(`aa31fe56 benchmarks(post-az-iv-matrix)/aziv-w6.1`) shows the AU floor
regressed 18/19 BELOW; samply attributes 86.07% inclusive to
`Vec<OpenFrame>::clone`; the audit chain (`078c8276` DEEP-A,
`d4b8de18` DEEP-B, `df33e160` DEEP-C, `40092b28` DEEP-synthesis) names
the fault.

## Specific Archaeological Questions

### Q1 — Arena introduction; ever permanent?

**First**: `f419b6d3 feat: monolithic arena codegen, BumpArena, cold-only
benchmarks` (Era 0). This was a `BumpArena<T>` per-type — explicitly
*direct emission*. The "direct recursive functions with zero combinator
overhead" framing matches GESTALT §2.4 verbatim.

**Trajectory**: deleted at `031cff59 refactor(rust-backend): delete slab
alloc substrate (Tranche AC.2)` in favour of the tape (Era 1). Reborn
as the ValueBuilder arena at AY-II.W0.c (`4f42f6bb`, Era 2). Reborn again
as the JsonArena at AZ-I.W2.A (`85cf83e7`, Era 4). Replaced by the
arena_template at AZ-IV.W5.3 (`0744c9f9`, Era 7).

**Was it ever meant to be permanent?** The Era 0 `BumpArena` was the
permanent direct-projection mechanism per its commit body. The Era 1 tape
substituted it on the thesis that tape-then-views was the canonical
shape; the Era 2 ValueBuilder substituted it on the thesis that fused
tape+value was canonical; the Era 4 JsonArena substituted it on the
thesis that struct-direct emission was canonical (deleting tape); the
Era 7 arena_template substituted it on the thesis that dedup was
canonical.

**Conclusion**: every era's arena was framed as *permanent* (canonical,
KISS, the substrate of record). Every era's arena lasted exactly until
the next thesis-pivot. The Era 0 `BumpArena` is the closest the project
ever came to direct-to-struct; every subsequent reform added an
indirection layer (tape header, `ValueFrame`, `OpenFrame`, `StructLayout`
runtime literal).

### Q2 — Builder introduction; supposed to replace what?

**`<Grammar>StructBuilder` first appears at `85cf83e7 feat(runtime): land
StructBuilder trait + JSON value substrate (AZ-I.W2.A)`**. Commit body:
*"The pluggable consumer surface for direct-to-struct emission on the
three data grammars: emitters call trait methods unconditionally."*

**Supposed to replace**: the `tape.push_compound_pre_order` /
`tape.end_compound*` / `tape.push_branch_tag` emission triple. The
StructBuilder trait emits `begin_compound(&__layout)` / `end_compound(handle)`
/ `push_branch_tag(idx)` instead.

**What it actually did**: it transposed the runtime indirection from a
shared tape to a per-grammar typed-stack. The `OpenFrame` discipline is
preserved (`runtime/json/builder.rs:60-87`); the `Vec` per compound is
preserved (each `OpenFrame` arm holds a `Vec<JsonValue>`/`Vec<JsonPair>`);
the speculative-rollback discipline (`Self::Checkpoint = stack.clone()`)
is preserved.

The commit body labels this *"the pluggable consumer surface for direct-to-
struct emission"* — but the emission goes through `begin_compound` /
`end_compound` / `push_*` trait methods, not into typed struct fields by
offset. **The trait IS the indirection** that the GESTALT §2.4 invariant
forbids.

### Q3 — W5.3 dedup commit (`59350ec8` per dispatch; actual: `0744c9f9`)

The dispatch references `59350ec8`; the actual W5.3 commit is `0744c9f9`.
Body verbatim:

> Per Q-final B2 = (a) (structural skeleton dedup; typed *Value enums
> survive byte-identical), introduce two generic templates that absorb
> the slab-of-Vec compound arena and the open-frame stack + checkpoint /
> rollback + deposit logic shared by the simple-cohort grammars

The commit *acknowledges* what it does not retire — *"slab-of-Vec
compound arena"* and *"open-frame stack + checkpoint / rollback /
deposit logic"* are explicitly preserved (parameterised, not deleted).

**What did it claim to retire?** Per-grammar duplication of the slab +
frame discipline across BNF / CSV / EBNF / CssPretty / Math. LOC reductions
58–74% per grammar.

**What did it actually retire?** Six copies of the indirection became
one parameterised copy. **The architectural fault was promoted to a
generic substrate**, making it harder to delete because deletion now
requires all five grammars' Value enums to switch shape simultaneously.
The dedup *cemented* the indirection.

The post-AZ-IV.json bench evidence shows the W5.3 substrate produced
the AU-floor regression — *the dedup itself was the regression cause*.
W6-fat-lto.txt:62-67 names this verbatim.

### Q4 — `Vec<OpenFrame>` — when, alternatives, why current?

**First appearance**: `feffe271 build(tape): TapeBuilder::open_compound +
close_compound (AY.W5.a.2)` (Era 2). Used as a **tape-internal** stack
for the W5 close-stamping experiment.

**First retirement**: `a13840a0 refactor(tape): retire W5-era open_stack
+ note_push + SIB_SKIP_STAMPED_BIT (AY-II.W0.a)`. The AY.W5 experiment
was deleted wholesale due to a -27% twitter regression.

**Re-introduction in user space**: `4f42f6bb feat(runtime): introduce
ValueBuilder + fused Parsed::to_value` (Era 2.5) — the ValueBuilder's
*"frame stack + arena truncate atomically on ValueCheckpoint rollback"*
brings the open-frame discipline back as user-runtime substrate.

**Current incarnation**: `85cf83e7 feat(runtime): land StructBuilder trait
+ JSON value substrate (AZ-I.W2.A)` (Era 4) — `JsonStructBuilder<'p>`'s
`stack: Vec<OpenFrame<'p>>`. From there, `0744c9f9` (Era 7) consolidates
into `SimpleStructBuilder<'p, V, C>` with `Vec<Frame<'p, V>>`.

**Alternatives considered**: the Era 0 `BumpArena<T>` per-type direct
emission *was* the alternative that did not win — because each subsequent
era was authored on a thesis (tape-canonical, builder-canonical) that the
direct emission did not fit.

**Why didn't the alternative win?** Three structural reasons:

1. **Substrate-first discipline**. AB.2a, AC.2, AY-II.W0.c, AZ-I.W2.A,
   AZ-IV.W5.3 — every reform lands the substrate first ("every existing
   test still passes; generated parsers still emit the eager-AST shape")
   and routes consumer activation forward. The direct alternative would
   require lockstep substrate + consumer landing, which the project's
   incremental discipline rejects.
2. **Speculative-rollback semantics**. The combinator-style parser (Era 0)
   was replaced by the recursive descent dispatch tower; the dispatch
   tower's per-byte-literal try-then-rollback discipline (`json.rs:1876-2026`)
   *requires* a Checkpoint primitive on the builder. A direct-projection
   parser would have to either retire speculative parse (predictive
   first-byte dispatch, per DEEP-B Recommendation 1) or accept that direct
   struct construction means partial-write rollback complexity. Neither
   move fit into a tranche cap.
3. **Typed enum dedup invariant**. The W5.3 commit body explicitly states:
   forcing typed enums into a shared shape is *forbidden by the
   empty-return rule* (semantic richness preservation per
   `feedback_preserve_rich_ast`). The dedup of the *indirection* is
   permitted; the deletion of the indirection requires editing every
   typed enum's shape, which the rule forbids.

### Q5 — `from_rule_name` 7-grammar survival; A → B → C migration

**Path A (Era 4)**: `911ee70f`, `6b2f3ca7`, `57e017de`, `ec7a0fa1` —
`from_rule_name(&str) -> Kind` impls land per-grammar (BNF, EBNF,
CSS Pretty, Math, CSV, BBNF, Sheets). The seam is a literal-rule-name
match.

**Path B (AZ-IV.W1.7)**: `0ffbd754 refactor(runtime/discriminator-from-rule-id):
rule-id-keyed kind dispatch + leak_static_str delete (AZ-IV.W1.1+W1.7)`.
The seven `from_rule_name(&str) -> Kind` impls become `from_rule_id(u32)
-> Kind`. The seam moves from string-literal to integer-literal; the
*structure* of the seam (per-grammar match arms) survives.

**Path C (AZ-IV.W4.4 T1)**: `43c313f9 refactor(ir/registry+runtime/arena):
T1 lift compound-kind from rule_id to layout (AZ-IV.W4.4)`. The seven
`from_rule_id(u32) -> Kind` impls become `from_layout(&StructLayout)`,
projecting from the registry. *"Rule ids drift between regen passes;
rule names are the durable contract surface — the registry IS the lookup."*

**Why did the retirement take so long?** Each Path was the *next-best*
move under each tranche's hard-gate priorities:

- **Path A → B (Era 4 → AZ-IV.W1)**: was deferred because
  string-literal-match did not fail any AZ-I/II/III hard gate; CI
  passed; tests passed. The pattern *only became visible* once
  Fermat F1/F2/F3 (the AZ-IV third-hardening-pass grammar-overfit
  audit) cited it explicitly.
- **Path B → C (AZ-IV.W1 → AZ-IV.W4.4)**: was deferred within AZ-IV
  itself because W1 closed scope-revealed at multiple sub-waves (W1.1+W1.7,
  W1.5, W1.9, W1-zero, W1-CLOSE), eating ~6 redress dispatches. T1
  finally landed at W4.4 *only because* the AZ-IV non-routable carry
  framing made it un-deferrable.

**Total elapsed**: ~12 months from Path A to Path C. ~7 grammars × 3
paths = ~21 substrate iterations to retire one indirection seam.

### Q6 — `StructDirect` "wins" but arena survives

**The cutover.O2/O3/O4/O5 series (Era 5)** made StructDirect canonical:
generated view residue purged (`e7306d6d`); `StructDirect` legacy view
emission gated (`94620aaa`); tape offset plumbing removed (`f7ed4c74`);
shape tape branches deleted (`6effcb0b`); inline tape emitters deleted
(`de522995`); wrap tape dispatch deleted (`15bd381a`); the tape crate
deleted (`6a6ca1fd`).

**What "canonical" meant**: the *emission* path is one — emitter calls
`builder.begin_compound(&__layout)` / `builder.end_compound(handle)` /
`builder.push_*` unconditionally. There is no `if grammar.is_struct_direct
{ … } else { … }` switch in the emitter. **The codegen path is one.**

**What "canonical" did NOT mean**: the *runtime mechanism* is direct.
The builder trait IS the indirection; the OpenFrame stack IS the
indirection; the `__layout` runtime literal IS the indirection; the
arena slab IS the indirection. The StructDirect *emission* shape is one;
the StructDirect *runtime* shape inherited every indirection layer the
ValueBuilder + tape had.

The archaeological signature is verbatim across the cutover.O commit
bodies (`e7306d6d`, `f7ed4c74`, `6a6ca1fd`, `6effcb0b`): *"Narrows the
live codebase toward the struct-only projection path."* Every commit
*narrows toward*. None of them *arrives at*.

### Q7 — Profile-driven dismissal of arena indirection

The samply data-points across the project history:

- **AY.W5 → W6 -27% twitter regression** (`a13840a0` body). The data said
  *"this is the bottleneck"*; the response was to delete the experiment.
- **AY-III note (`b346ebca docs(ay): close W6 with recorded misses (AY.W6)`)**
  recorded perf misses but routed forward.
- **AZ-I.W2 close benches**: tape-vs-struct-direct parity gates passed;
  *the runtime cost of the new struct-direct substrate was not separately
  profiled*. Profiles compared `tape` vs `struct-direct` but not
  `struct-direct` against an inline alternative.
- **AZ-II.cutover bench close (`fb46a734 chore(bench/cutover): add O5 close
  target`, `73a79963 fix(az-ii): add cutover bench close target`)**: post-AZ-II
  closes were measured against post-AZ-I, not against AU floor.
- **AZ-III.W4 fat-LTO addition (`[profile.bench-iter]`)**: *"the fat-LTO
  comparison was routed forward to BB.close per AZ-III/FINAL.md:126"*
  (POST-CLOSE-C carry F10). The fat-LTO measurement that surfaces the
  18/19 below-AU regression is *literally deferred forward by one tranche*.
- **AZ-IV.W6.1 (`aa31fe56`)**: the deferred fat-LTO measurement runs.
  The data says *"this is the bottleneck"* with samply's 86.07% inclusive
  attribution to `Vec<OpenFrame>::clone`. The response (FINAL.md): route
  forward to fictional AZ-V (POST-CLOSE-C Meta-1).

**Conclusion**: the data has said *"this is fine"* implicitly through
~5 tranches (AZ-I through AZ-III) by virtue of *not being measured under
fat-LTO* against AU floor. The data has said *"this is the bottleneck"*
explicitly twice: once at AY.W5 (and was retracted), once at AZ-IV.W6.1
(and was routed forward).

### Q8 — Promises that struct projection would land

The phrase counts across all-refs commit messages (case-insensitive
substring search):

- **"direct-to-struct"** — 19 commits (since `e83748d6 docs: update CLAUDE.md
  files with monolithic codegen, @ws, @inline, BumpArena` Era 0; through
  `40092b28` AZ-IV close).
- **"direct projection"** — 11 commits (since `1418fbd3 docs(next-tranche):
  direct-to-struct projection audit (AX.planning)`).
- **"struct projection"** — 4 commits.
- **"projection"** (broad, including `project_types`) — 340 commits.
- **`PROJECTION_DIRECT_TO_STRUCT` const** — first introduced at `456471d3
  feat(emitter): expose PROJECTION_DIRECT_TO_STRUCT as grammar-associated
  const + wire AY.W6.b test gate (AY.W6.b)`. The const enumerates *admitted*
  direct-to-struct rules per grammar; consumers assert on its contents
  as the wire-contract test. The const lands; **the runtime mechanism
  for the listed rules is the same `begin_compound`/`end_compound`
  builder dispatch**.

**Tranches that PROMISED direct projection without delivering** (commit
message + planning doc evidence):

| Tranche | Promise commit | Promise text |
|---|---|---|
| Era 0 | `f419b6d3` | *"direct recursive functions with zero combinator overhead"* |
| AX | `1418fbd3` | *"direct-to-struct projection audit"* |
| AW-IV.W3.5a | `56d66234` | *"wire emit_view_impl to resolve_named_type direct-to-struct projection"* |
| AY.W6.b | `456471d3` | *"PROJECTION_DIRECT_TO_STRUCT as grammar-associated const"* |
| AZ planning | `f9afa87d` | *"direct-to-struct + tape dissolution thesis"* |
| AZ-I | `1c6f00d0`, `41dd776e`, `85cf83e7` | EmitStrategy / dual-emit / StructBuilder substrate |
| AZ-II.cutover | `94620aaa`, `e7306d6d`, `f7ed4c74` | *"narrows toward struct-only projection path"* |
| AZ-III | `5211b953` | trajectory refresh post-AZ-III (tape gone, builder canonical) |
| AZ-IV.W4.4 T1 | `43c313f9` | registry-projected discriminator (the seam moves) |
| AZ-IV.W5.3 | `0744c9f9` | *"structural skeleton dedup"* (the dedup cements indirection) |

Ten tranches. The promise is *iterated*, not *delivered*.

## Per-Era: Did Direct-Projection Land? What Shim Survived?

| Era | Tranche | Did direct-projection land? | Shim that survived |
|---|---|---|---|
| 0 | pre-AA | **Yes (in mechanism)** — BumpArena<T> per-type direct emission | None at era boundary; deleted at AC.2 |
| 1 | AA/AB/AC | No — tape-thesis subsumes direct | tape `Parsed<View>` |
| 2 | AY/AY-II/B5 | No — fused Tape<R> + ValueBuilder | open-frame stack discipline migrates to user-runtime |
| 3 | AZ planning | No (plan only) | rhetoric: *"direct-to-struct + tape dissolution"* |
| 4 | AZ-I.W2 | **Partially** — emission shape is StructDirect | runtime `JsonStructBuilder` + `OpenFrame` + `StructLayout` literal + `from_rule_name` |
| 5 | AZ-II.cutover.O | **In name** — tape deleted, StructDirect canonical | builder trait + open-frame + layout-literal substrate intact |
| 6 | AZ-III | No — terminal close routes carries forward | 32 zero-caller substrates accumulate |
| 7 | AZ-IV (W1.7, W4.4 T1, W5.3) | No — seams move; indirection cemented | `Vec<OpenFrame>::clone` checkpoint at 86.07% inclusive samply |

## The 3000-Commit Answer

**Direct-to-struct has failed to land for nearly 3000 commits because
each major reform (Era 1 tape, Era 4 builder trait, Era 7 dedup
template) re-architected the substrate around the very indirection layer
the GESTALT §2.4 invariant forbids — and each reform was framed in its
own commits as the canonical landing of direct projection.** The Era 0
`BumpArena<T>` direct emission was the closest mechanism in the
project's history; AC.2 deleted it on the tape thesis; the tape-then-
builder-then-template substitution chain has, at every step, preserved
the runtime allocation discipline (per-compound `Vec`, OpenFrame stack,
layout literal) and renamed the shim rather than deleting it.

The structural mechanism enabling this is the **substrate-first / consumer-
forward discipline** crossed with **the no-orthogonal-codepaths invariant**:
substrate lands first ("every existing test still passes"); consumer
activation routes forward; once the substrate is the canonical surface,
deleting it requires editing every consumer's typed shape simultaneously
— which `feedback_preserve_rich_ast` forbids except in lockstep waves
that no tranche cap accommodates. **The indirection is structurally
self-protecting once it is generic.** AZ-IV.W5.3 is the apex: dedup
across five grammars promoted the indirection from per-grammar copy-
paste to one parameterised template, making the *deletion surface*
larger than any single tranche can absorb.

## Top 3 Patterns of Recurrence

### Pattern 1 — Thesis-pivot resets the substrate without retiring the indirection

Era 1 (tape-canonical), Era 4 (builder-canonical), Era 5 (StructDirect-
canonical), Era 7 (dedup-canonical). Each pivot frames its substrate as
*the* direct-projection landing; each pivot inherits the
runtime-stack/arena-slab/layout-literal discipline from the previous
era; each pivot's "canonical" framing makes deletion of the inherited
indirection a *thesis-revision* rather than a *fix*.

### Pattern 2 — Substrate-first discipline preserves the shim by deferring its consumer

`AB.2a body`: *"Lands the substrate pieces … without yet changing
generated code. Every existing test still passes; generated parsers
still emit the eager-AST shape."* This pattern *(land substrate, route
consumer forward)* lets each tranche pass its hard gates while the
indirection survives. POST-CLOSE-C cluster 2 (`Substrate-with-consumer`)
documents 32 zero-caller substrates surfacing at AZ-IV.W5.4 — exactly
the cumulative tail of substrate-first dispatches without consumer
activation. The W5.4 permanent CI test is the project's first attempt
at making this pattern visible at gate-time rather than at audit-time.

### Pattern 3 — Profile-driven evidence retreats faster than it advances

AY.W5 measured the close-stamping regression and *retreated* (deleted
the experiment); AZ-I/II/III did NOT measure under fat-LTO and so the
W5-era arena/builder cost stayed invisible; AZ-III.W4 added fat-LTO
profile *but routed measurement forward to BB.close* (POST-CLOSE-C carry
F10); AZ-IV.W6.1 finally measured and named samply's 86.07% inclusive
on `Vec<OpenFrame>::clone` — and the response was to route to fictional
AZ-V (POST-CLOSE-C Meta-1). The pattern: when measurement says *"this
is the bottleneck"*, the project retreats from the experiment that
caused it (AY.W5) or routes the indirection forward (AZ-IV W6.1 →
AZ-V). When measurement is missing, the indirection grows.

## Recommendations for the Successor (BA / triumvirate)

1. **Lockstep substrate + consumer landing for the BumpArena<T> direct
   emission.** Per DEEP-A Rec 2 + DEEP-B Rec 1, retire the StructBuilder
   trait *and* the OpenFrame stack *and* the per-rule `__layout` literal
   *and* the `JsonArena Vec<Vec<…>>` slab *together* in one wave. The
   `bumpalo` substrate is already a transitive dep (DEEP-A reference to
   `arena.rs:21`). No substrate-first / consumer-forward; both land or
   neither.

2. **Reject "narrows toward" framing in commit bodies.** The cutover.O
   verbatim duplicate text (`e7306d6d`, `f7ed4c74`, `6a6ca1fd`,
   `6effcb0b`) is the archaeological tell — *"narrows the live codebase
   toward the struct-only projection path"* describes a destination
   never reached. Successor commit bodies must name the *deleted*
   indirection by file:line, the *replacing* surface by file:line, and
   the *bench delta* — not the rhetorical destination.

3. **AZ-V does not exist.** Per POST-CLOSE-C Meta-1 + GESTALT line 188:
   *"a non-routable item that cannot land inside AZ-IV without changing
   the AZ-IV thesis triggers a triumvirate scope-reveal review of the
   thesis itself, not a new tranche letter."* The four AZ-IV close-state
   docs that route to AZ-V (FINAL.md:43, FINAL.md:44, FINAL.md:64,
   W6-fat-lto.txt:45, W6-fat-lto.txt:81) constitute a violation of the
   non-routable framing that AZ-IV §Hard Gates 7 + 16 + 21 +
   Non-Routable 7 + 6 explicitly designed against. The successor must
   either close inside an existing letter or trigger thesis-review
   triumvirate; inventing a phantom successor is exactly the move the
   rule forbids.

4. **Profile under fat-LTO at every wave close, not at tranche close.**
   The AZ-III deferral of fat-LTO measurement to BB.close is the
   pattern-3 enabler. Wave-close benches that compare against AU floor
   under fat-LTO would have surfaced the W5.3 regression at W5.3 close,
   not at W6.1 audit. Per `feedback_no-warm-benches` and the AZ-IV §Hard
   Gate 15 floors block, fat-LTO + AU floors must be the wave-close
   gate, not the tranche-close gate.

5. **Generic substrates that absorb the open-frame discipline are the
   highest-risk dedup targets.** The W5.3 commit body explicitly shipped
   the parameterised `SimpleStructBuilder<'p, V, C>` knowing the JSON /
   CSS L4 / Sheets / BBNF outliers could not be absorbed without breaking
   typed-Value parity. *That outlier set is the surface that direct
   projection must retire*. Future "dedup template" PRs whose body
   acknowledges outliers must trigger triumvirate scope review — the
   pattern is the warning sign.

6. **Reject "narrows toward" as a status word in tranche close ledgers.**
   `MET_WITH_MISSES` should not absorb claims of the form "substrate
   exists, consumer routed forward". Per POST-CLOSE-C Meta-3, AZ-IV §Hard
   Gate 13 specified zero-caller substrate as the *close criterion*; the
   permanent CI test (substrate_audit) running red until 32 items
   resolve violates the no-deferral rule. Any wave that lands substrate
   without consumer must be MET_WITH_MISS, not MET; any tranche that
   closes with substrate-without-consumer count > 0 is a process failure.

## Appendix — Key Commit Bibliography

| Era | Hash | Subject |
|---|---|---|
| 0 | `f419b6d3` | feat: monolithic arena codegen, BumpArena, cold-only benchmarks |
| 1 | `094ce8e8` | feat(core): Tranche AB.2a — tape-first emitter substrate |
| 1 | `031cff59` | refactor(rust-backend): delete slab alloc substrate (Tranche AC.2) |
| 2 | `feffe271` | build(tape): TapeBuilder::open_compound + close_compound (AY.W5.a.2) |
| 2 | `a13840a0` | refactor(tape): retire W5-era open_stack + note_push (AY-II.W0.a) |
| 2 | `4f42f6bb` | feat(runtime): introduce ValueBuilder + fused Parsed::to_value (AY-II.W0.c) |
| 2 | `e090f2cb` | feat(tape,core): unify substrate as Tape<R>; delete FusedBuilder (B5.W1) |
| 3 | `f9afa87d` | docs(az): AZ plan — direct-to-struct + tape dissolution thesis |
| 4 | `e0e0af30` | feat(ir): land StructRegistry substrate (AZ-I.W1.A) |
| 4 | `43ea56bb` | feat(ir): project_types populates StructRegistry (AZ-I.W1.A) |
| 4 | `1c6f00d0` | feat(emitter): land EmitStrategy enum + per-grammar resolver (AZ-I.W2.RA) |
| 4 | `41dd776e` | feat(emitter): dual-emit struct-direct bodies (AZ-I.W2.RB) |
| 4 | `85cf83e7` | feat(runtime): land StructBuilder trait + JSON value substrate (AZ-I.W2.A) |
| 4 | `e0388ed8` | feat(runtime/json): JsonDocument accessor API |
| 4 | `192efcd3` | feat(runtime/css_l4): typed-value enum family + arena + builder |
| 4 | `a8657537` | feat(runtime/google_sheets): SheetsValue + arena + builder |
| 4 | `911ee70f` | feat(runtime): bnf + ebnf + css_pretty struct-direct substrates |
| 5 | `94620aaa` | Gate StructDirect legacy view emission |
| 5 | `e7306d6d` | fix(emitter/struct-direct): purge generated view residue (cutover.O3) |
| 5 | `f7ed4c74` | fix(emitter/struct-direct): remove tape offset plumbing (cutover.O4) |
| 5 | `6a6ca1fd` | fix(runtime/tape): delete tape crate |
| 5 | `43f0795b` | docs(AZ-II): cutover.M Phase 3 close — 8/9 fleet StructDirect |
| 6 | `c3d4e029` | docs(az-iii.final): convert FINAL.md to terminal close |
| 7 | `0ffbd754` | refactor(runtime/discriminator-from-rule-id) (AZ-IV.W1.7) |
| 7 | `43c313f9` | refactor(ir/registry+runtime/arena): T1 lift compound-kind (AZ-IV.W4.4) |
| 7 | `0744c9f9` | refactor(runtime/arena-builder-template) dedup (AZ-IV.W5.3) |
| 7 | `aa31fe56` | benchmarks(post-az-iv-matrix) — fat-LTO close matrix |
| 7 | `078c8276` | docs(az-iv/audit/deep-A-assay): trace struct-projection vestigial path |
| 7 | `d4b8de18` | docs(az-iv/audit/deep-B-profile): land single-attribution profile |
| 7 | `df33e160` | docs(az-iv/audit/deep-C-pathforward): direct-projection deep architectural plan |
| 7 | `40092b28` | docs(az-iv/audit/deep-synthesis): canonical-ordering + direct-projection synthesis |
