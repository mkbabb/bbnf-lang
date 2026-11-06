# R3 — The greenfield-from-skinny delineation: per-subsystem GREENFIELD vs CARRY vs PORT-FROM-LEGACY

Charter: SK-V23 PASS-ALPHA §3 ("Greenfield-from-skinny, delineated honestly"). Skinny won the
SK-V21 adjudications and was ratified by SK-V22 as the tree of record, but the user's SK-V23 verdict
withdraws blanket ratification: skinny is the SEED, not the ship. Four named extant problems drive
greenfield — hardcoding residue (the WART-C two-site name-gate and kin), NO type system (monotone
fold only), no proper SIMD support (dead/partial kernels), poor Value API (prototype-grade sink).
This audit reads the live skinny workspace at `/Users/mkbabb/Programming/bbnf-lang/skinny` and
assigns each of the ten subsystems a disposition, then cross-checks SK-V22 research/02 (the
graduation audit that said "carry-and-fix") so this SHARPENS rather than contradicts it.

**The census (live, from `wc -l crates/*/src`).** bbnf-bench 35,933 · codegen 10,201 · runtime
6,522 · parse-that-regex 3,022 · passes 2,750 · bbnf-simd 2,324 · grammar 2,142 · ir 1,020 ·
test-fixtures 404 · bbnf-regex 322 (stub) · bbnf 111. `crates/simd-scan/src/` is EMPTY. Edition
2021. 32 files carry inline `#[cfg(test)]` (17 in bbnf-bench alone), i.e. the ~75 inline-test blocks
the charter cites. `[workspace.metadata.bbnf.grammars]` declares ONLY json; sheets + css_l4 sources
still reach into legacy `grammar/`.

**The dispositions in one line.** GREENFIELD: the generator/emit spine (§2), the value plane (§4),
the type plane (§6), the SIMD dispatch/cost-table (§5). CARRY-WITH-FIXES: the frontend (§1), the
runtime tape (§3), the decision engine (§7), xtask/regen (§8), tests (§9). PORT-FROM-LEGACY: the
grammar `.bbnf` corpus + egraph + csp-solver (feed §1/§7/§8). REWRITE-SHRINK: the bench harness (§10).

---

## The consumer-invariant gate, defined once (Y.13, applied per subsystem)

Every subsystem below carries a gate. For GREENFIELD subsystems the gate is a **differential
oracle**: the frozen skinny output at HEAD is the reference, and the fresh implementation must
produce byte-identical observable output for its consumer across the whole corpus before the old
code is deleted. Skinny was frozen as `sk-v19-frozen` precisely as this oracle (research/02 §3, W0.0)
— the greenfield verdict USES that freeze, it does not waste it. For CARRY subsystems the gate is the
existing Lock falsifier re-homed onto skinny paths. No subsystem's old code is deleted until its
gate is green on the graduation baseline (W0.11 load-accounted, not the disingenuous warm figure).

---

## §1 — Frontend (grammar + IR): CARRY-WITH-FIXES

- **Live**: `crates/grammar/src/lib.rs` (2,142, single file) + `crates/ir/src/lib.rs` (678) +
  `crates/ir/src/cost.rs` (342). The bbnf parser, `GrammarIr`, `resolve_refs`/`validate_resolved_refs`
  (ir/lib.rs:84-111,300).
- **Verdict**: CARRY. The parser is the adjudicated frontend; it is not implicated by any of the four
  named problems. Two named fixes: (a) `grammar/src/lib.rs` is a 2,142-LOC single file — a god module
  in gestation (no-god-modules precept) — split into `grammar/src/{parser,ast,error}.rs`; (b) it
  carries inline `#[cfg(test)]` (part of the 32) to relocate to `tests/`.
- **Greenfield seam it must OPEN**: today the grammar carries type annotations only as opaque text —
  `RuntimeTypedProjection.type_text/type_offset/type_end` (codegen/src/grammar_provider.rs:200-202)
  is a byte-slice, never a parsed type. The frontend must grow a real `->` type-expression parse so
  the §6 type plane has an AST to consume. That is additive to the parser, not a rewrite of it.
- **PORT-FROM-LEGACY dependency**: the `.bbnf` grammar SOURCES (sheets, css/l4 ×15, and the
  self-hosting bbnf/bnf/ebnf/csv/toml/math corpus) must MOVE into `skinny/grammars/` — today only
  `grammars/json.bbnf` lives in-tree (Cargo.toml:61); sheets/css sources reach into legacy `grammar/`
  (research/02 §G4). Pure data move, blocks all regen once legacy archives.
- **Gate**: parse every corpus `.bbnf` to a `GrammarIr` byte-identical to the frozen skinny parse
  (differential on the AST debug-dump), plus the ref-resolution Lock.
- **Cross-check vs research/02**: research/02 §G4/§2 already said PORT-IN the sources and treated the
  frontend as adjudicated-and-kept. Greenfield verdict does NOT change the call — it only ADDS the
  type-annotation parse seam that §6 now requires. SHARPENS, no contradiction.

## §2 — The generator / general_descent: GREENFIELD (the emit spine, re-founded as ONE path)

- **Live**: `crates/codegen` (10,201). The emit spine is `general_descent/{emit.rs 452, mod.rs 93,
  regex_first.rs 230, view.rs 431}` (1,206) PLUS the parallel JSON fork
  `json_sink_direct/` + `json_templates/{generated,parser,value,view,visitor,config}.rs`.
- **The extant problems (all three walls, live)**:
  - **WART-C is a TWO-SITE name-gate, both confirmed live.** Site 1:
    `runtime_generator.rs:98` — `if !sink_only.has_rule("value") { return emit_general_descent(...) }`
    routes any grammar OWNING a rule literally named `value` to the verbatim json_templates path,
    everything else to general_descent. Site 2: `json_sink_direct/mod.rs:98` —
    `.find(|rule| rule.name == "value")` re-couples on the same name INSIDE the sink. Re-keying on
    `has_rule` (commits 36ca10646/f4633d468) made site 1 structural, but **the fork itself survives**:
    two emit paths, two maintenance surfaces, and a grammar that happens to name a rule `value` still
    diverges. This is the hardcoding residue the charter names.
  - **WALL A (whitespace hardcode), live.** `general_descent/emit.rs:126-128` emits `skip_ws` as
    unconditional `byte.is_ascii_whitespace()`; no `?w`-derived `WsPolicy`. Newline-significant
    grammars (TOML) mis-parse.
  - **WALL B (leaf-regex approximation), live.** `general_descent/regex_first.rs` (230) emits a
    delimiter-scan `match_leaf` approximation, not a bounded regex interpreter; `bbnf-regex` is a
    322-LOC stub with zero matcher symbols. RFC3339 datetimes, escaped/multiline strings, `[\s\S]*?`
    break.
- **Why GREENFIELD not carry-and-fix**: PROTO-B1 proved the walls dissolvable, but it patched the
  existing paths in a throwaway worktree. Carrying that patch preserves the two-path shape — the fork
  can regrow the instant a new grammar looks JSON-shaped. The user's "hardcoding remains" verdict is
  a verdict on the ARCHITECTURE: there must be ONE emit path, parameterized by `WsPolicy` + a real
  regex interpreter + grammar-derived projections, with json_templates/ + json_sink_direct DELETED.
  Re-founding the spine (not patching two branches) is the only way the fork cannot reappear.
- **LOC**: greenfield the unified emitter ≈ 1,800-2,200 LOC (general_descent's 1,206 re-authored +
  the bounded regex interpreter ~400 + `WsPolicy` threading ~150). DELETE ≈ 1,500+ LOC of
  json_templates/ + json_sink_direct fork. Net codegen shrinks.
- **Gate**: the JSON grammar, emitted through the ONE path, produces a parser byte-identical on tape
  output to the frozen json_templates parser across the full JSON corpus (the differential oracle is
  the very code being deleted). THEN delete the fork. No number moves until byte-parity holds.
- **Cross-check vs research/02**: research/02 §G1 framed this as "regen-wiring, not research —
  land PROTO-B1." SK-V22 SPEC §GW2a framed it as "the WART-C fold close." The greenfield verdict
  DOES change the depth of the call: research/02 said WIRE the proven patch; SK-V23 says RE-AUTHOR
  the spine so the two-path shape is structurally impossible, not merely bypassed. This is a
  sharpening — same target ("one emit path serves every grammar"), deeper mechanism (delete-and-refound
  vs patch-and-bypass). It does not contradict research/02; it raises the bar research/02 set.

## §3 — The runtime tape: CARRY (largely already done)

- **Live**: `crates/runtime/src/tape/{mod.rs, assembler.rs, cursor.rs 36, offsets.rs}` — the single
  AoS `Vec<TapeRec>` substrate, the SK-V21-adjudicated shape (SK-V21 SPEC:232-247, AoS beats 7-column
  SoA).
- **Verdict**: CARRY. Not implicated by the four problems; the AoS decision is settled evidence, not
  a wart. Fix: relocate inline tests; confirm the single-writer `frame_depth` invariant + Lock-1
  falsifier (research/02 §TW1b: "collapses to a gate, not a workload").
- **Gate**: Lock-1 (zero payload writes/allocs in the runtime path) on skinny's tape, re-homed.
- **Cross-check vs research/02**: research/02 §TW1b says "largely already done, mostly a gate." The
  greenfield verdict does NOT change this — the tape is the one substrate the four problems leave
  untouched. Full agreement.

## §4 — The value plane / sink: GREENFIELD (the poor Value API, re-founded grammar-neutral)

- **Live**: the fused sink exists ONLY as a JSON-owned trait — `runtime/src/grammars/json/sink.rs`
  (120, `JsonSink`), with the lazy lane in `json/view.rs` (488) + `json/value.rs` (172) over
  `tape/cursor.rs` (36). Sheets has `view.rs` (279) but NO sink; css_l4 has `sink.rs` (16 — a husk).
  Every value surface is PER-GRAMMAR and generated (`GoogleSheetsKind` enum "one variant per grammar
  rule, no shared structural vocabulary" — sheets/view.rs header).
- **The extant problem**: this is the "poor Value API, prototype-grade sink, no unified path/lane
  surface" the charter names. There is no grammar-neutral value plane, no serde bridge, no
  `StructRegistry` projection fence, no projection-totality invariant, no CSS depth closure / Unparsed
  escape hatch (research/02 §G2).
- **Why GREENFIELD**: the JSON sink was written to prove the fused-sink thesis cold (SK-V21), not to
  be the plane. Generalizing three divergent per-grammar view/value/sink files into a neutral
  path+lane+sink surface is a re-founding, not a patch — the shared structural vocabulary the sheets
  header explicitly disclaims having is the entire deliverable. This is also where the legacy
  `bbnf-path` cursor alphabet (918 LOC) is REBUILT natively rather than ported (research/02 §2:
  "rebuild the cursor surface inside runtime at TW3").
- **LOC**: greenfield ≈ 1,200-1,600 LOC (neutral `Path`/`Cursor`/`Sink`/lazy-`View` + serde bridge +
  projection fence), retiring ~1,059 LOC of per-grammar json/sheets/css view/value/sink. The fused
  eager sink remains the DEFAULT (SK-V21:279 close condition).
- **Gate**: the projection-totality invariant made executable —
  `PROJECTION_DIRECT_TO_STRUCT.len() == materializers == consumers` (DEFERRED-LEDGER W4) — plus the
  fused-sink output byte-identical to the frozen `JsonSink` on the JSON corpus, and the lazy view
  cursor-walk identical to `json/view.rs`. Differential oracle = the per-grammar files being retired.
- **Cross-check vs research/02**: research/02 §G2/§TW3 already said "SURVIVES UNCHANGED in scope — the
  single largest genuine forward workload; skinny has the JSON prototype only." The greenfield verdict
  AGREES and names it explicitly: research/02 called it "generalizes the prototype," SK-V23 calls it
  greenfield — same work, the label now matches the reality. No contradiction; the "poor Value API"
  verdict simply ratifies research/02's own framing that the plane is future work, not extant.

## §5 — The kernel / SIMD plane: CARRY the aarch64 primitives, GREENFIELD the dispatch + cost-table + SOTA uplift; DELETE x86

- **Live**: `crates/bbnf-simd` (2,324) carries a real aarch64 NEON family — `structural_classify_64`
  (155), `bracket_depth_mask_64` (107), `comment_body_mask_64` (145), `unescape_uxxxx` (214),
  `classify_tbl4` (104), `match_tiny_plain_string` (136), `byte_class_from_eq_set_64` (87),
  `string_block` (72), `digit_mac` (71) — each with a scalar fallback. `dispatch.rs` (113) is the
  selection surface. `crates/simd-scan/src/` is EMPTY (dead stub).
- **The extant problem ("no proper SIMD, dead/partial kernels")**: the primitive BODIES are proven
  (SK-V21's win), but the CONSUMPTION is partial. `dispatch.rs` wires only `SelectedClassifier`
  (classify_chunk) + a `PrimitiveKernels` struct of FIVE fn-pointers (byte_class_from_table_64,
  bitmap_prefix_xor_64, bitmap_next_set_bit, bulk_emit_positions_64, eob_pad_clamp — dispatch.rs:51-85).
  The ~15 aarch64 kernel bodies vastly outnumber the ~6 the dispatch ladder actually selects — the
  rest are reachable only by direct call or are dead. There is NO `KernelCostTable`/`KernelShape`
  (research/02 §G3: grep-absent). And the SOTA NEON leverage the charter mandates — the vshrn idiom
  family (SK-V21:44,319 confirms `vshrn` beats hand-asm ~20-28%; already skinny-side in
  `movemask.rs`), multi-block pipelining, shufti/truffle/Teddy-class prefilters, vectorized DFA stages
  — is unbuilt.
- **Verdict**: SPLIT. CARRY the proven aarch64 primitive bodies (they ARE the SK-V21 win; deleting
  them would refute settled evidence). GREENFIELD (a) the dispatch surface into a pluggable
  `KernelCostTable` so kernel selection is data, not a hardcoded 6-fn struct (pluggable-components
  precept), and (b) the SOTA-NEON uplift layer. DELETE x86 wholesale per charter §1: the empty
  simd-scan stub, and the x86 references that survive as cfg-guards/target-cpu strings in
  bbnf-bench (json_parity.rs, report.rs, gate.rs, lock14_baseline.rs), codegen/lib.rs, passes/lib.rs
  — plus an enforcement design (cfg-deny / CI guard) so x86 cannot regrow.
- **LOC**: CARRY ≈ 1,600 LOC of aarch64 primitive bodies. GREENFIELD ≈ 400-600 LOC (cost-table
  substrate + dispatch ladder + SOTA-NEON kernels). DELETE the empty stub + all x86 guard sites.
- **Gate**: per-kernel differential (SIMD output == scalar fallback, the checkasm-liveness discipline,
  charter §2/dav1d) on every consumed kernel; a grep-zero x86 falsifier that fails the build if any
  x86 cfg/intrinsic reappears; cost-table selection must never regress cold throughput vs the frozen
  hardcoded dispatch (W0.11 baseline, PMU-accounted).
- **Cross-check vs research/02**: research/02 §G3 called the kernels "the SK-V21 win, in-tree" and
  cost-table "genuinely absent." The user's "no proper SIMD, dead NEON" verdict PARTIALLY CHANGES the
  framing: research/02 read the plane as done-and-kept; SK-V23 reads it as proven-but-under-leveraged.
  Reconciled: CARRY the primitive correctness research/02 rightly praised, GREENFIELD the selection +
  SOTA layer research/02 admitted was absent, and ADD the x86 abrogation research/02 (pre-charter)
  never scoped. Sharpens by separating "kernel body" (carry) from "kernel plane" (greenfield).

## §6 — The type plane: GREENFIELD (there is no type system to carry)

- **Live**: NOTHING to carry. Grep for `monotone`/`infer`/`unify`/`Hindley`/`TypeDesc` across
  `ir/src` and `passes/src` returns zero type-inference machinery. Types survive only as opaque text:
  `RuntimeTypedProjection { type_text, type_offset, type_end }` (grammar_provider.rs:200-202) captures
  the `->` annotation as a byte-slice; `lower/tape_plan.rs` carries NO width/type fold (grep-empty for
  width/monotone/widen). What exists is a monotone tape-width fold at lowering, not a type system.
- **The extant problem ("NO type system, monotone fold only")**: the charter's most literal verdict.
  There is no type representation, no inference, no unification, no checking — a grammar's `->`
  arrows are threaded to the emitter as text and materialized structurally. Backend-agnostic types
  (TypeDesc::Named, per the backend-agnostic-types precept) do not exist here.
- **Why GREENFIELD**: you cannot carry-and-fix a subsystem that is absent. This is net-new: a real
  type plane — abstract `TypeDesc` (Named/Aggregate/Scalar), a checker over the frontend's `->`
  type-expression AST (§1's new seam), the typed-materialization-invariant made real (every `->`
  reaches the tape emitter; inference composes types, never loses them — the precept). Whether
  full bidirectional/HM or a constraint solver over the existing egraph is a design question for the
  plan, but "monotone fold" is not a foundation to extend.
- **LOC**: net-new ≈ 800-1,200 LOC (type AST consumption + `TypeDesc` + checker + the composition
  rules), landing on `grammar`/`ir` per research/02 §TW5.
- **Gate**: the typed-materialization-invariant as an executable falsifier — the count of `->` arrows
  in every corpus grammar equals the count reaching the tape emitter; type-parity = full typed-AST
  equivalence on the differential oracle (the frozen text-threaded projections must remain a subset of
  what the real plane derives, never a superset it loses).
- **Cross-check vs research/02 — THE SHARPEST DIVERGENCE.** SK-V22 (SPEC §TW5, via PROTO-B3) framed
  the type work as a "width patch" — extend the monotone fold. The user's "NO type system" verdict
  CHANGES that call: not a patch to the fold, but a real type plane built fresh. This is the one place
  the greenfield verdict genuinely OVERRIDES a SK-V22 carry-and-fix disposition rather than merely
  deepening it. research/02 undersized TW5 by treating the absent type system as an extendable fold;
  SK-V23 correctly sizes it as greenfield. Flagged for the tranche codex as a scope increase on TW5.

## §7 — The decision engine (passes): CARRY-WITH-PRUNE

- **Live**: `crates/passes` (2,750) = `lib.rs` (2,096, god module) + `backend_egraph.rs` (311) +
  `decision_csp.rs` (278) + `diagnostics.rs` (65). Consumes egraph + csp-solver via `../crates` path.
- **Verdict**: CARRY. Not implicated by the four problems. Named fixes: (a) `lib.rs` is a 2,096-LOC
  god module — split by concern (no-god-modules precept); (b) RETIRE `decision_csp.rs` — the
  tautological CSP decision stage marked DELETE (research/02 §2, addendum §1:18); one `propagate()`
  engine over the egraph (unified-propagate precept), not egraph-plus-csp; (c) relocate inline tests.
- **PORT-FROM-LEGACY dependency**: egraph + egraph-derive (2,228) VENDOR IN; csp-solver (7,393)
  VENDOR then PRUNE the decision stage (research/02 §2 critical path). General-infra crates keep their
  own home (general-infra-crates precept) — vendor as a sub-tree, not stuffed into passes.
- **Gate**: decision output identical on the corpus with `decision_csp` removed (proving it was
  tautological); Lock-4 re-vendor checkpoint.
- **Cross-check vs research/02**: research/02 §TW4.2 says "one engine over passes, retire
  decision_csp, scanner unification." Greenfield verdict does NOT change the call — the engine is
  sound, the CSP stage is dead weight already slated for deletion. Full agreement.

## §8 — xtask / regen: CARRY-WITH-FIXES (kill the per-grammar fork)

- **Live**: `skinny/xtask` (~5,401, research/02 §G5) — a 2,780-LOC `main.rs` + `real_typed_schema.rs`
  (1,014) + `skv15_w0.rs` (729) + `regen.rs` + `regen_css.rs`. Each grammar has its own hardcoded
  const roster (`JSON_SOURCES`, `SHEETS_SOURCES` in main.rs; a `regen_css` TARGET table; a hand-carried
  sheets schema in real_typed_schema.rs). `[workspace.metadata.bbnf.grammars]` declares ONLY json
  (Cargo.toml:60).
- **Verdict**: CARRY the xtask crate, but GREENFIELD-REBUILD the regen host as ONE metadata-driven
  generator — this IS B2's per-grammar fork (research/02 §G5), the same pathology as WART-C one layer
  up. Every grammar declared in `[workspace.metadata.bbnf.grammars]`, driven through the single §2
  emit path; the hardcoded const rosters + real_typed_schema.rs hand-schema DELETE.
- **LOC**: rebuild the regen host ≈ 600-900 LOC metadata-driven; DELETE ≈ 2,500+ LOC of per-grammar
  fork (const rosters, hand schemas, per-grammar entry points).
- **Gate**: `cargo xtask regen` from a clean tree reproduces every generated grammar dir byte-identical
  to the committed output (clean-regen-discipline precept: generated files are always fresh regen).
- **Cross-check vs research/02**: research/02 §G5 named the xtask fork blocker #3 and prescribed
  exactly this metadata-driven rebuild. Greenfield verdict AGREES and hardens: the fork is not
  fixable in place any more than WART-C is — same delete-and-refound mechanism. Sharpens by tying
  xtask's fork to §2's fork as one architectural sin.

## §9 — Tests / oracles: CARRY (relocate) + one greenfield oracle harness

- **Live**: 32 files carry inline `#[cfg(test)]` under `crates/**/src/` (17 in bbnf-bench) — the
  ~75 inline-test blocks. The differential oracle IS the frozen skinny tree (`sk-v19-frozen`).
- **Verdict**: CARRY the tests, RELOCATE all 32 files' blocks to `tests/` (no-inline-tests precept,
  research/02 §G7). GREENFIELD one thing: the differential-oracle harness that every greenfield
  subsystem above depends on — a corpus-driven byte-parity runner comparing fresh-impl output to the
  frozen reference. It does not exist as a first-class harness today; each greenfield gate needs it.
- **LOC**: mechanical relocation (no net LOC); greenfield oracle harness ≈ 300-500 LOC in a
  `tests/`-level crate.
- **Gate**: grep-zero for `#[cfg(test)]` under `src/`; the oracle harness green against frozen skinny.
- **Cross-check vs research/02**: research/02 §G7 counted 32 files, prescribed relocation. Greenfield
  verdict ADDS the oracle harness (implicit in research/02's "differential oracle" framing, now made a
  named deliverable because four subsystems depend on it). Sharpens.

## §10 — The bench harness: REWRITE-SHRINK (not carry, not greenfield-from-scratch)

- **Live**: `crates/bbnf-bench` (35,933 — the largest crate by 3.5×) carries generated bench payloads
  + 17 inline-test files + lock14_baseline + gate.rs + report.rs (which reference x86 target-cpu
  strings to abrogate).
- **Verdict**: REWRITE-SHRINK. The harness is 55% of the whole workspace's src and most of it is
  generated payload, not harness logic. It is not "carry" (it is bloated and carries x86 + inline
  tests) and not "greenfield" (the SK-V21/W0.11 profiling protocol — PMU + paired interleaving +
  multi-window + per-block loadavg, cold-only — is settled and must be preserved verbatim). Shrink:
  regenerate payloads as fresh xtask output (not committed bulk), relocate 17 inline-test files,
  strip x86 from json_parity.rs/report.rs/gate.rs/lock14_baseline.rs, re-home Lock-14 onto skinny's
  actual crate dirs (research/02 §TW0-W0.3: lock14_baseline.rs:1287 already points at
  `crates/codegen/src/general_descent/`).
- **LOC**: harness logic ≈ 3,000-5,000 LOC survives; ≈ 30K of generated payload de-committed to
  regen-on-demand.
- **Gate**: the W0.11 load-accounted cold baseline reproduces within the load-robust ratio band;
  no warm/cached figures (no-warm-benches precept).
- **Cross-check vs research/02**: research/02 counted bbnf-bench at 35,933 and flagged its 18 inline
  tests but did not prescribe a shrink. Greenfield verdict ADDS the payload-de-commit + x86 strip;
  the profiling protocol itself is untouched. Sharpens by naming the harness as its own disposition
  rather than leaving it inside the generic inline-test/x86 sweeps.

---

## Summary table

| Subsystem | Verdict | Live LOC | Greenfield/net LOC | Driving problem | Changes research/02? |
|---|---|---:|---:|---|---|
| §1 Frontend (grammar+IR) | CARRY+FIX +PORT sources | 3,162 | +type-annot parse seam | god-file split | No — adds §6 seam |
| §2 Generator/emit spine | **GREENFIELD** | 10,201 (spine 1,206+fork) | ~2,000 new / −1,500 del | WART-C 2-site gate + WALL A/B | Deepens (refound vs patch) |
| §3 Runtime tape | CARRY | ~400 | gate only | none (AoS settled) | No — agrees |
| §4 Value plane/sink | **GREENFIELD** | ~1,059 per-grammar | ~1,400 new | poor Value API | No — ratifies TW3 |
| §5 Kernel/SIMD plane | CARRY bodies / **GREENFIELD** dispatch / DELETE x86 | 2,324 | ~500 new / −x86 | dead/partial NEON, no cost-table | Partial — split body vs plane |
| §6 Type plane | **GREENFIELD (net-new)** | 0 | ~1,000 new | NO type system | **YES — overrides "width patch"** |
| §7 Decision engine | CARRY+PRUNE +PORT egraph/csp | 2,750 | −278 (csp) | god-file, dead CSP stage | No — agrees |
| §8 xtask/regen | CARRY+REBUILD host | 5,401 | ~750 new / −2,500 del | per-grammar fork (B2) | Hardens |
| §9 Tests/oracles | CARRY relocate + GREENFIELD harness | 32 files | ~400 new | inline tests | Adds harness |
| §10 Bench harness | REWRITE-SHRINK | 35,933 | ~4,000 survives / −30K payload | bloat + x86 | Adds shrink |

**The load-bearing greenfield work, ranked**: §2 emit spine (the fork that keeps regrowing) → §6 type
plane (absent, and the one true override of SK-V22) → §4 value plane (the poor API) → §5 SIMD
dispatch+SOTA (the under-leveraged win). Everything else carries, ports, or shrinks. The delineation
SHARPENS research/02 on nine of ten subsystems and OVERRIDES it on exactly one: the type plane, which
SK-V22 undersized as a monotone-fold width patch and the user's verdict correctly sizes as greenfield.
