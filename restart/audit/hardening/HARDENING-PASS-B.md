# Hardening — Pass B (Codegen + Runtime + Optimisers)

Date: 2026-05-03
Hardener: HARDENING.md target=PASS-B
Target: `restart/audit/passes/PASS-B.md` (Pass-B synthesis @ 548 lines)
Per-agent corpus: `pass-b-agent-{1..6}-*.md` (2,892 lines)
Authoritative override consulted: `restart/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md`
Time budget: 45 min hard cap.

---

## §1 — Target identification

Pass B covers `crates/core/src/backend/` (119 files, ~22 K LOC), `crates/core/src/runtime/` (75 files, ~12 K LOC), `crates/core/src/pipeline/` + `pipeline.rs` (9 files, ~1.5 K LOC), `crates/core/src/grammar/generated/` (10 files, 168,750 LOC), the optimiser sister crates (`egraph`, `egraph-derive`, `csp-solver`, `simd-scan`), and `xtask/`.

Verdict ledger from PASS-B.md §1.g (290 files total):

| Bucket | Count | PASS-B claim |
|---|---:|---|
| KEEP-OUTRIGHT | 11 | sister-crate stable surfaces |
| KEEP-MODIFY | ~120 | move + scrub |
| ABROGATE-DELETE | ~6 | path duplicate + parse_with legacy + substrate.rs |
| ABROGATE-MOVE | ~9 | the generated/ tree |
| ABROGATE-REPLACE | ~140 | per-grammar runtime + struct_direct sub-modules + Emitter trait reshape |

The synthesis frames the ABROGATE-REPLACE bucket as "Lock-14 demand mass" and proposes a single architectural pivot: **per-grammar declaration crates × 9 + template-emitted runtimes + direct-projection emit + reshaped Emitter trait**. The first conjunct — per-grammar declaration crates × 9 — is the surface Amendment 01 retracts. The remaining conjuncts (template-emit, direct-projection, Emitter reshape) survive amendment unchanged.

The substantive Pass-B mass — codegen extraction, runtime substrate split, OpenFrame retiral, struct_direct collapse, regen god-module split, Emitter coarsening — is correct in shape; the per-grammar-crate language is the locus that requires surgery.

---

## §2 — Cohort verdict

| Lane | Verdict | Faults | Recommendation |
|---|---|---:|---|
| 1 Lock-Adherence | partial | 4 | Strengthen Lock 1 OpenFrame stance; clarify Lock 14 surface; tighten Lock 9 escape-hatch language |
| 2 Sequencing Discipline | n/a (single-pass) | — | Dependency-arrow §13 §5.2 honoured; sequencing scrutiny defers to MASTER-PLAN target |
| 3 Cohesion | partial | 3 | Pass-B retains synthesised claims unsupported by the per-agent ledger (e.g., 86.07% samply share retirement is asserted, not derived) |
| 4 SOTA Anchoring | violated | 5 | Every Pass-B perf claim ("substrate-side: OpenFrame is the blocker" etc.) must cite sonic-rs / simdjson / lightning-css numbers — five gates are silent |
| 5 Grammar-Authoritative (Lock 14) | violated-with-amendment | 18 | PASS-B contains 18 sites naming `crates/<grammar>/` × 9 declaration crates; under Amendment 01 these resolve to `bbnf-runtime/src/grammars/<name>/` + `bbnf-host-prims/`. Per-X tables required for "all grammars" claims. |
| 6 Generated-Code Budget | partial | 4 | LOC distribution table at §1.d (168,750 across 9 grammars) is correct baseline; missing per-wave budget projection for OpenFrame retirement net |
| 7 Friction Forecast | silent (must add) | 6 | Zero friction-surface enumeration; no error-message commits for layout-lowering / Pratt-misfire / arena-default surprises |
| 8 Carry & Deferral | partial | 7 | "Pass-A coordination" §5.a names items but receivers are vague; "Pass-C coordination" §5.b doesn't name receiving gates |
| 9 Greenfield Discipline | partial | 3 | OpenFrame retirement is correctly named as architectural transposition; substrate.rs delete + struct_direct collapse honoured; per-grammar crate proliferation is the failure of greenfield (overfitting); resolved by Amendment 01 |

Final decision: **requires amendments**.

The Pass-B substance (the 19-item punch list at §7) is execution-ready in *shape* — the surgeries name target paths, the dependency arrow at §13 sequences correctly, the architectural transpositions ratify the right pivots. What requires amendment is *language*: every "per-grammar declaration crate" reference re-anchors to template-emitted subdirectory under `bbnf-runtime/src/grammars/<name>/` or to `bbnf-host-prims/` per Amendment 01; SOTA gates name competitor-dataset-platform; friction surfaces enumerate; carries name receivers + blockers + gates.

The pivot itself — Lock 1 + Lock 13 + Lock 14 retire together via template-emit + direct-projection + Emitter coarsening — survives the amendment in full. The 9-per-grammar-crate proliferation was overfitting on the Lock-14 escape hatch; the corrected substrate (24-member workspace, zero per-grammar crates) honours the lock by *construction*, not by escape.

---

## §3 — Lane 1 — Lock-Adherence

Standard: walk the 14 locks. Per-lock verdict + cite.

### Lock 1 — Tape and columnar dead

**Verdict: partial.**

PASS-B §6.a row 1 honest: "honoured at production-symbol level; ~50 doc residue sites; OpenFrame is the substantive question". Agent B.3 §Lock 1 cites the question explicitly: "*does `OpenFrame` count as 'tape rebranded'? The honest answer: yes in spirit.*" Agent B.6 §4.2 verifies non-completeness: "OpenFrame residue **does** remain in Pass-B scope across 6 files + 109 mentions. Phase-4 BA option-(a) has *not* landed across all 9 grammars; today's substrate is OpenFrame-bearing."

PASS-B §2.b states "the template's emission uses **direct-projection** — no OpenFrame heap-stack" and §8.2 confirms "no heap-stack of OpenFrame; no `StructBuilder` trait ceremony; no `<G>StructCheckpoint` Vec-clone. Lock 1 honoured by mechanism, not just by symbol-naming."

**Fault L1.1**: PASS-B §6.a row 1 ("OpenFrame is the substantive question") and §4.b ("regen-clean scrub") leaves the OpenFrame question *open*. Lock 1 strict reading per `restart/locks/14-LOCKS.md` line 34: "*no 'tape rebranded as fast-path'. The substrate IS the typed-enum + slice-borrow.*" Surgery: PASS-B must declare verbatim — *OpenFrame is tape rebranded; OpenFrame retires by mechanism, not negotiation.* The §4.c "OpenFrame migration completeness gate" is the right framing; the §6.a row 1 wording must align (replace "is the substantive question" with "is tape rebranded; retires via direct-projection emit").

**Fault L1.2**: PASS-B §1.b row 5 ("`runtime/{<g>}/parse_with.rs` (4 files; bbnf, json, css_l4, google_sheets; ~480 LOC) | ABROGATE-DELETE | typed-path → legacy-path lowering retires when `runtime/path.rs` retires"). The legacy-path lowering retires correctly; but PASS-B §7 surgery 9 ("Direct-projection emit") doesn't extend the negative-assertion gate at `crates/core/tests/struct_direct_snapshots.rs:45-53` to also assert `OpenFrame` absence. PASS-B §7 surgery 14 *does* extend the gate ("extends to assert `OpenFrame`, `<G>StructBuilder`, `<G>StructCheckpoint` absence"). Surgery 9 must explicitly name surgery 14 as its gate.

### Lock 2 — Layout lowering canonical name

**Verdict: out-of-scope (Pass A).** PASS-B §6.a row 2 marks "out of Pass-B; Pass-A scope". §5.a item 2 ("Layout lowering pass naming convergence per Lock 2 — Pass-A scope") names it as Pass-A coordination. The cross-cut to Pass-B per Agent B.6 §6.1 invariant 2 ("`<G>Document: <G>PathQuery`") implicitly depends on Layout lowering pass output. PASS-B §3 doesn't name a *receiving Pass-B gate* for the Layout output; surgery is to add one (covered in Lane 8).

### Lock 3 — Cursor-parse + byte-skip unified

**Verdict: honoured structurally; cursor consult on eager wasted (the documented gap).** PASS-B §6.a row 3 + Agent B.3 §Lock 3 both cite `generated/json.rs:3443-3448` — eager `__EAGER_EMPTY_PATH` constructed but cursor calls *not* constant-folded. PASS-B §6.a row 3 surgery: "constant-fold cursor on EMPTY_PATH binding". Honoured.

### Lock 4 — Per-domain orthogonal optimisation

**Verdict: honoured.** PASS-B §6.a row 4 + Agent B.3 §Lock 4 cite `crates/egraph/src/csp_scheduler.rs:368` as layered (not unified). The output-piping discipline is observed.

### Lock 5 — IR + per-backend lower

**Verdict: partial.** PASS-B §6.a row 5 + Agent B.3 §Lock 5 cite the divergence: "Rust adopts per-shape walk via `emit_*` methods. TS + WASM follow." PASS-B §2.c (Emitter reshape) addresses this — collapse 30-method trait to 8-10 per-shape methods. Honoured via §7 surgery 8 ("Reshape `Emitter` trait per Agent B.4 §Q7"). Surgery 8's wording is correct; the residual question is whether option (a) (Rust adopts per-IrNode) or option (b) (TS/WASM adopt per-shape) wins. Agent B.4 §Q7 punts to the synthesis; PASS-B §2.c picks per-shape implicitly. Synthesis ratifies per-shape; honoured.

### Lock 6 — xtask emits committed source

**Verdict: honoured.** PASS-B §6.a row 6 + Agent B.3 §Lock 6 cite `xtask/src/main.rs:1-9` and the 168,785 LOC committed at `crates/core/src/grammar/generated/`. No proc-macro façade for parser emission. Surgery 12 (split `regen.rs`) preserves Lock-6 semantics.

### Lock 7 — `crates/path/` consolidated

**Verdict: violated; redress pending.** PASS-B §6.a row 7 + Agent B.3 §Lock 7 cite four locations (`crates/bbnf-path/`, `crates/bbnf-path-ts/`, `crates/core/src/path/`, `crates/core/src/runtime/path.rs`). PASS-B §2.d (path consolidation) and §7 surgery 10 ("Consolidate path machinery per Lock 7") name the redress: `crates/path/` + `crates/path-core/` + retirement of `runtime/path.rs` + retirement of per-grammar `parse_with.rs` legacy lowering. Honoured via punch-list.

### Lock 8 — Surpass sonic-rs / simdjson / lightning-css

**Verdict: silent (must add).** PASS-B §6.a row 8 ("substrate-side: OpenFrame + checkpoint clone are the blockers") names *the mechanism* but not the *gate*. Agent B.3 §Lock 8 marks "out of Pass-B file-level scope". PASS-B has no SOTA-anchored bench gate; §8.9 mentions "SOTA-anchored gates (sonic-rs twitter, lightning-css bootstrap)" but doesn't *name the numbers* (sonic-rs M1 Pro twitter 436 µs; lightning-css 4.16 ms Bootstrap). Surgery in Lane 4.

### Lock 9 — Slice-borrow primary

**Verdict: violated structurally; redress in §8.2 + §7 surgery 9.** PASS-B §6.a row 9 cites: "partial — eager arena alloc; introduce `parse(input)` slab-free; arena via `parse_in`". Agent B.3 §Lock 9 cites `runtime/json/builder.rs:135` — `Vec::with_capacity(8)` + JsonArena slab-allocation in default `JsonStructBuilder::new()`. The redress is direct-projection emit (§7 surgery 9) which holds partial state on the call stack + SmallVec.

**Fault L9.1**: PASS-B §7 surgery 9 names direct-projection emit as the Lock-9 redress but doesn't *enumerate the three lifetime APIs* (`parse(input)` slab-free; `parse_in(input, &bump)` bumpalo opt-in; `parse_owned(input)` no-borrow). Lock 9 verbatim: "*The three are surfaces over the same parse implementation; the lifetime parameter is the discriminant.*" Surgery: Pass-B §7 surgery 9 extends to enumerate the three surfaces and tag each as slab-free / bumpalo-opt-in / owned-opt-in.

### Lock 10 — Pratt + SIMD auto-detected

**Verdict: honoured.** PASS-B §6.a row 10 + Agent B.3 §Lock 10 + Agent B.5 §7 + §8 (KEEP cost-model + SIMD-detect in egraph + simd-scan; no separate `bbnf-pratt` / `bbnf-simd-detect` crates). Lock 10 honoured by mechanism; KISS perf-bias respected (§6 + §7 + §8 of replacement design).

### Lock 11 — Path-deps for incubating sister crates

**Verdict: honoured; promotion candidates ready.** PASS-B §6.a row 11 + Agent B.3 §Lock 11 + Agent B.4 §Q3 + §7 surgery 13 ("Promote egraph + egraph-derive + csp-solver per Lock 11"). API-freeze checklist enumerated in Agent B.4 §Q3. Honoured.

### Lock 12 — ser + gorgeous archive

**Verdict: out-of-scope (Pass C).** PASS-B §6.a row 12 + Agent B.3 §Lock 12 + §5.b item 1.

### Lock 13 — No god directories

**Verdict: violated; redress in punch-list.** PASS-B §6.a row 13 cites the archetype: "violated archetype (`runtime/` 17 children mixed concerns); shape sub-API divergence; 11 god modules". §7 surgery 4 + 5 + 8 + 12 + 16 + 18 collectively redress.

**Fault L13.1**: PASS-B §6.a row 13 cites *11 god modules* but doesn't enumerate the per-file split design. Agent B.2 §6 + Agent B.4 §Q5 enumerate `xtask/regen.rs` split (849 LOC → 6 sub-modules) and shape `struct_direct.rs` collapse. The remaining 9 god modules (per Agent B.2 §6 table: `flat/struct_direct.rs` 1033 LOC; `dispatcher/support.rs` 902 LOC; `regex_scan_adapter.rs` 786 LOC; `wrap/struct_direct.rs` 622 LOC; `runtime/css_l4/builder.rs` 1014 LOC; `runtime/css_l4/value.rs` 852 LOC; `runtime/css_l4/document.rs` 541 LOC; `backend/emitter.rs` 566 LOC; `keyword/struct_direct.rs` 534 LOC; `array/mod.rs` 514 LOC) need explicit split designs. Surgery 16 says "11 Pass-B god modules split per CENSUS §5 + Agent B.2 §6" but doesn't name target sub-module decomposition for each. Surgery: surgery 16 expands to per-file split-target table.

### Lock 14 — Full grammar generalisation

**Verdict: violated systematically; redress mass-targeted; **but the proposed redress includes the per-grammar-crate footprint that Amendment 01 retracts.**

PASS-B §6.a row 14 cites "violated systematically — 9 per-grammar runtime dirs × ~7 hand-written files; bbnf-ir manifest mirror (Pass-A)". §7 surgery 6 names "`xtask/src/runtime_template/`" (correct under Amendment 01); §7 surgery 7 names "Scaffold per-grammar declaration crates `crates/<grammar>/` × 9" — **superseded by Amendment 01**.

Per Amendment 01: the per-grammar declaration crates DO NOT EXIST in the corrected workspace. Replacement substrate is:

- Generated parser + runtime modules: `crates/bbnf-runtime/src/grammars/<name>/{generated.rs, runtime.rs}` — template-emitted subdirectories under bbnf-runtime, NOT separate crates
- Host-fn implementations: `crates/bbnf-host-prims/src/` — single grammar-agnostic primitive library
- Tests + fixtures: `crates/bbnf-test-fixtures/<name>/` — fixture files only, no Rust per-grammar
- Bench harnesses: `crates/bbnf-bench/benches/` — template-emitted benches

**Fault L14.1**: PASS-B §1.b rows 4-7, §1.d row 2, §2.a, §2.b, §3, §5.a item 1, §5.b items 2-3, §6.a row 14, §7 surgery 7, §7 surgery 18, §8.1, §8.3, §8.8 — 18 sites name "per-grammar declaration crates" or `crates/<grammar>/` × 9. Under Amendment 01 each re-anchors. Surgery: re-anchor per the table below.

| PASS-B site | Stale language | Re-anchored language |
|---|---|---|
| §1.b row 4 | `crates/bbnf/src/runtime/`, `crates/bbnf/src/specialised/` | `crates/bbnf-runtime/src/grammars/bbnf/`; specialised content as `bbnf-host-prims` extension or grammar-source `@host` directive |
| §1.b row 5 | `crates/json/src/runtime/` | `crates/bbnf-runtime/src/grammars/json/` |
| §1.b row 6 | `crates/css-l4/src/runtime/`, `crates/css-l4/src/specialised/` | `crates/bbnf-runtime/src/grammars/css-l4/`; CSS-specific colour-function logic as `bbnf-host-prims::compose` or grammar-source `@host` |
| §1.b row 7 | `crates/sheets/src/runtime/`, `crates/sheets/src/specialised/` | `crates/bbnf-runtime/src/grammars/sheets/`; canonical-form logic as host-fn composition |
| §1.b row 8 | per-grammar declaration crates (5 trivial cohort) | `crates/bbnf-runtime/src/grammars/{bnf,csv,ebnf,css_pretty,math}/` template-emit |
| §1.d row 2 | "per-grammar declaration crate `crates/<grammar>/src/generated.rs`" | `crates/bbnf-runtime/src/grammars/<name>/generated.rs` |
| §2.a heading | `crates/<grammar>/` × 9 in heading | strike "× 9 per-grammar crates"; replace with `crates/bbnf-runtime/src/grammars/<name>/` template emission + `bbnf-host-prims/` |
| §2.a body | "per-grammar declaration crates emerge — one per grammar — carrying:" with 6 sub-bullets | "per-grammar runtime subdirs emerge under `bbnf-runtime/src/grammars/`, all template-emitted; host-fn implementations live in `bbnf-host-prims/`; tests + fixtures live in `bbnf-test-fixtures/<name>/` (fixture files, not Rust)" |
| §2.b para 2 | "extension via per-grammar declaration crate's `specialised/` module" | "extension via host-fn composition in metadata or `@host` directive in grammar source" |
| §3 row "crates/<grammar>/ × 9" | "9 new crates | per-grammar declaration crates" | strike row; add `crates/bbnf-host-prims/` row + `crates/bbnf-test-fixtures/` row |
| §5.a item 1 | "Pass-A scope; Pass-B requires the resolution" | unchanged in substance; the receiver is bbnf-host (the dispatch + registry mechanism), generic per Amendment 01 |
| §5.b item 2 | "gorgeous per-grammar files (CENSUS §2.5) retire alongside per-grammar declaration crates" | "gorgeous per-grammar files retire alongside the central per-grammar dirs (template-emit replaces them)" |
| §6.a row 14 | "9 per-grammar runtime dirs × ~7 hand-written files" / "template-emit per Agent B.5 §1" | unchanged; the per-grammar runtime *directories* under `bbnf-runtime/src/grammars/` retire as hand-written modules and emerge as template-emitted (Amendment 01 §"What replaces per-grammar declaration crates" para 1 explicitly preserves this wording) |
| §7 surgery 7 | "Scaffold per-grammar declaration crates `crates/<grammar>/` × 9" | "Scaffold `bbnf-host-prims/` + `bbnf-runtime/src/grammars/<name>/` template-emitted subdirs (no per-grammar crates)" |
| §7 surgery 18 | "Generated-output relocation — `crates/core/src/grammar/generated/` distributes across per-grammar declaration crates" | "Generated-output relocation — `crates/core/src/grammar/generated/` relocates to `crates/bbnf-runtime/src/grammars/<name>/generated.rs` (template-emitted)" |
| §8.1 | "9 per-grammar declaration crates + `crates/path/` family" | "template-emitted subdirs under `bbnf-runtime/src/grammars/` + `bbnf-host-prims/` + `crates/path/` family" |
| §8.3 | "extension via per-grammar declaration crate's `specialised/` module" | "extension via metadata-declared host-fn composition or grammar-source `@host` directive" |
| §8.8 | "168,750 LOC distributes across 9 per-grammar declaration crates per Agent B.4 §Q6" | "168,750 LOC distributes across 9 template-emitted subdirs under `bbnf-runtime/src/grammars/`" |

**Fault L14.2**: PASS-B §6.a row 14 surgery says "template-emit per Agent B.5 §1" but doesn't include the future-grammar onboarding test (Lock 14 verifiability invariant: adding `yaml.bbnf` requires source file + metadata block + `cargo xtask regen`; nothing else). Surgery: row 14 surgery extends to "+ future-grammar onboarding test (yaml.bbnf) verifying two-surface ceremony". This is the Lock-14 closure gate per Amendment 01 §"Tranche-drafting discipline under amendment" item 5.

**Fault L14.3**: PASS-B §1.e row 4 (simd-scan) and §1.b row 5 (per-grammar runtime json/) — neither names the *future-grammar onboarding test* as the Lock-14 closure invariant. Per Amendment 01 §"Premise" + §"Settled position": "future-grammar onboarding test (Lock 14 verification) collapses to a two-step ceremony". Surgery: PASS-B §6.a row 14 explicitly names the receiving gate as future-grammar onboarding test (added to Tranche E per Amendment 01).

**Lock 14 lane verdict: violated; redress mass-targeted; surgery is reanchoring 18 sites + adding future-grammar onboarding test.** The substantive redress (template-emit + grammar-agnostic host-prims + zero per-grammar crates) is correct under Amendment 01; PASS-B's substance survives, only language re-anchors.

---

## §4 — Lane 2 — Sequencing Discipline

PASS-B is a single-pass synthesis; sequencing-discipline lane (per HARDENING.md §Lane 2: "if target is a multi-wave plan") is **n/a**. The Pass-B punch list at §7 carries 19 items in dependency-correct order (pre-conditions → extraction → template → declaration crates → emit reshape → direct-projection → path → pipeline → xtask → promote → scrub → tests → god-module split → bench → relocation → verification gate). The dependency arrow at PASS-B §13 §5.2 (closing posture) "no half-state — each step's output is the next step's input" is correct in claim; sequencing scrutiny defers to the MASTER-PLAN target audit (which inherits the Pass-B punch list and assigns to Tranche E waves).

---

## §5 — Lane 3 — Cohesion

Standard: every claim verifiable from artefacts the target produces or cites. Identify orphan claims and orphan deliverables.

### Fault C.1 — 86.07% samply share retirement claim

PASS-B §8.2 states "Lock 1 honoured by mechanism, not just by symbol-naming" and §7 surgery 9 cites "RESTART-SKETCH §A.7 (86.07% samply share retirement)". The claim is *the mechanism retires the cost*. The 86.07% number per RESTART-SKETCH §A.7 is the `JsonStructBuilder::checkpoint` deep-clone share. Direct-projection emit replaces the heap-stack with call-stack frames + SmallVec — this *should* retire the 86.07% but Pass B doesn't *measure* it. The claim is structural (no checkpoint = no clone) but no Pass-B gate validates the post-restart samply distribution.

Surgery: PASS-B §7 surgery 9 extends to *"validate via samply post-restart that the JsonStructBuilder::checkpoint hot path is absent; share <1% expected"*. Without measurement, the claim is structural-only.

### Fault C.2 — Manifest mirror retirement orphan

PASS-B §6.b row "system-cohesion" cites "2 PathSegment defs, 1 manifest mirror" — the mirror is `crates/ir/src/registry/strategy.rs:130-185 PRODUCTION_MANIFEST_TABLE` which mirrors `[workspace.metadata.bbnf-strategy]`. PASS-B §5.a item 1 ("bbnf-ir Lock-14 redress") + §7.1 item 3 (pre-conditions) name the redress: "`PRODUCTION_MANIFEST_TABLE` consults `[workspace.metadata.bbnf-strategy]` at xtask time; runtime side carries no hardcoded grammar idents."

The claim is correct; the *gate* is missing. PASS-B doesn't name a Pass-B-internal gate that verifies the manifest mirror is gone post-restart. The verification commands at §7.2 surgery 19 ("Lock-14 verification gate — the 3 verification commands fire with ZERO matches in Pass-B substrate") cover the runtime side but not the IR side. Surgery: surgery 19 extends the verification command 1 to also check `crates/ir/src/registry/strategy.rs` carries no hardcoded grammar idents.

### Fault C.3 — Specialised cohort scope orphan

PASS-B §1.b rows 4-7 differentiate trivial cohort (5 grammars; fully template-emit) from specialised cohort (4 grammars; canonical surface from template + extension from declaration crate). Under Amendment 01 the *specialised extension* surface re-anchors: extension via metadata-declared host-fn composition or `@host` directive, not via per-grammar declaration crate's `specialised/` module.

Specifically: CSS L4 14-variant `OpenFrame` is the substantive question. Agent B.4 §Q2 raises it: "is the *14-variant CSS L4 OpenFrame* template-emittable (each variant describable from grammar shape + host-fn metadata) or genuinely hand-written?" PASS-B §2.b doesn't answer; it says "the trivial cohort emits 100% from template; the specialised cohort emits canonical surface from template + extension-via-host-fn from declaration crate". Under Amendment 01 the *extension* must be via host-fn composition (in metadata or `@host`); declaration crates don't exist.

The orphan: **does the 14-variant OpenFrame compose from `bbnf-host-prims` primitives + grammar-source `@host` directives, or does CSS L4 require Rust code that primitives + directives can't express?**

If primitives + directives suffice: Amendment 01 fully covers; CSS L4 emits 100% from template + composition.
If they don't suffice: **the amendment is inadequate** and CSS L4 requires a per-grammar Rust extension surface. The Pass-B audit must surface this.

Per Agent B.5 §1 "the trivial cohort emits 100% from template; the specialised cohort emits canonical surface from template + extension-via-host-fn from declaration crate" — Pass-B authoring agent already imagined the extension as host-fn-resolved, just emanating from a declaration crate. Under Amendment 01 the extension still resolves via host-fn; the *location* of the host-fn implementation moves to `bbnf-host-prims/` (or the grammar declares it `@host` and the prim composes from primitives). This is workable for CSS hex-color (Amendment 01 §"Host-fn implementations" example demonstrates `parse_hex_color = compose("regex:#[0-9a-fA-F]{6}", "parse_hex_pair", ...)`). But the 14-variant OpenFrame is more complex than hex-color — it carries 14 typed value variants per CSS construct (Color, Length, Selector, Declaration, etc.). The question whether each variant's runtime construction *composes from primitives* or requires bespoke Rust is unresolved.

Surgery: PASS-B §2.b para 4 (open question for the synthesis on CSS L4 14-variant) extends to: *"per Amendment 01, the resolution is host-fn composition in `bbnf-host-prims` or grammar-source `@host`. If a CSS L4 variant cannot decompose into primitive composition, that constitutes Lock-14 friction requiring extended-BBNF directive design (a Tranche E gate)."* This makes the Lock-14 closure gate *contingent on CSS L4 compositional adequacy*, which is the right discipline.

---

## §6 — Lane 4 — SOTA Anchoring

Standard: every parse-throughput gate cites competitor + dataset + platform.

PASS-B substrate has **no parse-throughput gates** in §1-§8 — the gates live in tranche docs (per Agent B.3 §Lock 8). The synthesis carries Lock-8 *mechanism* (§6.a row 8: "OpenFrame + checkpoint clone are the blockers; direct-projection emit; O(1) checkpoint") but doesn't cite SOTA targets.

### Fault SOTA.1 — §6.a row 8 silent on competitors

PASS-B §6.a row 8 surgery: "direct-projection emit; O(1) checkpoint". Lock 8 verbatim per `restart/locks/14-LOCKS.md` line 48: "*simdjson On-Demand 7 GB/s (JSON parse). sonic-rs M1 Pro twitter 436 µs (parse-to-typed-struct). lightning-css 4.16 ms Bootstrap (CSS).*" Surgery: row 8 surgery extends to *"direct-projection emit (no OpenFrame stack); O(1) checkpoint; Pass-B mechanism gates Lock-8 closure at: sonic-rs M1 Pro twitter 436 µs (JSON), lightning-css 4.16 ms Bootstrap (CSS L4)"*.

### Fault SOTA.2 — §8.9 silent on numbers

PASS-B §8.9 mentions "SOTA-anchored gates (sonic-rs twitter, lightning-css bootstrap)" without numbers. Surgery: §8.9 names verbatim "*sonic-rs M1 Pro twitter 436 µs; lightning-css 4.16 ms Bootstrap-4*" per `restart/corpora/SOTA.md` lines 50-54 + 134.

### Fault SOTA.3 — §7 surgery 17 (bbnf-bench) silent

PASS-B §7 surgery 17 ("bbnf-bench skeleton — `crates/bbnf-bench/` + per-grammar bench files") is silent on SOTA gates. Surgery: surgery 17 extends to "*per-grammar SOTA bench rows: json carries sonic-rs twitter parity gate (≤500 µs vs 436 µs); css_l4 carries lightning-css bootstrap parity gate (≤5 ms vs 4.16 ms); etc.*"

### Fault SOTA.4 — §4.b OpenFrame migration completeness gate silent

PASS-B §4.c ("OpenFrame migration completeness gate") names "OpenFrame appears ONLY in `archive/`" — this is a *negative-assertion* gate, not a SOTA-anchored throughput gate. Lock 8 demands SOTA anchoring for *every* perf claim. The OpenFrame retirement *enables* SOTA closure but isn't the closure itself.

Surgery: §4.c extends to *"in addition to the OpenFrame absence assertion, the post-restart bench gate validates samply hot-path attribution shifts away from JsonStructBuilder::checkpoint (was 86.07%) toward parser core; specific gate: post-restart samply share for parse-only hot-path > 60% on sonic-rs twitter dataset"*.

### Fault SOTA.5 — Pass-B carries the mechanism that meets gates; doesn't *gate*

Per Agent B.3 §Lock 8: "Pass-B substrate doesn't carry perf gates per se; the gates live in `docs/tranches/BA/`, `BB/`, `BC/`. The substrate carries the *mechanism* that meets the gates."

This is correct epistemically. But PASS-B §6.a row 8 (Lock 8 = "honoured / partial / violated") is silent on the carried gate's *receiver*. Per Lock 14 lane methodology, the carry must name receiving tranche + blocker + gate. PASS-B doesn't.

Surgery: §6.a row 8 explicitly names the receiver as the master-plan's Tranche E or Tranche J close gates (sonic-rs / lightning-css numbers per SOTA.md), with Pass-B substrate's Lock-8 closure contingent on that receiver landing.

---

## §7 — Lane 5 — Grammar-Authoritative Discipline (Lock 14 deep dive)

Standard: ZERO `match grammar { Json => ..., CssL4 => ..., ... }` arms in proposed generic crates; per-X tables for "all grammars" claims; future-grammar onboarding test; per-grammar code in declaration crates *only* (per HARDENING.md §Lane 5; under Amendment 01, this becomes "in `bbnf-host-prims/` or grammar-source `@host` only").

### Verification — match-arm grep

```
$ rg -nP 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|Bbnf\w*\s*=>|GoogleSheets\w*\s*=>' /Users/mkbabb/Programming/bbnf-lang/restart/audit/passes/PASS-B.md
(no matches)
```

Pass-B authoring text contains no proposed `match grammar` arms in generic crates. Honoured.

### Verification — grammar-name mention scan

```
$ rg -ni 'json|css_l4|bbnf|google_sheets|sheets|css_pretty|bnf|csv|ebnf|math' /Users/mkbabb/Programming/bbnf-lang/restart/audit/passes/PASS-B.md | wc -l
```

PASS-B.md mentions grammar names in 18 sites. Each must classify as ratified (per-X table cell, declaration crate path, audit anchor) or fault (paragraph hardcodes grammar in plan logic).

| Site | Class | Ratified or fault |
|---|---|---|
| §1.a row "rust/emitter/grammar.rs" | per-X table cell | ratified |
| §1.b rows 4-8 | per-grammar inventory + ABROGATE-REPLACE narrative | ratified per Amendment 01 (re-anchored to bbnf-runtime/src/grammars/<name>/) |
| §1.b paragraph "9 per-grammar dirs template-emit" | aggregated per-X count | ratified |
| §1.d row 1 ("`pub use bbnf::*`") | data point | ratified (the asymmetric aggregator is the redressed fault, not a grammar-arm) |
| §1.d row 2 paragraph "168,750 LOC distributes across 9 declaration crates: bbnf 21,503; bnf 3,290; csv 1,693; css_l4 107,138; css_pretty 9,021; ebnf 7,646; google_sheets 14,088; json 3,500; math 871" | per-X table | ratified (LOC distribution is per-grammar by virtue of being per-grammar source; the *receiver* re-anchors per Amendment 01 to bbnf-runtime/src/grammars/) |
| §2.a paragraph | architectural pivot describing per-grammar landing | **fault per Amendment 01**; surgery in Lane 1 L14.1 |
| §3 row "crates/<grammar>/ × 9" | facility ledger | **fault per Amendment 01**; surgery in Lane 1 L14.1 |
| §5.b item 2 | scope coordination | **fault per Amendment 01**; surgery in Lane 1 L14.1 |
| §7 surgery 7 | scaffold step | **fault per Amendment 01**; surgery in Lane 1 L14.1 |
| §7 surgery 18 | relocation step | **fault per Amendment 01**; surgery in Lane 1 L14.1 |
| §8.1 + §8.3 + §8.8 | greenfield commitments | **fault per Amendment 01**; surgery in Lane 1 L14.1 |
| §1.b row 6 "14-variant OpenFrame" | per-grammar architectural witness | ratified (the witness is the violation; the redress retires it) |

### Per-X tables for "all grammars" claims

PASS-B §1.b paragraph "9 per-grammar dirs template-emit", §1.d paragraph "168,750 LOC distributes across 9 declaration crates" — both are per-X (per-grammar) tables. Honoured.

PASS-B §2.b paragraph "trivial cohort emits 100% from template; the specialised cohort emits canonical surface from template + extension via per-grammar declaration crate's `specialised/` module" — needs to be per-X enumeration of *which* cohort each grammar belongs to. Trivial cohort = {bnf, csv, ebnf, css_pretty, math}; specialised cohort = {bbnf, json, css_l4, sheets}.

Surgery: PASS-B §2.b adds a per-X table:

| Cohort | Members | Template surface | Extension mechanism (Amendment 01) |
|---|---|---|---|
| trivial | bnf, csv, ebnf, css_pretty, math | full (value, document, view, kind, arena, builder, mod) | none |
| specialised | bbnf, json, css_l4, sheets | canonical | host-fn composition in `bbnf-host-prims` or `@host` in grammar source |

### Future-grammar onboarding test

PASS-B mentions no future-grammar onboarding test. Surgery: PASS-B §8 (greenfield commitments) adds §8.10:

> §8.10 — Future-grammar onboarding test
>
> Adding `yaml.bbnf` to the fleet requires:
> 1. Drop `grammar/yaml/yaml.bbnf` into the source tree
> 2. Add `[workspace.metadata.bbnf.grammars.yaml]` block to root Cargo.toml
> 3. Run `cargo xtask regen`
>
> No code change in any crate. No new crate. The Lock-14 verification command set fires zero matches; the per-grammar runtime subdir under `bbnf-runtime/src/grammars/yaml/` emerges template-emitted; if `yaml.bbnf` declares `@host` directives, host-fn composition in `bbnf-host-prims` resolves them.
>
> Receiver: Tranche E close gate per Amendment 01 §"Tranche E — the corrected convergent pivot".

### Lane 5 verdict: violated-with-amendment

The Pass-B authoring text honours the *spirit* of Lock 14 (zero match-arms, per-X tables for aggregations). But the proposed redress (per-grammar declaration crates × 9) overfits on the Lock-14 escape hatch, elevating *optional* per-grammar crates to *mandatory* infrastructure. Amendment 01 retracts; PASS-B reanchors per the L14.1 surgery table. The future-grammar onboarding test (L14.2 + the §8.10 addition) is the verifiable closure invariant.

---

## §8 — Lane 6 — Generated-Code Budget

Standard: per-tranche LOC budget; per-wave decomposition; per-grammar projection.

### Honour — §1.d LOC distribution

PASS-B §1.d row 2 cites the baseline distribution across 9 grammars:

| Grammar | Current LOC |
|---|---:|
| bbnf | 21,503 |
| bnf | 3,290 |
| csv | 1,693 |
| css_l4 | 107,138 |
| css_pretty | 9,021 |
| ebnf | 7,646 |
| google_sheets | 14,088 |
| json | 3,500 |
| math | 871 |
| **Total** | **168,750** |

Baseline-correct.

### Fault GCB.1 — No per-wave LOC delta projection

PASS-B doesn't project per-wave LOC deltas. The architectural pivots are likely to:

- **Net-grow**: typed-payload Rust expansion when OpenFrame retires (each enum variant materialises into stack-allocated struct per RESTART-SKETCH §B.2). Estimate: each per-grammar `<G>Builder` shrinks (no Vec<OpenFrame>) but each parse fn body grows (direct-projection inline construction). Plausibly +5-10% net per grammar.
- **Net-shrink**: struct_direct sub-modules retire (4 files × ~600 LOC = ~2400 LOC). Pipeline.rs collapse retires 103 LOC. xtask/regen.rs split = LOC-neutral. Per-grammar runtime hand-written (~440 LOC × 9 grammars = ~4000 LOC) retires; template emits canonical equivalent at ~similar LOC; net-zero.

Surgery: PASS-B §1 adds §1.h "LOC delta projection per architectural transposition":

| Pivot | Source LOC | Target LOC | Delta |
|---|---:|---:|---:|
| OpenFrame retiral × 9 grammars (direct-projection inline construction) | ~6000 (builder.rs × 9) | ~6500 (call-stack + SmallVec) | +500 |
| struct_direct sub-modules collapse (4 files) | ~2400 | 0 | −2400 |
| pipeline.rs facade collapse | 103 | 0 | −103 |
| Per-grammar runtime hand-written → template-emitted (canonical surface) | ~4000 | ~3800 | −200 |
| Per-grammar specialised content (CSS L4 OpenFrame variants → host-fn composition) | ~3000 | ~1500 (host-prim composition) | −1500 |
| **Net Pass-B substrate delta** | | | **−3700 LOC** |
| Generated tree (per-grammar parse fn bodies) | 168,750 | ~177,000 (typed-payload expansion) | **+8250** |

**Pass-B aggregate**: −3700 (substrate) + 8250 (generated) = **+4550 LOC net**, distributed across the 9 template-emitted subdirs under `bbnf-runtime/src/grammars/`.

### Fault GCB.2 — No per-wave budget gate

PASS-B §7 surgeries 4-19 don't carry per-wave LOC budgets. Surgery: each surgery extends to project its LOC delta with a budget gate (e.g., "surgery 9 (direct-projection emit): json grammar parse fn body grows from ~3500 to ~4000 LOC; 14% growth budgeted per RESTART-SKETCH §B.2; gate fails if growth exceeds 25%").

### Fault GCB.3 — xtask regen budget silent

PASS-B §7 surgery 6 ("Land `xtask/src/runtime_template/`") doesn't budget the xtask source LOC. Estimate per Agent B.5 §1: 200-400 LOC for proc-macro2 + quote generator. Surgery: surgery 6 extends to "+200-400 LOC xtask substrate".

### Fault GCB.4 — Specialised-cohort host-prim composition LOC silent

Under Amendment 01, the specialised cohort's extension (CSS L4 OpenFrame variants etc.) lives in `bbnf-host-prims/` as composition rules. The composition vocabulary's primitive count + per-grammar composition definition LOC isn't budgeted. Surgery: PASS-B §3 (new facilities) adds a `bbnf-host-prims` row with LOC budget — Amendment 01 §"Host-fn implementations" enumerates 8 primitives × ~30 LOC each = ~240 LOC primitive library + per-grammar composition declarations in metadata (~50 LOC per grammar × 4 specialised = 200 LOC). Total: ~440 LOC for `bbnf-host-prims/`.

### Lane 6 verdict: partial

The baseline LOC distribution is correct; the per-pivot projections + per-wave budget gates are absent. Surgery is mechanical (extend each surgery with LOC delta projection).

---

## §9 — Lane 7 — Friction Forecast

Standard: where users / grammar authors hit the new API and don't understand it; required cookbook + error-message + migration page; verify target gates them.

PASS-B contains **zero friction-surface enumeration**. This is the most consistent gap across the synthesis.

### Fault FF.1 — `pointer!` macro friction silent

Per HARDENING.md §Lane 7 "particular foci": `pointer!["a", "b", 1]` macro syntax was settled in Phase-4 BB.W5. PASS-B §1.b row "runtime/path.rs" mentions PathSegment but doesn't address `pointer!` macro friction. Surgery: PASS-B §3 (new facilities) or §8 adds friction surface row for `pointer!` macro:

> Friction: grammar author writes `pointer!["a", "b", 1]` and expects index `1` to mean array element [1]. The macro's compile-time path AST disambiguates: integer literal in trailing position = array index; preceding string literals = field names. Error message commit: "*pointer! macro: expected string literal (field name) or integer literal (array index); got `{token}`. Field names are quoted strings; array indices are unquoted integers.*"

### Fault FF.2 — `parse / parse_in / parse_owned` lifetime friction silent

Per HARDENING.md §Lane 7 "particular foci". PASS-B §6.a row 9 cites "introduce `parse(input)` slab-free; arena via `parse_in`" but doesn't enumerate the friction. Surgery: PASS-B §3 adds friction surface for the three lifetime APIs:

> Friction: user calls `parser.parse(input)` and expects O(1) — `parse_in(input, &bump)` is faster on retained-result workloads, but `parse(input)` is correct for transient parses. Decision tree:
>
> | Workload | Surface | Rationale |
> |---|---|---|
> | One-shot parse → discard | `parse(input)` | slab-free; lowest overhead |
> | Parse → retain document → multi-query | `parse_in(input, &bump)` | bumpalo arena amortises subsequent queries |
> | Parse → cross thread / FFI / serialize | `parse_owned(input)` | no lifetime parametric |
>
> Error message commit: "*Cannot return borrowed `JsonValue<'p>` outside borrow scope; use `JsonParser::parse_owned(input)` to obtain `'static` value.*"

### Fault FF.3 — Layout lowering errors silent

PASS-B §5.a item 1 names the Layout pass as Pass-A scope. The user-visible error when Layout lowering fails (rule X has no resolvable layout because Y) is a Pass-B emission consideration. Surgery: PASS-B §3 adds friction surface:

> Friction: grammar author writes a rule with circular type reference (`a: a* b -> compose`) and Layout lowering fails. Error message commit: "*Rule `{name}` cannot resolve to a layout: {reason}. Consider {hint}.*" where `{reason}` is one of {circular type reference, ambiguous variant tag, unresolved host-fn signature, conflicting field type}; `{hint}` references the corresponding cookbook section.

### Fault FF.4 — Pratt + SIMD auto-detection misfire silent

Per HARDENING.md §Lane 7. PASS-B §6.a row 10 cites "honoured" without enumerating misfire surfaces. Surgery: PASS-B §3 adds friction surface:

> Friction: grammar author writes a left-recursive rule the optimiser classifies as Pratt; the auto-detection misfires when the operator-chain shape is incomplete. Error message commit: "*Rule `{name}` matches Pratt heuristic ({matched_pattern}) but {reason}; falling back to recursive descent. To force recursive descent regardless, set `[workspace.metadata.bbnf.grammars.{g}.pratt = false]`.*" The escape hatch to *override* auto-detection lives in metadata, not in grammar source (Lock 10 forbids `@pratt`).

### Fault FF.5 — Crate-split migration silent

PASS-B §2.a names the crate split (`crates/core/` → `bbnf-codegen` + `bbnf-runtime` + ...). The user-visible migration is rewriting `use bbnf_core::backend::Emitter` to `use bbnf_codegen::Emitter`. PASS-B doesn't gate a migration page or per-import diff.

Surgery: PASS-B §7 adds surgery 20 — "Crate-split migration page: produce `docs/migration/post-restart-imports.md` with sed-recipe for every changed import path."

### Fault FF.6 — Future-grammar onboarding error message silent

Adding `yaml.bbnf` requires source file + metadata block. Friction: user adds source file but forgets metadata block (or vice versa). Error message commit: "*Grammar `{g}` declared in `{source.bbnf}` but absent from `[workspace.metadata.bbnf.grammars]`; add `[workspace.metadata.bbnf.grammars.{g}]` block to root Cargo.toml. See cookbook §A.1.*"

### Lane 7 verdict: silent (must add)

Six friction surfaces enumerated; PASS-B currently gates none. Surgery is to add §3 (new facilities) sub-table or §8 (greenfield commitments) §8.11 with friction surfaces + error messages + cookbook gate.

---

## §10 — Lane 8 — Carry & Deferral Audit

Standard: every "deferred to" / "carries to" / "future" / "TBD" / "user adjudicates" must name (a) receiver, (b) blocker, (c) receiving gate.

### Fault C&D.1 — §5.a "Pass-A coordination" carries vague

PASS-B §5.a lists 3 items "carries to Pass A":
1. Lock 14 violations in `crates/ir/`
2. Layout lowering pass naming convergence
3. Typed IR variant table per Phase-4 BC.W0

| Item | Receiver | Blocker | Gate |
|---|---|---|---|
| 1 | named (Pass A) | named (Lock 14) | **silent** |
| 2 | named (Pass A) | named (Lock 2) | **silent** |
| 3 | named (Pass A) | named (Phase-4 BC.W0) | **silent** |

Each of 3 items names receiver + blocker but *not* the receiving gate. Surgery: PASS-B §5.a items 1-3 each name the Pass-A wave + the verification command (e.g., item 1: "Receiver: Pass-A wave A.W2 (bbnf-ir Lock-14 redress); Blocker: PRODUCTION_MANIFEST_TABLE hardcodes 9 grammar idents; Gate: `rg -nP 'Json\w*Parser|...' crates/ir/src/` returns ZERO post-Pass-A").

### Fault C&D.2 — §5.b "Pass-C coordination" carries vague

PASS-B §5.b lists 4 items "carries to Pass C":
1. ser + gorgeous archive per Lock 12
2. gorgeous per-grammar files retire alongside per-grammar declaration crates
3. analysis crate (LSP-facing)
4. bbnf-path + bbnf-path-ts retirement-into-`crates/path/`

| Item | Receiver | Blocker | Gate |
|---|---|---|---|
| 1 | named (Pass C) | named (Lock 12) | **silent** |
| 2 | named (Pass C) | named (declaration crates retire) → re-anchor per Amendment 01 (no declaration crates; gorgeous per-grammar files retire alongside template-emitted gorgeous output under `bbnf-runtime/src/grammars/<name>/prettify.rs`) | **silent** |
| 3 | named (Pass C) | named (LSP for BBNF) | **silent** |
| 4 | named (Pass C) | named (Lock 7) | **silent** |

Surgery: PASS-B §5.b items 1-4 each name receiving Pass-C wave + verification gate.

### Fault C&D.3 — §2.b open question deferred without receiver

PASS-B §2.b para 4: "Open question for the synthesis: is the *14-variant CSS L4 OpenFrame* template-emittable...?" The synthesis is the receiver but doesn't *resolve* the question — PASS-B §2.b para 5 says "if the latter, the synthesis must accept per-grammar declaration crates carrying ~1000 LOC of specialised runtime code". Under Amendment 01 this resolves: the extension is host-fn composition, not declaration crates. The carry needs to *land*: PASS-B §2.b extends to *"per Amendment 01, the resolution is host-fn composition; surgery 7 of §7 names the bbnf-host-prims composition substrate as the extension surface; CSS L4 hex-color is the demonstration"*.

### Fault C&D.4 — §3 "Synthesis catalogues NO additional facilities" defers to KISS

PASS-B §3 paragraph: "The synthesis catalogues NO additional facilities beyond these. The brand-new candidates from Agent B.5 §6 (cost-model crate), §7 (pratt crate), §8 (simd-detect crate) all default to KEEP-IN-EGRAPH / KEEP-IN-CODEGEN per `feedback_kiss-perf-bias`."

This is a defer-with-rationale (KISS), but doesn't name the *gate* that *re-opens* the question. Surgery: §3 paragraph extends to "*revisit if a non-egraph consumer arrives for cost-model; revisit if a non-codegen consumer arrives for pratt + simd-detect*". Already implicit in Agent B.5 §6 + §7 + §8 individually but PASS-B's synthesis flattens.

### Fault C&D.5 — §4.c OpenFrame migration completeness gate carries OPS detail

PASS-B §4.c "OpenFrame migration completeness gate" names "post-restart, OpenFrame appears ONLY in `archive/`". Receiver = `crates/core/tests/struct_direct_snapshots.rs:45-53` extension (per §7 surgery 14). Blocker = direct-projection emit (per §7 surgery 9). Gate = the negative-assertion. Honoured.

### Fault C&D.6 — §6.a row 8 carries Lock 8 to "tranche docs" without naming tranche

PASS-B §6.a row 8 surgery: "direct-projection emit; O(1) checkpoint" — substrate-side mechanism. The receiver tranche for Lock-8 closure is unnamed. Surgery: row 8 names "Receiver: Tranche E (per Amendment 01) close gate; Blocker: direct-projection emit + bench harness landed; Gate: sonic-rs M1 Pro twitter ≤500 µs (vs sonic-rs's 436 µs); lightning-css 4.16 ms Bootstrap parity for CSS L4."

### Fault C&D.7 — §1.b row 3 carries CSS L4 14-variant question without resolution

PASS-B §1.b row "runtime/css_l4/" cites "14-variant typed-value content lives in `crates/css-l4/src/specialised/`". Under Amendment 01 the receiver re-anchors. Surgery: row 3 cites "*14-variant typed-value content composes from `bbnf-host-prims` primitives + grammar-source `@host` directives; if a variant cannot decompose, that is Lock-14 friction surfacing extended-BBNF directive design (Tranche E gate)*". The unresolved question per Lane 3 C.3 is the same.

### Lane 8 verdict: partial

Seven dangling carries; each requires receiver + blocker + gate. Surgery is mechanical (extend each carry).

---

## §11 — Lane 9 — Greenfield Discipline

Standard: no quick solutions; no workarounds; no legacy code uncontested; idiomatic gestalt; architectural transpositions for elegance / simplicity / performance.

### Honour — Lock 1 + Lock 13 + Lock 14 retire together

PASS-B §6.a "The two most consequential Pass-B lock violations are **Lock 13** and **Lock 14**; both retire via per-grammar declaration crates + template-emitted runtimes." Under Amendment 01 the language reanchors but the *substantive* claim survives: Lock 1 + Lock 13 + Lock 14 retire as one architectural pivot. PASS-B §13 closing posture confirms: "**per-grammar declaration crates + template-emitted runtimes + direct-projection emit + reshaped Emitter trait**" — three of four conjuncts ratify under Amendment 01; the fourth (declaration crates) is the surface that retracts.

### Honour — OpenFrame retires by mechanism

PASS-B §8.2 "no heap-stack of OpenFrame; no `StructBuilder` trait ceremony; no `<G>StructCheckpoint` Vec-clone. Lock 1 honoured by mechanism, not just by symbol-naming." Greenfield-correct: the architectural transposition (direct-projection) replaces the workaround (checkpoint deep-clone). Per HARDENING.md §Lane 9 "**Architectural transpositions for elegance / simplicity / performance** are mandatory" — honoured.

### Honour — substrate.rs delete + struct_direct collapse

PASS-B §7 surgery 8 ("Reshape `Emitter` trait... Retire `backend/rust/emitter/shapes/struct_direct.rs` sub-modules (4 files); retire `backend/rust/emitter/shapes/substrate.rs` (119 LOC)"). The substrate-selection vestigial decision dies; the orthogonal-codepath sub-modules collapse. Honoured.

### Fault GD.1 — Per-grammar declaration crate proliferation IS the greenfield failure

Per HARDENING.md §Lane 9 "**No quick solutions** — every proposal honours its substrate, not patches it"; "**Idiomatic, gestalt approaches** — Rust-idiomatic; sonic-rs / lightning-css / simdjson cohesion the standard".

PASS-B §2.a proposes 9 per-grammar declaration crates as the Lock-14 redress. Per Amendment 01 §"Premise": "This is overfitting. Lock 14 names per-grammar declaration crates as an *optional* escape hatch, not a default. The master plan elevated the escape hatch to a mandatory 9-crate footprint without cause." This *is* a quick solution — using the escape valve as the default to retire 9 hand-written runtime dirs without grappling with the deeper question (is the per-grammar specialisation expressible in metadata + grammar source alone?).

The greenfield-correct redress (per Amendment 01): **zero per-grammar crates**. Two declarative surfaces (source file + metadata block). Host-fn composition in `bbnf-host-prims`. Future-grammar onboarding test = two-step ceremony.

PASS-B's redress is *better than the status quo* (template-emit retires hand-written runtimes regardless of where they land) but *not greenfield-correct* (declaration crates are escape-hatch overfitting). Amendment 01 closes the gap.

### Fault GD.2 — sonic-rs / lightning-css cohesion claim silent on workspace shape

PASS-B mentions sonic-rs / lightning-css / simdjson by name twice (§6.a row 8; §8.9). Per HARDENING.md §Lane 9 "*sonic-rs / lightning-css / simdjson cohesion the standard*" — these are workspace-shape exemplars. sonic-rs has *one* crate; lightning-css has *one* crate. The bbnf-lang post-restart workspace under Amendment 01 has 24 members; a 33-member workspace (PASS-B's implicit count via 9 per-grammar declaration crates) is *farther* from sonic-class cohesion.

PASS-B §8.1 "multiple small crates, each with one purpose" is the framing the workspace shape claims to honour. The 24-member workspace honours it; the 33-member overfit doesn't (each per-grammar crate's purpose is grammar-named, which is anti-Lock-14).

Surgery: PASS-B §8.1 + §13 closing posture re-anchor to Amendment 01's 24-member workspace. The framing — multiple small crates each with one purpose — survives; the count drops from 33 to 24.

### Fault GD.3 — gorgeous-per-grammar carry silent on Amendment 01 receiver

PASS-B §5.b item 2: "gorgeous per-grammar files (CENSUS §2.5) retire alongside per-grammar declaration crates". Under Amendment 01: gorgeous per-grammar files retire alongside the *template-emitted prettify subdirs under `bbnf-runtime/src/grammars/<name>/prettify.rs`*. Surgery in Lane 1 L14.1 row §5.b item 2.

### Lane 9 verdict: partial

The substantive greenfield commitments (architectural transposition; OpenFrame mechanism retirement; substrate.rs delete; struct_direct collapse) honour the discipline. The per-grammar-crate proliferation is the greenfield failure surface; Amendment 01 retracts; PASS-B reanchors. The cohesion-with-SOTA-exemplars claim survives Amendment 01 (24 < 33; closer to sonic-rs's one-crate cohesion).

---

## §12 — Punch list (execution-ready surgery)

In execution order, with target file:line, verbatim edit, owner, scope, lane.

### Pre-conditions (require Amendment 01 application + prior-pass coordination)

1. **§1.b rows 4-7 row 8 + §1.d row 2 paragraph + §2.a heading + §2.a body para 1 + §2.b para 2 + §3 row 3 + §5.a item 1 + §5.b items 1-4 + §6.a row 14 surgery + §7 surgery 7 + §7 surgery 18 + §8.1 + §8.3 + §8.8 reanchor**
   - Target: PASS-B.md 18 sites enumerated in Lane 1 L14.1
   - Edit: per the L14.1 reanchoring table (col "Re-anchored language")
   - Owner: pass-b-amendment agent
   - Scope: multi-section
   - Lanes: 1 + 5

2. **§6.a row 1 OpenFrame stance**
   - Target: PASS-B.md §6.a row 1 surgery cell
   - Verbatim edit: replace "OpenFrame is the substantive question" with "OpenFrame is tape rebranded; retires via direct-projection emit"
   - Owner: pass-b-amendment
   - Scope: single-line
   - Lane: 1 (L1.1)

3. **§7 surgery 9 cite surgery 14 as gate**
   - Target: PASS-B.md §7 surgery 9
   - Verbatim edit: append "Verification gate: surgery 14 negative-assertion extends to assert OpenFrame absence."
   - Owner: pass-b-amendment
   - Scope: single-line
   - Lane: 1 (L1.2)

4. **§7 surgery 9 enumerate three lifetime APIs**
   - Target: PASS-B.md §7 surgery 9
   - Verbatim edit: extend to enumerate `parse(input)` slab-free, `parse_in(input, &bump)` bumpalo opt-in, `parse_owned(input)` no-borrow as the three discriminant-by-lifetime surfaces over one parse implementation
   - Owner: pass-b-amendment
   - Scope: paragraph
   - Lane: 1 (L9.1)

5. **§7 surgery 16 expand to per-file split-target table**
   - Target: PASS-B.md §7 surgery 16
   - Verbatim edit: expand to enumerate the 11 god modules with per-file split-target sub-modules (per Agent B.2 §6 table)
   - Owner: pass-b-amendment
   - Scope: paragraph + table
   - Lane: 1 (L13.1)

### Pass-B execution

6. **§6.a row 14 add future-grammar onboarding test**
   - Target: PASS-B.md §6.a row 14 surgery
   - Verbatim edit: append "+ future-grammar onboarding test (yaml.bbnf two-step ceremony) per Amendment 01"
   - Owner: pass-b-amendment
   - Scope: single-line
   - Lane: 1 (L14.2)

7. **§8 add §8.10 future-grammar onboarding test**
   - Target: PASS-B.md §8 (greenfield commitments)
   - Verbatim edit: insert §8.10 per Lane 5 surgery (verbatim block)
   - Owner: pass-b-amendment
   - Scope: paragraph
   - Lane: 5 (L14.2 / L14.3)

8. **§2.b add per-X cohort table**
   - Target: PASS-B.md §2.b
   - Verbatim edit: insert per-X table (trivial vs specialised cohort enumeration with extension mechanism per Amendment 01)
   - Owner: pass-b-amendment
   - Scope: paragraph + table
   - Lane: 5 (Lane 5 §"Per-X tables")

9. **§3 add `bbnf-host-prims/` row + `bbnf-test-fixtures/` row**
   - Target: PASS-B.md §3 facility ledger
   - Verbatim edit: per Amendment 01 §"Corrected workspace shape" rows
   - Owner: pass-b-amendment
   - Scope: table rows
   - Lane: 1 + 5

10. **§3 strike `crates/<grammar>/ × 9` row**
    - Target: PASS-B.md §3 row "crates/<grammar>/ × 9"
    - Verbatim edit: strike row
    - Owner: pass-b-amendment
    - Scope: table row
    - Lane: 5 (L14.1 row §3)

11. **§7 surgery 9 add samply-share validation**
    - Target: PASS-B.md §7 surgery 9
    - Verbatim edit: append "Validate post-restart samply distribution: JsonStructBuilder::checkpoint hot path absent; share <1% expected (was 86.07% per RESTART-SKETCH §A.7)"
    - Owner: pass-b-amendment
    - Scope: paragraph
    - Lane: 3 (C.1)

12. **§4.b extend to manifest mirror gate**
    - Target: PASS-B.md §4.b (Tape doc-residue scrub)
    - Verbatim edit: extend Lock-14 verification command 1 to also check `crates/ir/src/registry/strategy.rs` carries no hardcoded grammar idents
    - Owner: pass-b-amendment
    - Scope: paragraph
    - Lane: 3 (C.2)

13. **§2.b para 4 resolve specialised cohort question per Amendment 01**
    - Target: PASS-B.md §2.b para 4 (open question on CSS L4 14-variant)
    - Verbatim edit: per Lane 3 C.3 surgery (resolution = host-fn composition; CSS hex-color = demonstration; if variant fails to decompose, extended-BBNF directive design = Tranche E gate)
    - Owner: pass-b-amendment
    - Scope: paragraph
    - Lane: 3 (C.3)

14. **§6.a row 8 cite SOTA targets**
    - Target: PASS-B.md §6.a row 8 surgery
    - Verbatim edit: append "Pass-B mechanism gates Lock-8 closure at: sonic-rs M1 Pro twitter 436 µs (JSON), lightning-css 4.16 ms Bootstrap (CSS L4)"
    - Owner: pass-b-amendment
    - Scope: single-line
    - Lane: 4 (SOTA.1)

15. **§8.9 cite SOTA numbers**
    - Target: PASS-B.md §8.9
    - Verbatim edit: append "sonic-rs M1 Pro twitter 436 µs; lightning-css 4.16 ms Bootstrap-4 per `restart/corpora/SOTA.md`"
    - Owner: pass-b-amendment
    - Scope: single-line
    - Lane: 4 (SOTA.2)

16. **§7 surgery 17 cite per-grammar SOTA gates**
    - Target: PASS-B.md §7 surgery 17
    - Verbatim edit: extend to "per-grammar SOTA bench rows: json carries sonic-rs twitter parity gate (≤500 µs vs 436 µs); css_l4 carries lightning-css bootstrap parity gate (≤5 ms vs 4.16 ms); sheets carries TBD-baseline gate"
    - Owner: pass-b-amendment
    - Scope: paragraph
    - Lane: 4 (SOTA.3)

17. **§4.c extend to samply-attribution gate**
    - Target: PASS-B.md §4.c (OpenFrame migration completeness gate)
    - Verbatim edit: append "post-restart bench gate validates samply hot-path attribution shifts away from JsonStructBuilder::checkpoint (was 86.07%) toward parser core; gate: post-restart samply share for parse-only hot-path > 60% on sonic-rs twitter dataset"
    - Owner: pass-b-amendment
    - Scope: paragraph
    - Lane: 4 (SOTA.4)

18. **§6.a row 8 name receiver tranche**
    - Target: PASS-B.md §6.a row 8 surgery
    - Verbatim edit: append "Receiver: Tranche E (per Amendment 01) close gate; Blocker: direct-projection emit + bench harness landed; Gate: sonic-rs M1 Pro twitter ≤500 µs"
    - Owner: pass-b-amendment
    - Scope: single-line
    - Lane: 4 (SOTA.5) + 8 (C&D.6)

19. **§1 add §1.h LOC delta projection table**
    - Target: PASS-B.md §1 (verdict ledger end)
    - Verbatim edit: insert §1.h "LOC delta projection per architectural transposition" with per-pivot table (5 rows + total)
    - Owner: pass-b-amendment
    - Scope: paragraph + table
    - Lane: 6 (GCB.1)

20. **§7 surgeries 4-19 add LOC budget gates**
    - Target: PASS-B.md §7 each surgery
    - Verbatim edit: append per-surgery LOC delta projection + budget gate (e.g., surgery 9: "json grammar parse fn body grows from ~3500 to ~4000 LOC; 14% growth budgeted; gate fails if >25%")
    - Owner: pass-b-amendment
    - Scope: 16 single-line additions
    - Lane: 6 (GCB.2)

21. **§7 surgery 6 add xtask substrate LOC budget**
    - Target: PASS-B.md §7 surgery 6
    - Verbatim edit: append "+200-400 LOC xtask substrate per Agent B.5 §1"
    - Owner: pass-b-amendment
    - Scope: single-line
    - Lane: 6 (GCB.3)

22. **§3 add `bbnf-host-prims/` LOC budget**
    - Target: PASS-B.md §3 row `bbnf-host-prims/` (added in punch-list item 9)
    - Verbatim edit: include LOC budget — 8 primitives × ~30 LOC = ~240 LOC primitive library + per-grammar composition declarations in metadata (~50 LOC × 4 specialised = 200 LOC); total ~440 LOC
    - Owner: pass-b-amendment
    - Scope: row + comment
    - Lane: 6 (GCB.4)

23. **§3 add friction-surface sub-table**
    - Target: PASS-B.md §3 (or §8 §8.11)
    - Verbatim edit: insert friction-surface table for `pointer!`, lifetime APIs, Layout errors, Pratt misfire, crate-split migration, future-grammar onboarding (six rows; each with friction surface + verbatim error message + cookbook gate)
    - Owner: pass-b-amendment
    - Scope: paragraph + table
    - Lanes: 7 (FF.1-FF.6)

24. **§7 add surgery 20 (crate-split migration page)**
    - Target: PASS-B.md §7 punch list end
    - Verbatim edit: insert surgery 20 — "Crate-split migration page: produce `docs/migration/post-restart-imports.md` with sed-recipe for every changed import path."
    - Owner: pass-b-amendment
    - Scope: single-line
    - Lane: 7 (FF.5)

25. **§5.a items 1-3 name receiving Pass-A wave + verification command**
    - Target: PASS-B.md §5.a items 1-3
    - Verbatim edit: each item names Pass-A wave (e.g., "Receiver: Pass-A wave A.W2"; "Gate: rg verification command verbatim")
    - Owner: pass-b-amendment
    - Scope: paragraph
    - Lane: 8 (C&D.1)

26. **§5.b items 1-4 name receiving Pass-C wave + verification gate**
    - Target: PASS-B.md §5.b items 1-4
    - Verbatim edit: each item names Pass-C wave + gate
    - Owner: pass-b-amendment
    - Scope: paragraph
    - Lane: 8 (C&D.2)

27. **§3 paragraph extends to KISS revisit gate**
    - Target: PASS-B.md §3 closing paragraph
    - Verbatim edit: append "revisit if a non-egraph consumer arrives for cost-model; revisit if a non-codegen consumer arrives for pratt + simd-detect"
    - Owner: pass-b-amendment
    - Scope: single-line
    - Lane: 8 (C&D.4)

28. **§13 closing posture re-anchor workspace count**
    - Target: PASS-B.md §13 closing posture
    - Verbatim edit: replace "9 per-grammar declaration crates" with "template-emitted subdirs under `bbnf-runtime/src/grammars/` + `bbnf-host-prims/`"; the workspace count drops from 33 to 24 per Amendment 01
    - Owner: pass-b-amendment
    - Scope: paragraph
    - Lane: 9 (GD.2)

29. **§5.b item 2 Amendment 01 receiver re-anchor**
    - Target: PASS-B.md §5.b item 2
    - Verbatim edit: "gorgeous per-grammar files retire alongside template-emitted prettify subdirs under `bbnf-runtime/src/grammars/<name>/prettify.rs`"
    - Owner: pass-b-amendment
    - Scope: single-line
    - Lanes: 5 (L14.1) + 9 (GD.3)

30. **§13 closing posture verbatim edit "per-grammar declaration crates" → Amendment 01 substrate**
    - Target: PASS-B.md §13 closing posture
    - Verbatim edit: replace "**per-grammar declaration crates + template-emitted runtimes + direct-projection emit + reshaped Emitter trait**" with "**template-emitted subdirs under `bbnf-runtime/src/grammars/` + `bbnf-host-prims/` + direct-projection emit + reshaped Emitter trait + zero per-grammar crates**"
    - Owner: pass-b-amendment
    - Scope: single-line
    - Lane: 5 + 9

---

## §13 — Final readiness

> **Decision: amendment-required.**
>
> Pass-B's substance — the architectural pivot retiring Lock 1 + Lock 13 + Lock 14 together via template-emit + direct-projection + Emitter coarsening — is correct. The 19-item punch list at §7 names the right surgeries in dependency-correct order; the OpenFrame retirement, struct_direct collapse, substrate.rs delete, and god-module splits honour greenfield discipline. What requires amendment is *language*, not *substance*: 18 sites name "per-grammar declaration crates × 9" as the Lock-14 redress; under Amendment 01 (which supersedes any per-grammar-crate language), these resolve to template-emitted subdirectories under `bbnf-runtime/src/grammars/<name>/` + `bbnf-host-prims/` + `bbnf-test-fixtures/`. The amendment honours Lock 14 by *construction* (zero per-grammar crates; future-grammar onboarding test = source file + metadata block) where Pass-B's draft honoured it by *escape hatch* (declaration crates as the optional Lock-14 fallback elevated to default). Plus six friction surfaces missing error-message commits; five Lock-8 SOTA gates missing competitor-dataset-platform anchors; seven dangling carries missing receiver + blocker + gate; per-wave LOC budget projections silent.
>
> Hereupon the pass-b-amendment agent applies the 30-item punch list verbatim. The Pass-B synthesis re-emerges Amendment-01-compliant; the master-plan synthesizer inherits the corrected substrate; Tranche E (the convergent pivot under Amendment 01) drafts with the corrected facility ledger.
