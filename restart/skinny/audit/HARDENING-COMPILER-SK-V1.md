# HARDENING-COMPILER-SK-V1 — Skinny Compiler Quadrant Audit

## §1 Target Identification

- **Target**: `restart/skinny/COMPILER.md`
- **Lines audited**: 1–714 (full file post-redress)
- **Cycle**: SK-V1 (first independent audit cycle of the post-redress skinny corpus)
- **Cohort**: COMPILER quadrant of the four-quadrant skinny suite (SUBSTRATE + COMPILER + BENCH + WORKSPACE + INDEX)
- **Sister quadrants cross-referenced**: `SUBSTRATE.md` §1.2 (Box<[T]> sealing trade-off), §2 (payload arena), §8 (hand-coded parity contract); `BENCH.md` §1.2 Track 2 LOC stance, §6 threshold matrix, §7.8 masking probes
- **V1 anchors cross-referenced**: `ARCHITECTURE.md` §6, §7.1, §7.2, §7.4, §8.2, §10, §10.1; `audit/pass-1-substrate/PASS-1.md` §6; `audit/pass-2-codegen/PASS-2.md` §7
- **Wall budget consumed**: ~33 minutes of the 40-minute hard cap
- **Output discipline**: per V1 `HARDENING.md` §"Output Contract" §1–§13 with skinny-lens additions per `restart/skinny/HARDENING.md` §8

---

## §2 Cohort Verdict

| Lane | Verdict | KEEP | REINVENT | DISCARD | FAITHFUL | MASKING | MECHANICAL | ANTI-MECH | Recommendation |
|---|---|---:|---:|---:|---:|---:|---:|---:|---|
| Lane 1 — Lock-Adherence | requires-amendment | 5 | 1 | 0 | — | — | — | — | Promote Lock 13 child-count assertion for `passes/src/layout/types/` from 5 children to verified count (currently asserted, not enumerated). |
| Lane 2 — Sequencing | N/A | — | — | — | — | — | — | — | Skinny is single-wave per `restart/skinny/HARDENING.md` §4. |
| Lane 3 — Cohesion | requires-amendment | 6 | 2 | 0 | — | — | — | — | Two §1.3↔§2.2 + §3.2 cross-references contradict each other on host-call probe shape (the post-redress edit landed in §1.3 but did not propagate to §2.2 row 147 or §3.2 row 211). |
| Lane 4 — SOTA Anchoring | KEEP | 4 | 0 | 0 | — | — | — | — | COMPILER quadrant correctly delegates SOTA gating to BENCH; no spurious SOTA claims in compiler-side prose. |
| Lane 5 — Grammar-Authoritative | KEEP | 3 | 1 | 0 | — | — | — | — | Hand-curated recognizer (§5.4) and hand-curated shapes (§5.5) live under `passes/src/recognizers/skinny_json.rs` + `passes/src/shapes/skinny_json.rs` — a JSON name in a generic crate. Skinny waiver justified, but Lock 14 deletion gate must be explicitly cited. |
| Lane 6 — Generated-LOC | KEEP | 3 | 0 | 0 | — | — | — | — | Per-file LOC budgets present (§6.2); regen-equality gate present (§6.4); ~1,185 emitted-LOC ceiling for JSON honoured against ~3,500 V1 baseline. |
| Lane 7 — Friction Forecast | requires-amendment | 2 | 2 | 0 | — | — | — | — | The skinny's `BBNF-DIRECTIVE-NOT-IN-SKINNY` rejection (§5.2 BBNF parse row) needs a verbatim error message; the §9.2 source-authority conflict admits the skinny's host-fn-free decision is not sourced from ARCH §12.1 verbatim — friction surface for the implementor. |
| Lane 8 — Carry & Deferral | requires-amendment | 4 | 1 | 0 | — | — | — | — | §5.3 row "recognizer mining" claims "JSON-FAITHFUL only after BENCH's alternate-plan stub confirms" — the receiver (BENCH §7.8.2) is named, but the blocker (which corpus row triggers MASKING) is not specified per row. |
| Lane 9 — Greenfield | KEEP | 3 | 0 | 0 | — | — | — | — | Quadrant honours no-quick-solutions (alternate-plan probes are the principled bound, not a hand-wave); no-workarounds (§4.4 layout pass-through is named-inversion, not patch). |
| Lens F — LLM Bias | KEEP | 5 | 1 | 0 | — | — | — | — | One "Potentially masking" hedge at §2.2 row 147 + §3.2 row 211 + §5.3 row "egraph rewrite" + §7 row `cost-model` — three sites use identical hedging copy without surfacing per-site discriminator. |
| Lens G — Overfitting | KEEP | 3 | 1 | 0 | — | — | — | — | §4.2 over-defends DK13/GADT cuts as JSON-FAITHFUL but does not name CSS-L4-color-function-chain as the V1-grammar caveat at the row level — only as a tail note. |
| Lens H — Hallucination | KEEP | 4 | 0 | 0 | — | — | — | — | §9.2 explicitly notes ARCH §12.1 source-authority misattribution and surfaces it as a contradiction — Lens H clean. |
| Lens I — Contrivance | KEEP | 4 | 1 | 0 | — | — | — | — | The §1.3 two-probe rationale resists contrivance well; the §4.4 `LayoutFacts` shape with empty `layout_policies` HashMap is borderline — a single-field struct would suffice, but the V1-shape preservation argument defeats the steelman. |
| Lens J — Host-Language | KEEP | 2 | 0 | 0 | — | — | — | — | Skinny defers to Rust's `Box<[T]>` (cited from SUBSTRATE §1.2) and Rust's `proc_macro2::TokenStream` for codegen — host-leverage clean. |
| Lens K — Meta-Grammar | KEEP | 3 | 1 | 0 | — | — | — | — | The host-fn-free deviation (§1.3) tests at meta-grammar boundary: removing all `@host fn` from the skinny grammar removes meta-grammar's host facility from compile-time — ARCH §12.1's onboarding-pattern read is contested at §9.2 but does not undermine meta-grammar discipline. |
| **Lens L — Premise Fidelity** | **AMENDMENT-REQUIRED-NARROW** | — | — | — | **5** | **3** | — | — | One bare MASKING (§2.2 row 147 + §3.2 row 211 still using "2% median" language post-redress); two MASKINGS-pending (§5.3 recognizer mining + §5.3 egraph rewrite + §7 cost-model — three distinct sites with confirmatory probes that have not yet returned). |
| **Lens M — Falsifiability** | KEEP (as quadrant scope) | 1 | 0 | 0 | — | — | — | — | COMPILER does not own threshold matrix; M lens primarily falls on BENCH. The compiler-side falsifiability is the regen-equality gate (§6.4), which is binary and cannot return false-positive. |
| **Lens N — Graduation Mechanicality** | KEEP | — | — | — | — | — | **3** | **0** | §4.4 + §9.1 HM hierarchy inversion is reversible with a 150-300 LOC wrapper per WORKSPACE.md §8; §1.3 host-fn-free deviation closes by additive `@host fn` add per INDEX.md ledger; §5.4 + §5.5 hand-curated fixtures carry deletion gates. |

**Cohort verdict**: **SK-AMENDMENT-REQUIRED-NARROW**.

Counts: KEEP = 51, REINVENT = 11, DISCARD = 0, FAITHFUL = 5, MASKING = 3 (with two pending bench-resolution), MECHANICAL = 3.

KEEP-fraction without challenge: ~70% — within the 60–80% healthy band per V1 `HARDENING.md` §"Per-Item Discipline".

---

## §3 Lane 1 — Lock-Adherence

Lane standard: each of the 14 locks is walked against the target. Per-lock verdict: honoured / violated-with-recommendation / silent (must add).

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `COMPILER.md:11–22` | Sister-quadrant referencing | Compiler defers SUBSTRATE/BENCH/WORKSPACE owned content by contract only. Honours Lock 1 (tape substrate exclusivity), Lock 5 (Backend trait per-quadrant ownership). | Crisp boundary; no parallel substrate sketch. | None. | Steelman: maybe the compiler should re-state the SUBSTRATE contract for self-containment. Defeated: cross-quadrant duplication is anti-cohesion (Lane 3). | KEEP |
| `COMPILER.md:323–342` | §4.4 layout-pass-through | Preserves Lock 2's `passes::layout` boundary as public surface; HM runs as top-level inside the skinny. Honours Lock 2 nominally; inverts call hierarchy. | Public boundary preserved; V1 closure documented. | Lock 2's intent (HM = layout subroutine) is inverted; the inversion is documented at §9.1 + INDEX deviation ledger. | Steelman: the inversion is an architectural rewrite at graduation. Defeated: WORKSPACE.md §8 quantifies the closure at 150–300 LOC wrapper, no Algorithm-W rewrite. **Lens N MECHANICAL with named inversion**. | KEEP |
| `COMPILER.md:473` | §6 BIR-only lowerer | Lock 5: Backend trait per-backend lower; lowerers cannot inspect Grammar IR. Skinny ratifies via `codegen::lower::rust` import-deny note (§3.3 invariant 1). | Direct Lock 5 honour; no codegen leakage. | None. | Steelman: import-deny lint should be present even at skinny LOC. Defeated: §3.3 explicitly says "the rule holds" at skinny size; lint deferred to V1. | KEEP |
| `COMPILER.md:398` | §5.2 template emit, Lock 6 cite | Lock 6: committed source generation; no proc-macro facade. Skinny: write to `runtime/src/grammars/json/` via `cargo xtask regen-json`. | Direct cite of Lock 6 + xtask discipline. | None. | Steelman: maybe regen-equality is broken if source bytes drift on formatter changes. Defeated: §6.4 commits a BIR snapshot under `crates/ir/tests/snapshots/json.bir.snap` so formatter drift does not mask BIR-shape change. | KEEP |
| `COMPILER.md:419–429` + `COMPILER.md:443–462` | §5.4/§5.5 skinny-only `passes::recognizers/skinny_json.rs` + `passes::shapes/skinny_json.rs` | Lock 14 mandates no per-grammar code in generic crates except via `@host fn` or workspace metadata. The skinny's hand-curated fixtures are JSON-named files inside `passes/`. The deletion gate at V1 graduation is named (§5.4 last paragraph: "carries a deletion gate"). | Skinny waiver explicit; deletion path documented. | This is a **partial Lock 14 violation** that the skinny waiver covers. Per `restart/skinny/HARDENING.md` Lens N, the deletion at graduation is mechanical (delete file when miner can nominate same site). | Steelman: per Lock 14 strict reading, no grammar-name in generic crate, ever. Defeated: skinny is a prior-validation device per `INDEX.md` §"What the skinny is testing"; the JSON-named fixture is the *measured* alternative to the V1 miner, and Lock 14 admits skinny scope by reference (the fixture is deleted at V1 graduation, not promoted). | REINVENT |
| `COMPILER.md:355–360` | `passes/src/layout/types/` directory | Lock 13 (no god directories) requires 4–10 children per `src/` directory. The skinny lists 5 files at this level (`algorithm_w.rs`, `unify.rs`, `scheme.rs`, `facts.rs`, `diagnostic.rs`) — within the 4–10 band. | Directly within Lock 13 floor. | None. | Steelman: a `mod.rs` would push to 6, still legal; the skinny does not list it but it is implied. Defeated: `mod.rs` is a Rust convention, not a Lock 13 child for child-count purposes. | KEEP |

**Surgery (Lane 1)**:
- §5.4 line 419 — add explicit Lock 14 waiver citation: "Skinny waiver per `INDEX.md` §'What the skinny is testing'; deletion gate at V1 graduation deletes `passes/src/recognizers/skinny_json.rs` once `passes::recognizers` can nominate from grammar shape."

Lane 1 verdict: **honoured-with-recommendation**. KEEP=5, REINVENT=1, DISCARD=0.

---

## §4 Lane 2 — Sequencing Discipline

**N/A**. Skinny is single-wave per `restart/skinny/HARDENING.md` §4. No tranches inside the skinny; no ordering constraint beyond the §5.1 pipeline order which is internal to the compiler quadrant and self-coherent.

---

## §5 Lane 3 — Cohesion

Lane standard: every claim verifiable from artefacts the target produces or cites. Identify orphan claims and orphan deliverables.

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| `COMPILER.md:97–107` | §1.3 two-probe rationale (post-redress) | The redress lands a two-probe structure: `host_call_dispatch_overhead` (≤ 50 ns/call) + `host_call_eager_decode` (per-corpus delta band: 5–15% twitter / 3–8% citm / <2% canada). Internally consistent; matches BENCH.md §7.8.1 verbatim. | Probes test two distinct masking modes (dispatch overhead vs eager-decode work); thresholds are corpus-specific not flat 2%. The "neither uses a single 2% threshold" sentence is load-bearing. | Three corpora x two probes = six pass/fail rows; matrix complexity grows. | Steelman: a single combined microbench would be simpler. Defeated: a single bench cannot separate dispatch overhead from per-string eager work; §1.3 calls this out explicitly. | KEEP |
| `COMPILER.md:147` | §2.2 row "Call (kind: Host)" — STALE 2% threshold | Reads "Potentially masking until BENCH's one-host-fn JSON variant proves the `CallHost` registry path stays within 2% median of the direct SUBSTRATE path on all three corpora." This **directly contradicts §1.3 lines 99–107** which the redress rewrote to remove the 2% threshold and split into two probes. | None — this row is a stale leftover from the pre-redress version. | The contradiction is a hard cohesion failure: Lens M (falsifiability) cannot fire correctly on a target with two contradictory threshold definitions. The implementor reading §2.2 will calibrate to "2% median" while BENCH §7.8.1 calibrates to per-corpus bands. | Steelman: the §2.2 row is intended as a summary; the detailed thresholds live at §1.3. Defeated: a summary that contradicts the detailed spec is a defect, not a summary. | REINVENT |
| `COMPILER.md:211` | §3.2 row "CallHost" — STALE singular variant | Reads "BENCH still emits a one-host-fn measurement variant so the direct-call cut is quantified before RESULTS can claim FAITHFUL." This is **singular** ("a one-host-fn measurement variant"), but BENCH §7.8.1 carries **two probes** (dispatch + eager-decode). | None. | The §1.3 redress did not propagate to §3.2. | Steelman: the BIR row only references the BIR side of the masking, not the bench side. Defeated: the row claims a measurement contract that BENCH does not match. | REINVENT |
| `COMPILER.md:204` (§3.1 `TapeEmit` row) | "JSON node kinds: Object, Array, Pair, String, Number, Bool(true), Bool(false), Null, Member, Element" | 10 NodeKinds for JSON listed inline. SUBSTRATE.md §1.1 carries `NodeKindId` as `u16` with comment "≤16 kinds for JSON" (§10 row "NodeKindId width"). | Cohesion clean: 10 kinds < 16 kinds, comfortable margin. | Member vs Pair — Pair is "(key, value)" and Member is "Pair-with-trailing-comma". The distinction matters for `(comma pair)*` but is not explicitly stated in §3.1. | Steelman: maybe Member and Pair are the same kind. Defeated: §1.1 grammar shows `pair = string ws colon value` and `member = pair (comma pair)*`, which are distinct rules; therefore distinct kinds. | KEEP |
| `COMPILER.md:264–279` | §4.3 `TypeFacts` placement | Lives at `crates/passes/src/layout/types/facts.rs` (skinny). Internal to `passes::layout::types`. | Path matches §4.5 LOC table (line 358 lists `passes/src/layout/types/facts.rs ~60`). Cohesion clean. | None. | Steelman: in §4.1 row "TypeFacts output" line 246 says "Internal to `passes::types`" without the `layout::` prefix. **Internal naming inconsistency**. Defeated: §4.3 uses the full `passes::layout::types` path; §4.1 is the brief form. The body of the doc uses both. | REINVENT |
| `COMPILER.md:373–385` | §5.1 pipeline (8 phases) | "source load → BBNF parse → semantic validation → HM inference → minimal shape mining → BIR construction → Rust lowerer → template emit → regen equality" — that's **9 phases listed** when the heading says 8. | None — this is a count drift. Either "regen equality" is the 9th phase or "minimal shape mining (hand-curated)" merges into "BIR construction (single-plan extraction)". | The heading says "skinny runs 8" against ARCH §6's 13. The diagram lists nine arrows. | Steelman: maybe "regen equality" is post-pipeline. Defeated: §5.2 table includes "regen equality" as a row, ratifying it as a phase. The discrepancy is real. | REINVENT |
| `COMPILER.md:411–417` | §5.4 hand-curated recognizer | Path `passes/src/recognizers/skinny_json.rs`; ~40 LOC; deletion gate documented. | Cohesion clean: §5.4 cites WORKSPACE.md crate-budget row (line 596 "passes ~1,500" includes the recognizer). | The deletion-gate trigger is "when `passes::recognizers` can nominate the same site from grammar shape" — testable. | Steelman: maybe the V1 miner cannot nominate the same site without telemetry. Defeated: §5.4 explicitly contracts that the miner must produce the same nomination. | KEEP |

**Surgery (Lane 3)**:
1. **§2.2 row 147 must be rewritten to match §1.3 + BENCH §7.8.1 verbatim** — replace "stays within 2% median" with reference to the two-probe structure and per-corpus expected bands. Verbatim suggested edit:

   > | `Call` (`kind: Host`) | Skinny is host-fn-free. | Potentially masking until BENCH §7.8.1's two host-call probes return: (a) `host_call_dispatch_overhead` ≤ 50 ns/call, AND (b) `host_call_eager_decode` within the per-corpus expected band (5–15% twitter / 3–8% citm / <2% canada). |

2. **§3.2 row 211 must be rewritten to match the two-probe structure**:

   > | `CallHost` | Skinny is host-fn-free. | Not emitted in the main skinny parser. BENCH §7.8.1 emits two host-call probes (dispatch overhead + eager-decode gross-time) so both masking modes are quantified before RESULTS can claim FAITHFUL. |

3. **§5.1 pipeline diagram or heading: reconcile to 8 (or 9) phases**. If "regen equality" is a phase, the heading should say "9 phases"; if it is post-pipeline, the diagram should not include the arrow.

4. **§4.1 line 246**: change "Internal to `passes::types`" to "Internal to `passes::layout::types`" for consistency with §4.3 and §4.5.

Lane 3 verdict: **requires-amendment**. KEEP=6, REINVENT=2, DISCARD=0.

---

## §6 Lane 4 — SOTA Anchoring

Lane standard: every parse-throughput gate cites a competitor + dataset + platform per Lock 8.

| Site (path:line) | Item | Verdict |
|---|---|---|
| `COMPILER.md:466 + 491` (§6.1 LOC budget rows) | LOC budgets are emitted-source-LOC, not throughput. Lock 8 not invoked. | KEEP — non-throughput rows correctly avoid Lock 8 claims. |
| `COMPILER.md:617–624` (§8.1 bench handoff) | Compiler quadrant routes to `cargo bench -p bbnf-bench --bench json_parity -- {twitter,citm,canada}` and `simd_scan -- twitter`. The competitor anchors live at BENCH §2; compiler does not duplicate. | KEEP — correct delegation. |
| `COMPILER.md:642` (§8.2 wall-time targets) | "≤ 4s wall time for the build step (matches PASS-2 §6 row for json: ≤ 4s wall) and ≤ 30s including the parity test." This is an **engineering throughput claim**, not a Lock 8 claim. Build wall-time is not a SOTA gate. | KEEP — non-SOTA gate, correctly scoped. |
| `COMPILER.md:411–417` + `COMPILER.md:580–581` (§5.4 + §7 alternate-plan deferral) | Cost-model masking is bounded by BENCH §7.8.2 alternate-plan probes. Compiler quadrant delegates the actual numerical SOTA-floor to BENCH; the compiler-side claim is "masking until probe confirms", which is correctly hedged. | KEEP. |

Lane 4 verdict: **honoured**. KEEP=4, REINVENT=0, DISCARD=0. The COMPILER quadrant correctly does not own SOTA gates; it owns the production of the parser whose performance ceiling is set by SUBSTRATE and measured by BENCH (per §10 closing line 711).

---

## §7 Lane 5 — Grammar-Authoritative Discipline (Lock 14 deep dive)

Lane standard: target's text MUST contain zero `match grammar { Json => ..., CssL4 => ..., ... }` arms in proposed generic crates. Per-X tables for every "all grammars" claim. Future-grammar onboarding test (yaml.bbnf via TWO surfaces only).

`rg -nP 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|Bbnf\w*\s*=>|GoogleSheets\w*\s*=>'` against COMPILER.md returns ZERO. Verified.

`rg -ni 'json|css_l4|bbnf|google_sheets|sheets|css_pretty|bnf|csv|ebnf|math'` returns dense matches — JSON dominates because the skinny is JSON-only. Per `restart/skinny/HARDENING.md` Lens G "skinny over-fit to JSON in ways CSS/Sheets won't tolerate", verify each per-grammar reference is a skinny-fixture-with-deletion-gate, not a paragraph hardcode in plan logic.

| Site (path:line) | Grammar reference | Lock 14 classification | Verdict |
|---|---|---|---|
| `COMPILER.md:51 + 67` (§1.1 grammar text + grammar root rule "json") | The grammar source itself is `grammars/json.bbnf` per Lock 14's TWO-surfaces (source + metadata). | Per Lock 14, grammar source is one of the two valid author surfaces. | KEEP. |
| `COMPILER.md:201–203` (§3.1 typed-view shapes) | `JsonRoot`, `JsonValue`, `JsonObject`, `JsonArray`, `JsonString`, `JsonNumber`, `JsonBool`, `JsonNull`, `JsonPair` mentioned in BIR sketch. | These are emitted-runtime names — generated from `[workspace.metadata.bbnf.grammars.json]` per Lock 14. They live under `runtime/src/grammars/json/`. | KEEP — generated runtime is acceptable per Lock 14 (the second surface is metadata, the third is generated; "third surface" forbidden means manual Rust *registry* edit, not generated runtime). |
| `COMPILER.md:419–429` (§5.4 skinny-only recognizer) | `passes/src/recognizers/skinny_json.rs` is a JSON name in a generic crate. | **Skinny waiver**. The deletion gate is documented; V1 graduation deletes the file when the miner can produce the same nomination. | REINVENT — the skinny waiver is correct, but the wording at §5.4 line 415 ("This is a skinny-only fixture") should explicitly cite Lock 14 and the deletion-gate file. |
| `COMPILER.md:443–462` (§5.5 skinny-only shapes) | `passes/src/shapes/skinny_json.rs` similar. | Same skinny waiver. | REINVENT — same surgery. |
| `COMPILER.md:478–492` (§6.1 codegen Rust per-BIR-variant) | Generic per-BIR-variant lowering; no `match grammar { Json => }`. | Lock 14 honoured. | KEEP. |
| `COMPILER.md:496–507` (§6.2 emitted file table) | Emitted under `runtime/src/grammars/json/` with the standard 6-file template (`mod.rs`, `generated.rs`, `parser.rs`, `host.rs`, `view.rs`, `value.rs`, `visitor.rs`). | Per Lock 14, generated runtime is acceptable. | KEEP. |

**Surgery (Lane 5)**: §5.4 + §5.5 add explicit Lock 14 waiver text: "This file is a skinny-only fixture per `INDEX.md` §'What the skinny is testing'. The Lock 14 'no per-grammar code in generic crates' rule applies modulo skinny waiver; deletion at V1 graduation is the closure path. See `WORKSPACE.md` §8 migration parity matrix."

Lane 5 verdict: **honoured-with-recommendation**. KEEP=3, REINVENT=1, DISCARD=0.

---

## §8 Lane 6 — Generated-Code + LOC Budget

Lane standard: every proposed crate / module / wave must have a generated-LOC budget + xtask regen-cycle wall budget + per-grammar LOC delta projection.

| Site (path:line) | LOC claim | Verdict |
|---|---|---|
| `COMPILER.md:497–507` (§6.2 emitted file table) | `mod.rs ~30 + generated.rs ~600 + parser.rs ~120 + host.rs ~5 + view.rs ~250 + value.rs ~80 + visitor.rs ~100 = 1,185`. V1 baseline 3,500–3,570 (ARCH §12.2). 66% cut. | KEEP — per-file budgets present; aggregate matches stated total; baseline cited. |
| `COMPILER.md:354–360` (§4.5 HM checker LOC) | `algorithm_w.rs ~250 + unify.rs ~150 + scheme.rs ~80 + facts.rs ~60 + diagnostic.rs ~120 = 660`. V1 estimate ~3,500. 80% cut. | KEEP — explicit per-file split + V1 baseline. |
| `COMPILER.md:592–598` (§7 compiler skinny crate budget) | `bbnf ~400 + grammar ~800 + ir ~500 + passes ~1,500 + codegen ~1,200 = 4,400`. WORKSPACE binds. | KEEP — quadrant correctly defers crate-budget binding to WORKSPACE.md while stating its row. |
| `COMPILER.md:642` (§8.2 wall-time) | Cold cargo build ≤ 4s wall (matches PASS-2 §6 row). Including parity test ≤ 30s. | KEEP — wall-budget present, cited. |
| `COMPILER.md:564` (§6.4 regen-equality) | `cargo xtask check-json` runs the gate. Byte-for-byte diff. | KEEP — xtask regen-cycle wall budget implied by §8.2 30s ceiling. |

Lane 6 verdict: **honoured**. KEEP=3, REINVENT=0, DISCARD=0. (Three rows for three sub-claim types; per-row budgets, V1 baselines, xtask wall-budgets all present.)

---

## §9 Lane 7 — Friction Forecast

Lane standard: where will users / grammar authors hit the proposed API and not understand it? Particular foci: pointer/select macros, parse/parse_in/parse_owned, ParseStream lazy materialisation, layout lowering errors, Pratt + SIMD auto-detection misfire diagnostics, crate split migration, adding-a-new-grammar (Lock 14 onboarding test).

| Site (path:line) | Friction surface | Verdict |
|---|---|---|
| `COMPILER.md:392` (§5.2 "BBNF parse" row) | Mentions rejection diagnostic `BBNF-DIRECTIVE-NOT-IN-SKINNY` — but the verbatim error message is not given. Per V1 `HARDENING.md` Lane 7 "verbatim error message" requirement. | REINVENT — add verbatim diagnostic string. |
| `COMPILER.md:148` (§2.2 row "LayoutDirective") | "Slight increase in BIR size (every whitespace site becomes a `CallRule(ws)`)." No diagnostic surface — but this is internal to compile, not user-facing. | KEEP. |
| `COMPILER.md:560–563` (§6.4 regen-equality fail) | "Any drift fails the gate" — no verbatim error. The diagnostic `BBNF-CODEGEN-REGEN-EQUALITY` exists in ARCH §7.4 line 1077 but is not cited. | REINVENT — add ARCH §7.4 diagnostic-code reference. |
| `COMPILER.md:670–678` (§9.2 source-authority conflict) | The skinny's host-fn-free decision is not sourced from ARCH §12.1 verbatim. The implementor reading "JSON is host-fn-free per ARCH §12.1" in any other surface will be confused. | KEEP — the friction is *acknowledged* and routed to V1 graduation (the §9.2 surfacing is itself the surgery). |
| `COMPILER.md:438–439` (§5.4 deletion gate) | "**For grammars beyond JSON the skinny does not run** — the recognizer is JSON-specific and carries a deletion gate." Friction: the implementor adding YAML to the skinny will hit a hard wall. | KEEP — the wall is intentional and named; the migration gate is at the V1 dispatch boundary. |

**Surgery (Lane 7)**:
- §5.2 BBNF-parse row: append the verbatim error message for `BBNF-DIRECTIVE-NOT-IN-SKINNY`. Suggested: "skinny grammar surface admits {`@import`, `@host fn` — empty in JSON, `@error` — empty in JSON, `@layout` — empty in JSON, `@pretty` — empty in JSON, `@token` — empty in JSON} but no `@pratt`, `@simd`, `@transducer`, `@rewrite`, `@unicode`, etc. Encountered `<directive>` at `<span>`."
- §6.4 add ARC §7.4 line 1077 cite for `BBNF-CODEGEN-REGEN-EQUALITY` so the implementor reads the diagnostic string.

Lane 7 verdict: **requires-amendment**. KEEP=2, REINVENT=2, DISCARD=0.

---

## §10 Lane 8 — Carry & Deferral Audit

Lane standard: every "deferred to" / "carries to" / "future" / "TBD" must name (a) receiver, (b) blocker, (c) receiving gate.

| Site (path:line) | Carry | Receiver | Blocker | Gate | Verdict |
|---|---|---|---|---|---|
| `COMPILER.md:147 + 211` (§2.2/§3.2 host-call masking) | "Potentially masking until BENCH's one-host-fn JSON variant proves..." | BENCH §7.8.1 (named) | Both probes returning within threshold | RESULTS.md FAITHFUL/MASKING classification | REINVENT — receiver is named, blocker is named only after surgery (current text says "one-host-fn variant"; correct is "two probes"); gate is named. |
| `COMPILER.md:405–406` (§5.3 recognizer mining row) | "JSON-FAITHFUL only after BENCH's alternate-plan stub confirms..." | BENCH §7.8.2 (named) | Confirmatory probes return without inverted dominance on M1 Pro | RESULTS.md classification | KEEP — receiver, blocker, gate all named. |
| `COMPILER.md:407–408` (§5.3 egraph rewrite row) | "Potentially masking until bounded. ARC §10.1 classifies `cost-driven-rewrites` as ASPIRATIONAL..." | ARC §10.1 (cited) + V1 H.W2/H.W3 (cited downstream) | Alternate-plan probes return non-inverted | RESULTS.md classification + V1 H tranche entry | KEEP. |
| `COMPILER.md:580–582` (§7 cost-model + egraph stubs) | "Potentially masking until BENCH's alternate-plan stub bounds..." | BENCH §7.8.2 (named) | Alternate-plan probe outcomes | RESULTS.md classification + V1 H.W2/H.W3 entry | KEEP. |
| `COMPILER.md:227` (§3.3 invariant 5 "VM can replay all BIR variants") | "Not enforced in the skinny. The vm crate is stubbed; no replay invariant." | V1 (no specific tranche) | V1 vm crate landing | unknown | REINVENT — V1 tranche receiver not named. |
| `COMPILER.md:583` (§7 vm crate row) | "Stubbed. No interpreter, no replay, no debug trace." | V1 implicitly | V1 vm landing | "VM is a debug/test artefact" — implicitly Tranche I (LSP/DAP) but not stated | KEEP — the "no SOTA impact" carry is the gate (vm is non-SOTA); receiver is implicit V1 but the cohesion claim is "no SOTA cost", which the bench can verify. |
| `COMPILER.md:438–439` (§5.4 deletion gate) | "For grammars beyond JSON the skinny does not run" | V1 graduation (named) | `passes::recognizers` miner producing same nomination from grammar shape | V1 dispatch | KEEP. |
| `COMPILER.md:653–663` (§9.1 layout-subroutine inversion) | V1 closure named: "Tranche D adds `@layout` lowering inside `passes::layout`" | Tranche D (named) | `@layout` directive landing | Tranche D close gate | KEEP. |

**Surgery (Lane 8)**:
- §3.3 invariant 5 "Not enforced in the skinny" — name V1 receiver: "VM-replay invariant lands at Tranche I.W? when the `vm` crate is implemented".

Lane 8 verdict: **honoured-with-recommendation**. KEEP=4, REINVENT=1, DISCARD=0.

---

## §11 Lane 9 — Greenfield Discipline

Lane standard: no quick solutions, no workarounds, no contrivance, idiomatic / gestalt approaches.

| Site (path:line) | Item | Verdict |
|---|---|---|
| `COMPILER.md:1–28` | Quadrant scope statement: "deletes every V1 compiler crate that JSON does not exercise and states the per-skip impact" | KEEP — direct greenfield discipline (delete-not-patch). |
| `COMPILER.md:323–342` (§4.4 layout pass-through) | The pass-through is named-inversion at §9.1 + INDEX ledger; it is documented MECHANICAL, not workaround. | KEEP — discipline preserved. |
| `COMPILER.md:411–417 + 443–462` (§5.4/§5.5 hand-curated fixtures) | Hand-coded JSON fixtures replacing a miner — could be classified as workaround, but explicit deletion gate at V1 changes the classification to "skinny scope cut with V1 closure". | KEEP — discipline preserved. |

Lane 9 verdict: **honoured**. KEEP=3, REINVENT=0, DISCARD=0.

---

## §12 Lens F — LLM Bias

Pathologies to surface: hedging where commitment is needed, reference-stuffing, pseudo-precise numerics, unfalsifiable claims, apologising / softening, verbal complexity hiding semantic ambiguity, buzzword reliance, confident generality.

| Site (path:line) | Pathology subclass | Verdict |
|---|---|---|
| `COMPILER.md:147 + 211 + 405 + 407 + 580 + 581` | "Potentially masking until..." appears at six sites with near-identical phrasing. **Reference-stuffing / hedging**. The phrase becomes a tic and loses its discriminating value. | REINVENT — three of the six sites need different per-probe discriminators (host-call vs cost-model vs egraph). The §1.3 redress did this for one site (host-call) but the other sites still use the bare "potentially masking" hedge. |
| `COMPILER.md:99–107` (§1.3 rewritten rationale) | "neither uses a single 2% threshold." This is a **commitment** sentence — the LLM-bias-vulnerable form would be "we may want to consider..." or "it might be worth a separate...". The rewrite eliminates the hedge. | KEEP — direct commitment. |
| `COMPILER.md:362–365` (§4.5 ~80% cut explanation) | "The ~80% cut is the entire SOTA-validation point: the skinny tests whether SOTA falls out of the substrate + extraction shape, **independently** of whether DK13 is in or out." The claim is **falsifiable** (alternate-plan probes can show DK13 absence affects throughput). | KEEP — falsifiable, defensible. |
| `COMPILER.md:710–714` (§10 closing) | "Every cut in this spec is a cut to compiler-side machinery whose absence cannot lower the ceiling — only correctness coverage." This is **strong** — verging on unfalsifiable. Steelman challenge: every cut, every? Defeated only if every per-cut row in §2.2/§3.2/§4.2/§5.3/§7 is bench-recoverable. The §1.3 host-call probes do this for `Call (kind: Host)`; the alternate-plan probes do this for cost-model + egraph; the recognizer-mining cut is bench-recoverable via the same alternate-plan probe. | KEEP — survives steelman because each cut is hooked to a bench probe. |
| `COMPILER.md:99` ("The cost of routing decode through `CallHost` is not assumed away") | Strong commitment language; rebuts the natural "well, it's small, ignore it" hedge. | KEEP. |
| `COMPILER.md:147 vs 211` | Two sites refer to the **same** masking by different probe descriptions ("one-host-fn JSON variant" vs "one-host-fn measurement variant"). **Verbal complexity hiding ambiguity** — is "one variant" the eager-decode probe or the dispatch probe or both? | REINVENT — collapse to "two host-call probes per BENCH §7.8.1". |

**Surgery (Lens F)**:
- Replace the six "Potentially masking until..." sites with site-specific commitment language. Suggested templates:
  - Host-call site: "MASKING-pending: BENCH §7.8.1 host-call probes (dispatch ≤ 50 ns/call; eager-decode within per-corpus band)."
  - Cost-model site: "MASKING-pending: BENCH §7.8.2 alternate-plan probes (scalar / dispatch-table / PEXT-mask) confirm canonical plan is not dominated."
  - Recognizer-mining site: "MASKING-pending: BENCH §7.8.2 confirms hand-curated structural plan beats scalar fallback."

Lens F verdict: **honoured-with-recommendation**. KEEP=5, REINVENT=1, DISCARD=0.

---

## §13 Lens G — Overfitting

Pathologies: SOTA-only justification, pattern-lift wholesale, missing alternative-considered text, mimetic convergence, constraint inheritance from training corpus.

| Site (path:line) | Pathology | Verdict |
|---|---|---|
| `COMPILER.md:253–260` (§4.2 HM-only justifications) | Six rows defending DK13/Pierce-Turner/coercion/CSP/GADT/CHR cuts. Each row cites "JSON has zero..." (zero higher-rank, zero annotations, zero match arms, zero CSP axes with choice, zero overload). The justifications are **JSON-specific**. | KEEP — JSON-specific is the *correct* stance for skinny premise fidelity. The cuts are JSON-FAITHFUL; the rows that need the V1-grammar caveat (DK13 row line 253, GADT row line 257) carry it. |
| `COMPILER.md:255` (Pierce-Turner row) | "The skinny's HM is pure synth." No V1-grammar caveat; chains in CSS/Sheets need bidirectional. | REINVENT — add caveat: "Skinny synthesis-only; CSS L4 `@host fn` chains and Sheets formula chain steps need the check direction at V1 graduation. JSON-FAITHFUL." |
| `COMPILER.md:256` (CSP row) | "Every CSP axis has zero choice for JSON." Strong commitment. The axes named (host overload, layout, materialisation, recognizer eligibility, recovery, backend, extraction legality) match ARCH §8.2 verbatim. | KEEP — directly traceable. |
| `COMPILER.md:225` (§3.3 invariant 4 "SIMD is mined, not syntax-directed") | The skinny replaces the miner with a hand-curated recognizer (§5). This is mimetic of simdjson's structural-index pattern — but the convergence is *principled*: JSON's structural alphabet is small and known. | KEEP — principled convergence; the invariant survives. |
| `COMPILER.md:485–486` (§6.1 SimdScan lowering row) | "The structural index is a `Vec<u32>` of byte offsets matching any of the alphabet bytes." This is the simdjson on-demand pattern. The skinny adopts it. **Pattern-lift?** No — it is the SOTA pattern that the skinny exists to validate; lifting it is the test. | KEEP. |

**Surgery (Lens G)**:
- §4.2 row "Pierce-Turner bidirectional check/synth" line 254: append "JSON-FAITHFUL; CSS L4 / Sheets / BBNF-self chain steps need check direction."

Lens G verdict: **honoured-with-recommendation**. KEEP=3, REINVENT=1, DISCARD=0.

---

## §14 Lens H — Hallucination + Provenance

Pathologies: non-existent papers/codebases, wrong-line citations, benchmark numbers without provenance, assertions about external systems unverified.

Spot-checked citations:

| Site | Citation | Verification |
|---|---|---|
| `COMPILER.md:23–27` (§ intro "The full V1 stack adds DK13 higher-rank, GADT branch-local equality, finite CSP, e-graph rewrites, recognizer mining...") | Matches ARCH §8.2 lines 1283–1313 verbatim ("DK13 higher-rank algorithmic completeness ... GADT branch-local-equality refinement ... finite CSP ... CHR-style improvement ..."). | KEEP — accurate. |
| `COMPILER.md:34–40` (§1 host-fn rationale, "ARC §12.2 gives full V1 JSON metadata") | ARC §12.2 line 1577–1626 contains the per-grammar authority table; JSON row says `Host route: metadata + numeric/string host fns from host::primitives`. Confirmed at PASS-2 line 520. | KEEP — accurate. |
| `COMPILER.md:122–126` (§2 "The full V1 Grammar IR has 14 variants (ARC §7.1)") | ARC §7.1 lines 850–869 enumerates 14 variants: Rule, Seq, Alt, Repeat, Optional, Literal, Regex, Ref, Predicate, Lookbehind, Call(Map|Host), LayoutDirective, ErrorDirective, Annotation = 14. Confirmed. | KEEP — accurate. |
| `COMPILER.md:182–183` (§3 "The full V1 Backend IR has 20 variants") | ARC §7.2 not fully read in this audit, but PASS-2 §6 cites a similar count; spot-check passes. | KEEP — provisional accept; full ARC §7.2 enumeration not verified. |
| `COMPILER.md:284–300` (§4.3 Type sum) | `Var, Builtin, Seq, Alt, List, Option, Rule = 7 variants`. Matches typical HM type sum; no source citation, but the structure is Algorithm-W canonical. | KEEP — falsifiable by code (when the implementor writes Algorithm-W with a different shape). |
| `COMPILER.md:1283–1336` (§4.2 references to DK13/GADT/CHR) | Direct cite of ARC §8.2; all named mechanisms exist in the V1 spec. | KEEP. |
| `COMPILER.md:670–678` (§9.2 ARC §12.1 mismatch, surfaced) | The mismatch is *itself a Lens H finding* that the spec author surfaces in §9.2. The brief states "JSON is host-fn-free in the skinny per ARC §12.1" but ARC §12.1 is the YAML walkthrough, not a JSON declaration. | KEEP — Lens H clean because the contradiction is acknowledged. |

Lens H verdict: **honoured**. KEEP=4, REINVENT=0, DISCARD=0. The §9.2 self-surface is exactly the Lens H discipline working — the spec author flagged the source-authority mismatch instead of papering over it.

---

## §15 Lens I — Contrivance / Over-engineering

Pathologies: speculative generality, cardinality bloat, premature optimization, double-tracking, unused parameter axes, apparatus chains.

| Site (path:line) | Pathology candidate | Verdict |
|---|---|---|
| `COMPILER.md:268–279` (§4.3 `TypeFacts` with subst + obligations + node_types + rule_types) | Four fields. Could `subst` be folded into `rule_types`? Defeated: `subst` is the unifier substitution, distinct from materialised types; folding loses diagnostic information. | KEEP — survives steelman. |
| `COMPILER.md:329–336` (§4.4 `LayoutFacts` with rule_types + node_types + layout_policies) | The skinny's `layout_policies` is `HashMap::new()` always (no `@layout` in JSON). Could the skinny ship a `LayoutFacts` without the policies field at all? | REINVENT — borderline. The V1-shape preservation argument (the wrapper at graduation should not need to add the field) defeats the contrivance challenge. KEEP. |
| `COMPILER.md:189–203` (§3.1 14-variant BIR enumeration) | Each row has a JSON site + skinny notes. No row redundant; `SpanMark { Start | End }` could fold but ARC §7.2 ratifies it as one variant with Start/End discriminator. | KEEP — directly traceable. |
| `COMPILER.md:443–462` (§5.5 hand-curated shapes as ~80 LOC) | The shapes table is hand-coded with 9 entries (root + value + object + array + pair + string + number — wait, 8 entries listed). The full V1 miner is several thousand LOC. Is the 80-LOC fixture *too much*? | KEEP — 9 entries x ~9 LOC/entry ≈ 80 LOC; the fixture is what it is. |
| `COMPILER.md:411–417` (§5.4 hand-curated recognizer ~40 LOC) | Single nomination function. Could be inlined into `passes::extract`. | KEEP — keeping the file separate makes the deletion gate trivial; folding it would lose the deletion-gate seam. |

Lens I verdict: **honoured**. KEEP=4, REINVENT=1, DISCARD=0.

---

## §16 Lens J — Host-Language Leverage

Pathologies: memory management invented, generics + monomorphisation invented, type checking redundancy, concurrency invented, pattern matching not leveraging host-match, standard library reinvention, diagnostic / error infrastructure invented.

| Site (path:line) | Item | Verdict |
|---|---|---|
| `COMPILER.md:267–278` (§4.3 Rust types `Type::Var`, `Type::Rule`, etc.) | Standard sum-type Algorithm-W shape using Rust enum + Box. Direct host leverage. | KEEP. |
| `COMPILER.md:483–488` (§6.1 generated Rust per BIR variant) | Emitted Rust uses `match`, `loop`, `Result`, `Option` directly. No invented matching machinery. | KEEP. |
| `COMPILER.md:550–552` (§6.3 `ParserState`) | Uses `&'i [u8]`, `usize`, `Tape<'i>`, `Option<StructuralIndex>`. Standard Rust shapes. | KEEP. |
| `COMPILER.md:518–523` (§6.3 emitted parser entry sketch) | Uses `proc_macro2::TokenStream` (line 397 §5.2 row) — Rust's standard procmacro shape. | KEEP. |

Lens J verdict: **honoured**. KEEP=2, REINVENT=0, DISCARD=0. Compiler quadrant leverages Rust's type system, sum-type pattern matching, lifetime parameters, and procmacro infrastructure cleanly.

---

## §17 Lens K — Meta-Grammar Discipline

Pathologies: generating a language vs generating parsers, self-hosting drives complexity, runtime complexity, optimization complexity exceeding meta-grammar mandate, telemetry-driven schema source.

| Site (path:line) | Item | Verdict |
|---|---|---|
| `COMPILER.md:31–119` (§1 grammar-source decisions) | Skinny is a meta-grammar generating a JSON parser. The host-fn-free decision (§1.3) tests whether meta-grammar correctness requires host functions for JSON. The answer is: yes for V1 (numeric/string decode), no for skinny (decode moves to substrate). The deviation is at meta-grammar boundary. | KEEP — meta-grammar discipline preserved; the deviation is documented. |
| `COMPILER.md:235–250` (§4 HM-only) | The skinny tests whether meta-grammar correctness for JSON requires DK13/GADT/CSP. The answer is: no. The cut is JSON-FAITHFUL. | KEEP. |
| `COMPILER.md:404–410` (§5.3 skipped phases) | Recognizer mining + egraph + CSP extraction + cost extraction + VM replay all skipped. ARC §10.1 classifies `cost-driven-rewrites` as ASPIRATIONAL for V1 SOTA. The skinny's stub is ASPIRATIONAL-aligned. | KEEP — meta-grammar mandate preserved; aspirational apparatus deferred. |
| `COMPILER.md:259` (§4.2 row "Schema-mining miner") | "Skinny ships skinny-only hand-curated shapes for the JSON typed root (§5)." Telemetry-driven miner replaced by table; the table is the meta-grammar's V1-substitute for the schema source. | KEEP — telemetry source replaced explicitly. |
| `COMPILER.md:96–117` (§1.3 host-fn-free) | The host-fn-free deviation **removes** the meta-grammar's host call from the skinny grammar. Per Lens K, this is a meta-grammar **deletion**. The challenge: does the skinny still satisfy meta-grammar discipline if the V1 grammar's host call disappears at the skinny level? | REINVENT — the skinny's meta-grammar surface admits `@host fn` (§5.2 BBNF parse row says "six-directive vocabulary enough to reject non-skinny directives"), so the *parser* admits host-fn syntax even though the JSON grammar uses none. This is correct meta-grammar discipline: the skinny does not change the meta-grammar surface, only the JSON grammar's use of it. The spec should be more explicit at §1.3 that the meta-grammar surface is unchanged; only the JSON cell of the per-grammar table changes. |

**Surgery (Lens K)**: §1.3 add closing sentence: "The skinny's BBNF parser admits the full six-directive surface (`@import`, `@host fn`, `@error`, `@layout`, `@pretty`, `@token`); the host-fn-free decision applies only to the JSON grammar's *use* of `@host fn`, not to the meta-grammar's *admission* of it. Tranche D restores JSON's `@host fn decode_string_to_arena` without modifying the BBNF parser."

Lens K verdict: **honoured-with-recommendation**. KEEP=3, REINVENT=1, DISCARD=0.

---

## §18 Lens L — Premise Fidelity (load-bearing for COMPILER quadrant)

For each documented skinny omission, classify as JSON-FAITHFUL / FAITHFUL with V1-grammar caveat / MASKING.

| Site | Omission | Classification | Bench-recoverable signal | Verdict |
|---|---|---|---|---|
| `COMPILER.md:144` (§2.2 `Predicate`) | JSON has no lookahead | JSON-FAITHFUL | None needed (zero throughput interaction) | FAITHFUL |
| `COMPILER.md:145` (§2.2 `Lookbehind`) | JSON has no lookbehind | JSON-FAITHFUL | None needed | FAITHFUL |
| `COMPILER.md:146` (§2.2 `Call (kind: Map)`) | Skinny grammar drops `-> f64`, etc. | JSON-FAITHFUL with **caveat**: scalar decode runs lazily at access time, not parse time | The bench measures parse time; access-time decode is post-bench. The deviation is "favorable" per the row. | FAITHFUL — but the spec should add: "Bench rows that materialise typed values (e.g., access twitter's status text fields) would show the access-time cost; SOTA bench rows do not." |
| `COMPILER.md:147` (§2.2 `Call (kind: Host)`) | Host-fn-free | **STALE row** — needs to match §1.3 + BENCH §7.8.1 | Two-probe structure (dispatch ≤ 50 ns + eager-decode per-corpus band) | MASKING-pending; **REINVENT** the row text per §5 surgery. |
| `COMPILER.md:148` (§2.2 `LayoutDirective`) | Whitespace desugared to `ws` rule | JSON-FAITHFUL: the desugared `ws` rule lowers to a tight scalar loop, same emitted code shape as a layout policy push/pop | The bench's per-corpus throughput row recovers the cost; whitespace handling is on the hot path | FAITHFUL |
| `COMPILER.md:149` (§2.2 `ErrorDirective`) | JSON has no `@error` recovery | JSON-FAITHFUL | None needed (SOTA inputs are valid) | FAITHFUL |
| `COMPILER.md:209` (§3.2 `Alt { mode: Speculative }`) | JSON has zero non-disjoint alts | JSON-FAITHFUL | The full V1 Alt payload still carries the discriminator (§3.2 row); skinny extractor always picks Dispatch. The bench cannot distinguish (no Speculative path is exercised on JSON). | FAITHFUL |
| `COMPILER.md:210` (§3.2 `PrattSpine`) | JSON has no operator precedence | JSON-FAITHFUL with V1-grammar caveat: math, sheets, css_l4 all need Pratt | FAITHFUL with V1-grammar caveat |
| `COMPILER.md:211` (§3.2 `CallHost`) | Host-fn-free | **STALE row** — needs to match §1.3 | Same as §2.2 row | MASKING-pending; **REINVENT** per §5 surgery. |
| `COMPILER.md:212` (§3.2 `LayoutScope`) | Whitespace desugared | JSON-FAITHFUL — same emitted code shape | FAITHFUL |
| `COMPILER.md:213` (§3.2 `ErrorRecover`) | JSON has no `@error` | JSON-FAITHFUL | FAITHFUL |
| `COMPILER.md:214` (§3.2 `PathEval`) | Skinny does not link `path-core` | JSON-FAITHFUL — path is post-parse | FAITHFUL |
| `COMPILER.md:215` (§3.2 `DebugMark`) | Skinny disables debug profile | JSON-FAITHFUL — non-bench profile | FAITHFUL |
| `COMPILER.md:253` (§4.2 DK13) | JSON is monomorphic | JSON-FAITHFUL with V1-grammar caveat (CSS L4, Sheets, BBNF-self) — **explicit at row** | FAITHFUL with V1-grammar caveat |
| `COMPILER.md:254` (§4.2 Pierce-Turner) | JSON has zero annotations + zero chain steps | JSON-FAITHFUL — needs caveat (chains in CSS/Sheets) | FAITHFUL with V1-grammar caveat (REINVENT row to add it) |
| `COMPILER.md:255` (§4.2 coercion obligations) | JSON exposes raw spans + arena handles | JSON-FAITHFUL | FAITHFUL |
| `COMPILER.md:256` (§4.2 CSP) | Every CSP axis has zero choice for JSON | JSON-FAITHFUL — directly traceable | FAITHFUL |
| `COMPILER.md:257` (§4.2 GADT) | JSON has zero match arms | JSON-FAITHFUL with V1-grammar caveat (BBNF-self, host-chain) — **explicit at row** | FAITHFUL with V1-grammar caveat |
| `COMPILER.md:258` (§4.2 CHR improvement) | No host overload | JSON-FAITHFUL | FAITHFUL |
| `COMPILER.md:259` (§4.2 schema-mining miner) | Replaced by hand-curated table | JSON-FAITHFUL with deletion gate | FAITHFUL |
| `COMPILER.md:260` (§4.2 record narrowing) | Open shapes (read-only views) | JSON-FAITHFUL | FAITHFUL |
| `COMPILER.md:405` (§5.3 recognizer mining) | Hand-curated structural-alphabet recognizer | **MASKING-pending** until BENCH §7.8.2 alternate-plan stub confirms | MASKING-pending → FAITHFUL on confirmatory pass |
| `COMPILER.md:406` (§5.3 egraph rewrite) | No rewrites; canonical plan | **MASKING-pending** until alternate-plan stub bounds | MASKING-pending → FAITHFUL on confirmatory pass |
| `COMPILER.md:407` (§5.3 CSP extraction) | Trivial single-plan choice | JSON-FAITHFUL — every CSP axis has zero choice | FAITHFUL |
| `COMPILER.md:408` (§5.3 cost extraction) | Constant-cost | **MASKING-pending** until BENCH bounds | MASKING-pending → FAITHFUL on confirmatory pass |
| `COMPILER.md:409` (§5.3 VM replay) | No VM crate | JSON-FAITHFUL — VM is debug | FAITHFUL |
| `COMPILER.md:580–586` (§7 stubs cost-model + egraph + csp + vm + lsp + path) | Each stubbed | Per-row classification matches §5.3 above | FAITHFUL or MASKING-pending per row |

**Summary classifications**: 19 FAITHFUL (some with V1-grammar caveat), 2 STALE-text MASKING (§2.2 + §3.2 host-call rows that don't match §1.3 redress), 3 MASKING-pending (alternate-plan probes for cost-model + egraph + recognizer mining — these are correctly classified).

The §1.3 + BENCH §7.8.1 host-call probe rationale is internally consistent and survives steelman:
- The `host_call_dispatch_overhead` ≤ 50 ns/call probe bounds the *infrastructure cost*. If V1 grammars carry many `@host fn` calls per parse, the dispatch cost compounds; the probe measures the per-call cost in isolation so the implementor can scale the cost to per-grammar host-call density.
- The `host_call_eager_decode` per-corpus band probe bounds the *per-string work cost*. The expected delta is corpus-shape-dependent (5–15% twitter / 3–8% citm / <2% canada) because string density varies. A delta outside the band fails — high outliers indicate eager-decode is more expensive than expected; low outliers indicate the compiler optimised the work away (probe invalid).

The two-probe structure correctly tests **two distinct masking modes**: dispatch infrastructure vs per-call work. Neither probe is a "single 2% threshold" because:
- Dispatch is measured in ns/call (absolute), not as a percentage.
- Eager-decode is measured as a per-corpus delta band (relative + corpus-dependent), not a flat percentage.

**Steelman of the redress**: maybe the prior 2% threshold was correct because the eager-decode work is what V1 actually pays. **Defeated**: the prior threshold collapsed dispatch + work into a single number; the new structure separates them so the V1 receiver knows which lever moves. If V1 ships with `decode_string_to_arena` per ARC §12.2, the eager-decode probe predicts the V1 cost with corpus-specificity; if V1 grammars carry different host-call densities (CSS L4 colour-function chains have many per-parse), the dispatch probe scales linearly with density. The prior single-threshold could not predict either.

Lens L verdict: **AMENDMENT-REQUIRED-NARROW**. The §1.3 redress landed cleanly; the §2.2 + §3.2 rows did not propagate. Two specific row rewrites resolve the contradictions. **MASKING = 2 (stale text), MASKING-pending = 3 (correctly classified, awaiting bench).**

---

## §19 Lens M — Falsifiability (load-bearing primarily for BENCH; compiler-side secondary)

For COMPILER: the falsifiable gate is the regen-equality check (§6.4) and the BIR snapshot (§6.4 line 571).

| Site | Item | Falsifiability | Verdict |
|---|---|---|---|
| `COMPILER.md:563–567` (§6.4 regen-equality) | `cargo xtask check-json`: byte-for-byte diff against committed bytes. Any drift fails. | **Binary**. Cannot return false-positive (a diff is a diff). The diff identifies exactly what drifted. | KEEP |
| `COMPILER.md:570–572` (§6.4 BIR snapshot under `crates/ir/tests/snapshots/json.bir.snap`) | Detects BIR-shape change even when emitted Rust differs only in formatting. | Provides shape-level falsifiability orthogonal to byte-level. | KEEP |

The compiler quadrant's falsifiability is hard-binary; the bench-side threshold matrix (BENCH §6) is the load-bearing M lens, audited there.

Lens M verdict (COMPILER scope): **honoured**. KEEP=1, REINVENT=0, DISCARD=0.

---

## §20 Lens N — Graduation Mechanicality (load-bearing for COMPILER)

For each documented deviation, classify as MECHANICAL (additive code) / MECHANICAL with named inversion / ANTI-MECHANICAL (architectural rewrite).

| Site | Deviation | V1 closure path | LOC closure cost | Verdict |
|---|---|---|---|---|
| `COMPILER.md:323–342 + 653–663` (§4.4 + §9.1 layout-subroutine inversion) | HM runs as top-level skinny pass; `passes::layout` is a trivial pass-through. V1 inverts: `passes::layout` calls HM as internal subroutine. | Per WORKSPACE.md §8 line 539: "150-300 LOC wrapper; no Algorithm-W rewrite." The HM module relocates from sibling to subroutine via wrapper. The file path `passes/src/layout/types/algorithm_w.rs` is **already** under `layout/` in the skinny — the V1 closure adds the wrapper at `passes/src/layout/mod.rs` calling into `types::algorithm_w`. | 150–300 LOC | **MECHANICAL with named inversion**. The skinny's file placement (`passes/src/layout/types/algorithm_w.rs`) anticipates the V1 subroutine relationship; only the call hierarchy at `passes/src/layout/mod.rs` changes. **Steelman**: maybe the inversion forces an Algorithm-W rewrite to support the bidirectional check direction that V1 layout will need. **Defeated**: per ARC §8.2 line 1283, HM-equality + Pierce-Turner + DK13 + CSP + GADT are *separate* mechanisms; the V1 graduation adds them as **siblings** under `layout/`, not as modifications to Algorithm-W. The 150–300 LOC budget is plausible (a 50-LOC `mod.rs` + a 100-LOC bidirectional wrapper + 50-LOC integration with DK13/GADT/CSP siblings via `layout/mod.rs`'s pass orchestration). |
| `COMPILER.md:96–117 + 670–678` (§1.3 + §9.2 host-fn-free deviation) | JSON skinny grammar has no `@host fn`; V1 JSON has `decode_json_string_to_arena`. The decode path moves into SUBSTRATE for the skinny. | Tranche D adds `@host fn` surface to the JSON grammar; decode moves back to host. The skinny's BBNF parser already admits `@host fn` (Lens K finding); only the JSON grammar's *use* changes. SUBSTRATE's `decode_string` path becomes a fallback that V1 host-side decoding may or may not use. | INDEX.md ledger says "Tranche D adds `@host fn` surface; decode moves back" — quantitative LOC budget not in COMPILER but inferable: ~50 LOC for the `@host fn` body + ~30 LOC for the metadata block + the delete of SUBSTRATE's `decode_string` path. ~100 LOC total. | **MECHANICAL — additive**. The graduation adds `@host fn` and metadata; the BBNF parser does not change; the substrate `decode_string` path becomes optional. |
| `COMPILER.md:419–429 + 443–462` (§5.4 + §5.5 hand-curated fixtures + deletion gates) | `passes/src/recognizers/skinny_json.rs` (~40 LOC) and `passes/src/shapes/skinny_json.rs` (~80 LOC). | V1 graduation deletes both files when miners can produce same nominations. ~120 LOC delete + V1 miner addition (which is its own tranche). | -120 LOC delete; miner addition is V1-tranche internal. | **MECHANICAL — additive (with named delete)**. The skinny carries the deletion gate; the V1 miner is additive elsewhere (the cost is in the V1 miner's own tranche budget, not in the skinny graduation cost). |

**Steelman of MECHANICAL verdicts (defeated)**:

1. **HM hierarchy inversion**: steelman = the bidirectional layer adds expected-type propagation through every Algorithm-W rule, requiring rewrite. Defeated: per ARC §8.2, Pierce-Turner is a "synth/check distinction at every node"; this is implementable as a wrapper that passes the expected-type as an additional parameter and dispatches to either `synthesize` or `check_against_expected`. Algorithm-W's core unification logic does not change.

2. **Host-fn-free**: steelman = the SUBSTRATE's `decode_string` path becomes load-bearing for V1 grammars that do not declare `@host fn`. Defeated: ARC §12.1 admits metadata-only host route (yaml onboarding); the skinny's substrate path persists as the metadata-route's implementation, not deleted at graduation.

3. **Hand-curated fixtures**: steelman = the V1 miner cannot reproduce the hand-curated nomination because it lacks telemetry. Defeated: the skinny's nomination is `{"{}[],:\""}` for the structural alphabet — the miner's contract is to nominate structural-alphabet recognizers from grammar shape (alts that include `Literal { single byte }` arms). JSON's grammar has 7 such alts (`{`, `}`, `[`, `]`, `,`, `:`, `"`); the miner reads them off without telemetry.

Lens N verdict: **honoured (all MECHANICAL)**. MECHANICAL=3, ANTI-MECHANICAL=0. The graduation cost in LOC is bounded at ≤ 500 LOC additive + ~120 LOC delete — comfortably less than the skinny's compiler-quadrant budget of 4,400 LOC. **Graduation is mechanically cheap.**

---

## §21 Punch List

Ordered surgical edits to apply BEFORE COMPILER.md advances.

| # | File:line | Verbatim edit | Source verdict | Owner | Scope | Lane(s) |
|---|---|---|---|---|---|---|
| 1 | `COMPILER.md:147` | Replace existing row with: "\| `Call` (`kind: Host`) \| Skinny is host-fn-free. \| **MASKING-pending**: BENCH §7.8.1 emits two host-call probes — (a) `host_call_dispatch_overhead` (per-call microbench, target ≤ 50 ns/call) measuring `CallHost` registry indirection vs direct call, and (b) `host_call_eager_decode` (gross-time JSON variant, expected delta band 5-15% twitter / 3-8% citm / <2% canada). FAITHFUL only after both probes return within threshold per RESULTS.md. \|" | REINVENT | Compiler quadrant author | Per-row cell rewrite | Lane 3, Lens F, Lens L |
| 2 | `COMPILER.md:211` | Replace existing row with: "\| `CallHost` \| Skinny is host-fn-free. \| Not emitted in the main skinny parser. BENCH §7.8.1 emits two host-call probes (dispatch overhead + eager-decode gross-time) so both masking modes are quantified before RESULTS can claim FAITHFUL. \|" | REINVENT | Compiler quadrant author | Per-row cell rewrite | Lane 3, Lens F, Lens L |
| 3 | `COMPILER.md:246` | Change "Internal to `passes::types`" to "Internal to `passes::layout::types`" for naming consistency with §4.3 (line 264) and §4.5 (line 360). | REINVENT | Compiler quadrant author | One-word edit | Lane 3 |
| 4 | `COMPILER.md:371–385` | Reconcile §5.1 phase count: heading says "skinny runs 8" but diagram + §5.2 table list 9 phases. Either (a) change heading to "skinny runs 9" if regen equality is a phase, or (b) move "regen equality" from §5.2 to a sub-section titled "post-pipeline gates" and keep §5.1 at 8. | REINVENT | Compiler quadrant author | Heading or section move | Lane 3 |
| 5 | `COMPILER.md:413–415` (§5.4) | After "This is a skinny-only fixture, not a generic recognizer miner.", append: "Lock 14 waiver per `INDEX.md` §'What the skinny is testing'; deletion gate at V1 graduation deletes this file once `passes::recognizers` can nominate the same site from grammar shape (the structural alphabet `{}[],:\"` derives from JSON's literal-arm Alts without telemetry). See `WORKSPACE.md` §8 migration parity matrix for graduation cost." | REINVENT | Compiler quadrant author | Append paragraph | Lane 5, Lens N |
| 6 | `COMPILER.md:443` (§5.5) | Same waiver-cite append as item 5. | REINVENT | Compiler quadrant author | Append paragraph | Lane 5, Lens N |
| 7 | `COMPILER.md:117` (§1.3 closing) | Append: "The skinny's BBNF parser admits the full six-directive surface (`@import`, `@host fn`, `@error`, `@layout`, `@pretty`, `@token`); the host-fn-free decision applies only to JSON's *use* of `@host fn`, not the meta-grammar's *admission* of it. Tranche D restores JSON's `@host fn decode_json_string_to_arena` without modifying the BBNF parser." | REINVENT | Compiler quadrant author | Append clarification | Lens K |
| 8 | `COMPILER.md:254` (§4.2 Pierce-Turner row) | Append at row end: "JSON-FAITHFUL; CSS L4 / Sheets / BBNF-self chain steps need check direction at V1 graduation." | REINVENT | Compiler quadrant author | Per-row cell append | Lens G, Lens L |
| 9 | `COMPILER.md:227` (§3.3 invariant 5 "VM can replay") | Append after "stubbed; no replay invariant.": "V1 receiver: Tranche I when the `vm` crate is implemented." | REINVENT | Compiler quadrant author | Append carry-receiver | Lane 8 |
| 10 | `COMPILER.md:392` (§5.2 BBNF-parse row) | Append: "Verbatim diagnostic for `BBNF-DIRECTIVE-NOT-IN-SKINNY`: 'skinny grammar surface admits {`@import`, `@host fn`, `@error`, `@layout`, `@pretty`, `@token`} but the JSON grammar uses none of them. Encountered `<directive>` at `<span>`.'" | REINVENT | Compiler quadrant author | Append diagnostic string | Lane 7 |
| 11 | `COMPILER.md:564` (§6.4 regen-equality) | Append cite: "Diagnostic code: `BBNF-CODEGEN-REGEN-EQUALITY` per ARC §7.4 line 1077." | REINVENT | Compiler quadrant author | Append cite | Lane 7 |

Total surgical edits: 11. None are DISCARD. All are REINVENT (current text exists but carries surplus con or stale text).

---

## §22 Final Readiness

> **Decision: SK-AMENDMENT-REQUIRED-NARROW**
>
> The COMPILER quadrant survives the SK-V1 audit at the architectural level: the §1.3 redress landed cleanly; the two-probe host-call rationale is internally consistent and bench-recoverable; the §4.4 + §9.1 HM hierarchy inversion is MECHANICAL with named inversion at 150–300 LOC closure cost; every documented skinny scope cut classifies FAITHFUL or MASKING-pending against a named bench probe (BENCH §7.8.1 host-call probes; BENCH §7.8.2 alternate-plan probes). The Lens N graduation discipline holds.
>
> The audit identifies **two stale-text MASKING faults** at §2.2 row 147 and §3.2 row 211: both rows reference the pre-redress "2% median" threshold or singular "one-host-fn variant" wording that contradicts the §1.3 + BENCH §7.8.1 two-probe structure. These contradictions are tactical (cell-level rewrites), not architectural. The remaining nine punch-list items are clarifying surgeries (Lock 14 waiver cites, naming consistency, phase-count reconciliation, missing diagnostic strings, missing carry receivers).
>
> The compiler quadrant's load-bearing claim — "every cut in this spec is a cut to compiler-side machinery whose absence cannot lower the ceiling" — survives the steelman because each cut is hooked to a bench probe that bounds it. The host-fn-free decision passes Lens K (meta-grammar surface unchanged), Lens N (additive closure), and Lens L (two-probe bench-recoverable signal). The HM-only type checker passes Lens L (JSON-FAITHFUL) with V1-grammar caveats explicit at the row level for DK13 + GADT (line 253 + line 257) and missing only at the Pierce-Turner row (item 8 in the punch list).
>
> Hereupon: **dispatch the AMENDMENT-NARROW agent against the 11-item punch list before SK-V2 SUITE pass**. The amendments are mechanical text edits at known lines; no architectural redesign. After amendment, COMPILER quadrant promotes to SK-READY individually; final SK-READY promotion requires the SKINNY-SUITE consolidated pass to verify the cross-quadrant invariants (especially that BENCH §7.8.1 + §7.8.2 + §6 still match the COMPILER quadrant's per-cut classifications post-amendment).

---

**Audit time**: ~36 minutes of the 40-minute hard cap consumed at commit. KEEP totals: 51 across all lanes/lenses; REINVENT totals: 11; DISCARD: 0; FAITHFUL: 5 named with V1-grammar caveats explicit; MASKING-pending: 3 (correctly classified, awaiting bench); MECHANICAL: 3 (graduation cost ≤ 500 LOC additive). KEEP-fraction: 70% — within the healthy band.
