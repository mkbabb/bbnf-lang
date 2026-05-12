# HARDENING-WORKSPACE-SK-V2

## §1 Target identification

- **Path**: `restart/skinny/WORKSPACE.md`
- **Lines audited**: 1-614 (unchanged from SK-V1; the WORKSPACE file has NOT been amended per the SK-V1 punch list)
- **Cycle**: SK-V2 (post-iteration; user reports `bbnf-bench` at 1993/2000 LOC per `xtask lint-loc`)
- **Lens stack**: Lanes 1, 3, 4, 5, 6, 7, 8, 9 (Lane 2 N/A — single-wave); Lenses F, G, H, I, J, K; Lenses L, M, N
- **Time consumed**: ~36 min (commit-pace per skinny HARDENING.md §9)
- **Predecessor**: `HARDENING-WORKSPACE-SK-V1.md` returned `SK-AMENDMENT-REQUIRED-NARROW` with 16-item punch list. SK-V1 C1 predicted `bbnf-bench` would push past 2,000 LOC under the BENCH §11.1 measurement-driven decomposition.
- **Empirical state at audit**: `xtask lint-loc` reproduces user-reported result; my run at 1998/2000 LOC (user reports 1993/2000 — two-run delta is noise within a single LOC). Track 2 measures 343 LOC vs the stale ≤500 cap. `bbnf-bench` other = ~1,655 LOC.
- **CSS prior probe**: absent from disk (`crates/bbnf-bench/src/track2/` contains only `json.rs` + 1-LOC `mod.rs`).

## §2 Cohort verdict

| Lane / Lens | Verdict | KEEP | REINVENT | DISCARD | Recommendation |
|---|---|---:|---:|---:|---|
| Lane 1 — Lock-Adherence | violated-with-recommendation (carried forward) | 10 | 3 | 0 | SK-V1 punch list NOT applied. Lock 14 schema sentinels (`"skinny-none"`, `"host::primitives"`) still strain the canonical schema; Lock 13 single-child mount-points (`runtime/grammars/`, `passes/layout/`) still unratified textually. **NEW**: WORKSPACE §3 still hardcodes the four recognizer overrides; the prototype Cargo.toml mirrors them verbatim — so the strain is now load-bearing in code, not just spec. |
| Lane 2 — Sequencing | N/A | — | — | — | Single-wave skinny. |
| Lane 3 — Cohesion | violated-with-recommendation (carried forward) | 6 | 3 | 0 | Pipeline shim location drift (C17) unfixed — `bbnf` crate on disk is one file (`lib.rs`); the §7 row 4 path `crates/bbnf/src/parse/pipeline.rs` is fiction. `fixture_root` BENCH↔WORKSPACE inconsistency unfixed. |
| Lane 4 — SOTA Anchoring | honoured | 4 | 0 | 0 | Unchanged from SK-V1. |
| Lane 5 — Grammar-authoritative | violated-with-recommendation | 6 | 1 | 0 | `"host::primitives"` symbolic reference vs. inlined-stub disposition still unresolved. **NEW**: `grammars/json.bbnf` (per Cargo.toml) is the on-disk grammar; no Lock-14 fault, but the SUBSTRATE update at §3.13 carrying close-token elision and `STRING_NEEDS_UNESCAPE` flag means grammar metadata semantics should be reviewed against the lazy-decode dependency. |
| Lane 6 — Generated-Code + LOC Budget | **HONOURED with measurement amendment** (this is the load-bearing SK-V2 finding) | 7 | 1 | 0 | **SK-V1 C1 prediction empirically refuted**: `bbnf-bench` lands at 1998 LOC against the 2,000 cap; Track 2 lands at 343 LOC against the stale 500 cap. The cap held by **implementation discipline + scope cuts**, not by spec amendment. See §8 below. |
| Lane 7 — Friction Forecast | silent (carried forward; sharper now) | 0 | 0 | 0 | The empirical tightness (1998/2000) **becomes a friction surface**: a single 3-line bug fix in `parity.rs` or `report.rs` will fail `lint-loc` with no diagnostic naming the budget-cliff. SK-V1's surgery 10 (xtask exit-code table) is now more load-bearing. |
| Lane 8 — Carry & Deferral | violated-with-recommendation (carried) | 6 | 2 | 0 | Wave-granular receivers (LSP→I.W2; VM→E.W2; type-system→D) still absent. |
| Lane 9 — Greenfield Discipline | honoured | 9 | 1 | 0 | Implementation discipline upholds Lane 9: the user's two false-route invalidations (pair-token fusion, function-pointer dispatch table) are documented as REJECTED with measurement evidence in REDRESS.md §16, §17. Both rejections are "no workarounds / no contrivance" greenfield wins. |
| Lens F — LLM bias | honoured-with-rec | 0 | 1 | 0 | 90s build-time still has no provenance. **NEW**: the SK-V1 C1 prediction itself was an LLM-bias artefact (hedging-where-commitment-is-needed) — predicted "Track 2 measurement-driven would push past 2,000 LOC" without a measurement model; reality was tighter. Lens F's refutation by measurement is the healthy pattern. |
| Lens G — Overfitting | honoured | 0 | 0 | 0 | JSON-overfit is the design. |
| Lens H — Hallucination + provenance | honoured-with-rec | 0 | 1 | 0 | PASS-2:432, PASS-2:435 still cited; orchestrator should verify. The user's iteration produced **measurement-anchored provenance** that displaces the SK-V1 cap prediction. |
| Lens I — Contrivance | honoured-with-rec | 8 | 1 | 0 | `simd-scan` `avx512/` `wasm/` dead arches still carried; the on-disk `crates/simd-scan/src/lib.rs` is 584 LOC (single file, not the directory layout WORKSPACE §4.8 sketches). **NEW**: WORKSPACE §4.8 directory shape is unverified against implementation. |
| Lens J — Host-language leverage | honoured | 5 | 0 | 0 | Cargo idiomatic; `samply` native. |
| Lens K — Meta-grammar discipline | honoured | 4 | 0 | 0 | Delegated. |
| Lens L — Premise fidelity | **violated-with-load-bearing-finding** | 5 | 3 | 0 | **NEW MASKING**: REDRESS §19 + the bench result (gross_eager_decode at 57.6%/77.2%/81.9% of Track 1) means **host-fn-free is no longer FAITHFUL for V1 JSON unless V1 keeps string decode lazy**. WORKSPACE §10 row 1 still claims "Low JSON impact only if BENCH.md's one-host-fn probe keeps `CallHost` dispatch within 2 percent" — but the dispatch probe (Probe A) passed at sub-50ns while the eager-decode probe (Probe B) failed at 5-15% bands. WORKSPACE.md §10 wording is post-redress stale. |
| Lens M — Falsifiability | N/A (delegated) | — | — | — | BENCH.md owns the matrix. The bench DID return G/NO-GO on all three corpora — Lens M's load-bearing function (matrix can return NO-GO) is **empirically demonstrated**, not just specced. |
| Lens N — Graduation mechanicality | **violated-with-route-amendment-trigger** | 4 | 4 | 0 | **NEW**: The user identifies "lazy-tape route is the architectural amendment surface." This route was implicit in REDRESS §18 (12-byte skipless-token rejected pending "lazy-offset tape replacement"); REDRESS §316 commits the lever explicitly. WORKSPACE §8.1 has not added a "lazy-offset tape" deviation row. INDEX deviation ledger is also silent. |

**Final decision: SK-AMENDMENT-REQUIRED-NARROW.**

The skinny WORKSPACE survives SK-V2 with the dominant fault class shifted: the SK-V1 punch list edits **did not land** in WORKSPACE.md, but the implementation reality (1998/2000 LOC; Track 2 at 343 LOC; outcome G/NO-GO empirically reproduced) has displaced parts of the SK-V1 fault model. The amendment surface is now **(a)** propagate the LOC-cap-held-empirically finding into WORKSPACE so the spec stops claiming 2,000 LOC with Track 2 ≤ 500 as a budget headline rather than a measured outcome; **(b)** add the lazy-tape deviation row to §8.1 + INDEX; **(c)** update §10 row 1 + row 9 to reflect REDRESS §19's eager-decode MASKING evidence; **(d)** carry forward the SK-V1 punch list items that remain valid (C10 is fixed; C14, C17, C18 are not).

---

## §3 Lane 1 — Lock-Adherence

### Lock-by-lock walk (carried forward + delta)

| Lock | Status pre-iteration (SK-V1) | Status post-iteration (SK-V2) | Delta |
|---|---|---|---|
| Lock 1 (tape substrate) | honoured via SUBSTRATE delegation | honoured + **REDRESS commits private-Vec sealing** | Lock 1 is now sharper-than-SK-V1; SUBSTRATE §1.2 + REDRESS §15 record the inversion |
| Lock 2 (HM is producer; HM in `passes::layout`) | honoured-via-inversion (deviation ledger row 7) | **same; iteration did not touch HM hierarchy** | No delta |
| Lock 5 (Backend trait) | honoured (Rust-only; `wasm = false`) | same | No delta |
| Lock 6 (no proc-macros) | honoured | same; codegen is committed Rust source | No delta |
| Lock 8 (SOTA anchors) | delegated to BENCH | delegated; bench has run and returns NO-GO | The Lock 8 question is now **empirically open** — the substrate is not yet sonic-class |
| Lock 13 (4-10 children) | honoured at counts; single-child mount-points unratified | same; **NEW** — on-disk `simd-scan/src/lib.rs` is one file, not the §4.8 directory tree | SK-V2 surgery surfaces: WORKSPACE §4.8 either describes V1 shape (mining target) and should say so, OR §4.8 is wrong about the skinny shape |
| Lock 14 (full grammar generalization; metadata + grammar + optional declaration crate) | sentinels strain schema; declaration-crate fence empty | same | C14 still open |

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| §3 line 137-186 (Cargo.toml) | `[workspace.metadata.bbnf]` ratified on disk: `skinny/Cargo.toml:52` carries `host_registry = "skinny-none"` verbatim | Schema sentinel propagated to live config | Honest carry of the deviation. | The validator in `grammar` (per §4.2) must accept the sentinel; the on-disk `crates/grammar/src/lib.rs` is 427 LOC — does it carry the schema validator at all? Unverified. Per SK-V1 surgery 8 the recommended surgery is a 30-LOC validator extension; not landed. | Steelman: `Cargo.toml` is read by `cargo` itself, not by `bbnf-grammar`. The `workspace.metadata.*` block is opaque to cargo; the validator's job is only to read it. As long as the bench harness reads the block via `cargo metadata --format-version 1`, the validator's status doesn't block the bench. Counter: the C18 surgery is about consumer reading the value, not cargo's parsing. KEEP under "validator status is partial-crate scope; not blocking bench"; defer to SK-V3 with the partial-crate sharpening. | KEEP-with-deferral |
| §3 line 150 (`default_registry = "host::primitives"`) | References a path that does not exist in the skinny crate set | Symbolic value matches V1 ARCH §5 | The path `host::primitives` is not present in any skinny crate (`host_stubs` is 50 LOC in `bbnf::host_stubs` per §1.1 row 4) | Steelman: symbol-only; validator does not look up paths. Counter: SK-V1 surgery 7 named this; not landed. Still REINVENT. | REINVENT (carry from SK-V1 #7) |
| §3 lines 170-175 (recognizer overrides) | Four enum-string overrides not in ARCH §5 canonical schema | Pins skinny choices | ARCH §5 says `auto` is canonical | SK-V1 surgery 9 named this; not landed. Reality: the live `Cargo.toml` has `pratt = "off"`, `simd = "json-structural-always"` etc. — the strain is now load-bearing in code. SK-V2 amendment must commit to one mechanism. | REINVENT (carry from SK-V1 #9; sharpened) |
| §4.4 (`passes/src/layout/types/`) | Single-child mount-point under `layout/` | Mirrors V1 `passes::layout` path | Lock 13 strain unratified textually | Skinny crate on disk: `crates/passes/src/lib.rs` is 418 LOC SINGLE FILE — neither the §4.4 layout nor any subdirectory exists yet. The spec describes the V1-shape, not the current skinny prototype. SK-V2 must clarify whether §4.4 is "V1 destination shape" or "skinny implementation shape." | REINVENT (sharpened: spec-vs-reality drift now apparent) |
| §4.6 (`runtime/src/grammars/`) | Single-child mount-point | Lock 14 generation-target shape | Same as above | KEEP under "deliberate mount-point" reasoning, **but the spec should name it**. | REINVENT (carry from SK-V1 #14) |
| §4.8 (`simd-scan/src/`) | Seven children including dead `avx512/` `wasm/` | "For parity" framing | Live on-disk `simd-scan/src/lib.rs` is 584 LOC SINGLE FILE | Steelman: the skinny mines the published `crates/simd-scan` verbatim; if that crate's V1 shape is the directory tree, the skinny inherits. The prototype's collapse to one file is then a temporary skinny-implementation discipline (consolidate to ship), and the §4.8 spec describes the V1 shape post-graduation. Surgery: §4.8 should disclose "skinny may begin as single-file `lib.rs` and expand into the directory tree before V1 graduation." | REINVENT (new SK-V2 finding) |

**Lane 1 verdict: violated-with-recommendation (4 REINVENT, 10 KEEP).** The SK-V1 Lock-13 findings carry forward, plus one new SK-V2 finding: spec directory layouts (§4.4, §4.6, §4.8) describe shapes the on-disk skinny does not have yet. WORKSPACE.md should disclose this is V1-destination shape with a skinny implementation deferral.

---

## §4 Lane 2 — Sequencing Discipline

N/A. Single-wave skinny. Reports as N/A per skinny HARDENING §4.

---

## §5 Lane 3 — Cohesion

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| §1.1 row 11 (line 55) vs §7 row 4 (line 503) | `pipeline` shim location | Two locations cited | Two receivers; mechanical migration | §1.1: "inlined into `xtask::regen` + `bbnf::compile`"; §7: "`crates/bbnf/src/parse/pipeline.rs` + `xtask/src/main.rs::regen`". Disagreement. **Reality**: on disk `crates/bbnf/src/` is single file `lib.rs` (no `parse/pipeline.rs`); `xtask/src/main.rs` exists; `bbnf-bench/src/probes.rs` orchestrates the parse-validate-emit chain via direct calls. The §7 path is fiction. | Surgery: standardise on one phrasing OR settle a third (the empirical one — bench harness orchestrates without a `bbnf::compile`). | REINVENT (carry from SK-V1 #12; sharpened with empirical evidence) |
| §3 line 139 vs BENCH §3.2 | `fixture_root` path inconsistency | WORKSPACE says `crates/test-fixtures/corpus`; BENCH says `tests/fixtures/json/` | Two different paths | On disk: `skinny/crates/test-fixtures/` exists (376 LOC); BENCH fixture path resolution not verified | KEEP via "two-stage path: manifest dir + corpus dir" rationale (SK-V1 surgery 10); spec should name it | REINVENT (carry from SK-V1 #10) |
| §10 row 1 (line 578) | "Low JSON impact only if BENCH.md's one-host-fn probe keeps `CallHost` dispatch within 2 percent of the direct path" | Cohesion against the post-redress BENCH | "2 percent" is the **pre-redress phrasing**. REDRESS §19 commits two probes (dispatch + eager-decode); BENCH §7.8.1 ratifies. The 2% number is gone. Cohesion fault. | Steelman: §10 row 1 is the impact-statement, which can fold both probes into one "low JSON impact only if both probes pass." But the verbatim "2 percent" anchors to the pre-redress single-probe model. **The same fault is C20 at COMPILER §2.2 line 147; here it is at WORKSPACE §10**. | REINVENT (NEW SK-V2 finding — Lens L sister) |
| §10 row 9 (line 586) | "Host fns + chains" cell: "JSON-FAITHFUL only after the one-host-fn probe passes" | Singular probe | Same. Should be "after both host-call probes pass: dispatch overhead ≤50ns AND eager-decode bands defensible." | Same as above. | REINVENT (NEW SK-V2 finding) |
| §11 line 612 | "Bench competitor crates (`sonic-rs`, `simd-json`) are dev-dependencies of `bbnf-bench`. They are not workspace.dependencies. Owned by BENCH.md." | Delegation | KEEP from SK-V1 | `serde_json` in workspace.deps (line 130) duplicates BENCH-side pin. SK-V1 surgery 13 not landed. | REINVENT (carry from SK-V1 #13) |
| §8 (mining plan rows) | Mining sources for V1 destinations | Each row cites legacy crate | Live in `crates/core/src/imports/`, `crates/bootstrap/`, etc.; mining plan unchanged | KEEP | KEEP |
| §1 row 9 (bbnf-bench description) | "Criterion harness, reproducibility schema emitter, parity matrix runner, and the ≤500 LOC Track 2 handwritten substrate probe" | Stale ≤500 cap | BENCH.md §1.2 dropped this cap. Surgery: strike "≤500 LOC". Sharpened by empirical: Track 2 lands at 343 LOC anyway, so the cap was non-binding. | Steelman: SK-V1 said the cap was stale (carry forward). SK-V2 sharpens: empirical Track 2 = 343 LOC means the cap was **non-binding in practice**. Why was it dropped, then? Because BENCH redesigned Track 2 as a substrate-correspondence checklist artefact, not a cap-bound parser. The cap dropped for the right reason (correspondence is what matters), not for the wrong one (would have overrun). Surgery: WORKSPACE §1.1 row 9 strike "≤500 LOC"; replace with "the Track 2 handwritten substrate probe (substrate-API correspondence per BENCH.md §10.6)". | REINVENT (carry from SK-V1 #1; status: still not landed) |

**Lane 3 verdict: violated-with-recommendation (3 REINVENT, 6 KEEP).** Three new SK-V2 cohesion findings: the `pipeline` shim path is fiction (no `crates/bbnf/src/parse/pipeline.rs`); the §10 row 1 + row 9 are pre-redress phrasing referencing a single probe + 2% threshold. The SK-V1 cohesion items remain unfixed.

---

## §6 Lane 4 — SOTA Anchoring

WORKSPACE owns no parse-throughput gates. Delegated to BENCH.md.

| Site | Item | Pros | Cons | Verdict |
|---|---|---|---|---|
| §5 line 444 + §9 line 553 | "≤90s clean release on M1 Pro" | Engineering iteration ceiling | Lens F: no provenance | KEEP (engineering); SK-V1 surgery 15 not landed |
| §3.1 line 226 | `debug = true`, `strip = false` | Honours samply rule | None | KEEP |
| §1.1 row 7 (`cost-model` skipped) | Routes deferral to BENCH alt-plan probes | Lock 8 via BENCH delegation | None | KEEP |
| §10 table | Per-omission impact statements | Lens L premise-fidelity honour | Three rows (§10 row 1, row 9, plus row 4's pre-redress single-probe phrasing) need post-redress refresh | KEEP at lane (delegated) |

**Lane 4 verdict: honoured.** Build-time gate (90s) carries no Lock 8 claim; correctly engineering-only.

---

## §7 Lane 5 — Grammar-Authoritative Discipline

| Site | Item | Verdict |
|---|---|---|
| §3 line 153 | `[workspace.metadata.bbnf.grammars.json]` | KEEP (single grammar; metadata-only onboarding) |
| §4.6 line 320 | `runtime/src/grammars/json/` per-grammar generated subdir | KEEP (Lock 14 permits per-grammar generated modules from single template) |
| §3 line 150 | `host_fns.default_registry = "host::primitives"` | REINVENT (carry from SK-V1 surgery 7) |
| §4 per-crate `src/` listings | No grammar-named modules in generic crates | KEEP — `grep`/inspection confirms: `crates/bbnf-bench/src/track2/json.rs` is the only grammar-named file; this is ratified (Track 2 is hand-coded JSON for the substrate ceiling probe). Empirically verified at 343 LOC. |
| `match Json => ... CssL4 => ...` grep | Per skinny HARDENING §5 Lane 5 | KEEP — WORKSPACE.md contains no such match arms. Spot-checked. |
| §1.1 row 4 + §3 line 150 + §7 row 3 | `host_stubs` grammar-agnostic | KEEP |

**Lane 5 verdict: violated-with-recommendation (1 REINVENT, 5 KEEP).** Same `"host::primitives"` finding as SK-V1.

---

## §8 Lane 6 — Generated-Code + LOC Budget (LOAD-BEARING for SK-V2)

### 8.1 The empirical LOC reconciliation — SK-V1 C1 prediction outcome

| Slot | Spec (WORKSPACE.md §2) | SK-V1 C1 prediction | Empirical (post-iteration) | Reconciliation |
|---|---:|---|---:|---|
| `bbnf` | 600 | — | 90 | Massively under; spec budget is V1-destination shape |
| `grammar` | 3,500 | — | 389 | Same |
| `ir` | 2,500 | — | 491 | Same |
| `passes` | 6,000 | "binding signal if overrun" | 377 | Massively under; HM-only constraint not yet stressed (skinny stops short of full HM; runs a hand-curated lowering through codegen templates) |
| `codegen` | 4,500 | — | 978 | Under |
| `runtime` | 4,000 | — | 1,142 | Under |
| `parse-that-regex` | 4,000 | — | 424 | Massively under (regex is inline-shaped not full HIR/NFA/DFA/VM) |
| `simd-scan` | 3,500 | — | 520 | Under (skinny did not mine the 2,607-LOC carry) |
| `bbnf-bench` | 2,000 | **predicted to overrun** | **1,998** | **Cap HELD empirically** |
| `test-fixtures` | 800 | — | 331 | Under |
| `xtask` | ~250 | — | 198 | Under (spec 350 in implementation; 250 in WORKSPACE.md) |
| Generated `runtime/src/grammars/json/` | ≤4,000 | — | 809 | Massively under (codegen template is leaner than projected) |
| Track 2 | ≤500 (stale cap) | "800-1,500 measurement-driven" | 343 | **BOTH the stale cap AND the measurement-driven projection wrong** — Track 2 came in tighter than either |
| Total handwritten | 31,400 | — | ~5,000 (rough sum) | The skinny is roughly **16% of WORKSPACE-projected handwritten LOC** |

### 8.2 SK-V1 C1 disposition

**SK-V1 C1 prediction: REFUTED.**

The C1 claim was: "BENCH §11.1 dropped the Track 2 ≤500 LOC cap (now measurement-driven, expected 800-1,500 LOC) and added optional CSS prior probe (≤600 LOC); WORKSPACE row 9 still caps `bbnf-bench` at 2,000 LOC."

Empirically:
- Track 2 measured at **343 LOC**, well under both the stale 500-LOC cap AND the BENCH §11.1 800-1,500 LOC "measurement-driven" projection.
- `bbnf-bench` other (criterion harness + metadata + parity + gates + benches + binaries) measured at **1,655 LOC** (out of `bbnf-bench` total 1,998 minus Track 2 343).
- **CSS prior probe was NOT implemented** (no `track2/css_prior.rs` on disk).
- Without the CSS prior probe, the BENCH §11.1 ceiling decomposition (~2,200 LOC) leaves room for the cap to hold.

**The disposition is now empirical, not predictive.** The cap held by the implementation choosing **(a)** to write a leaner Track 2 than BENCH §11.1 estimated, and **(b)** to defer the CSS prior probe. Both choices are visible in `crates/bbnf-bench/src/track2/json.rs` (374 LOC including blanks/comments; LOC counter says 343) and the absent CSS prior file.

### 8.3 The empirical LOC finding: what it reveals and what it conceals

**Reveals** (Lens L sharpening):

1. The 31,400-LOC ceiling carries **massive headroom** in the prototype state (~5,000 LOC observed). The skinny implementation is the prototype, not the V1-destination shape WORKSPACE.md §2 describes. The two diverge by ~6× on handwritten LOC.
2. The cap that held tightly (`bbnf-bench`) is the one closest to its V1-destination shape — because criterion harness, metadata schema, and probe orchestration cannot shrink below their semantic minimums.
3. Track 2 at 343 LOC and the on-disk prototype at ~5,000 LOC total confirm BENCH §10.6's substrate-API-correspondence-not-LOC-cap framing: **what matters is what Track 2 calls, not how short it is**. A 343-LOC Track 2 that calls `runtime::tape::*` + `simd_scan::*` directly is substrate-correspondent; a 343-LOC Track 2 that re-implements the SIMD scan would NOT be.

**Conceals** (Lens L MASKING residue):

1. The WORKSPACE.md §2 LOC budgets describe a state the prototype is nowhere near. The spec's binding signal at §2.1 ("if HM-only is relaxed, scope is wrong; flag at lint-loc time") **cannot fire from the current prototype** — `passes` is at 377/6000 LOC, the binding-signal triggers at 6,000.
2. The bench result is G/NO-GO; the substrate is the failure surface. But the WORKSPACE LOC discipline never bound — the skinny did not run out of room. So the bench failure is **not** a workspace-side failure; it is a substrate-design failure that WORKSPACE.md cannot adjudicate.
3. The CSS prior probe was deferred without disclosure: WORKSPACE.md §10 has no row mentioning "CSS prior probe absent in current skinny; routes to V2 anti-overfit lever." The deferral is a Lens L MASKING signal if not surfaced.

### 8.4 Per-item table

| Site | Item | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|
| §1.1 row 9 line 36 | "the ≤500 LOC Track 2 handwritten substrate probe" | Stale cap | Empirically not bound; cap could be raised, removed, or framed as substrate-API correspondence per BENCH §10.6 | SK-V1 surgery 1 still valid; sharpen with: "Track 2 measured at 343 LOC against substrate-correspondence checklist". | REINVENT (carry SK-V1 #1 + sharpen) |
| §2 line 73 row `bbnf-bench` (2,000 LOC decomposition) | "Track 2 handwritten parser (≤500)" | Stale | Empirical: bbnf-bench at 1998/2000; Track 2 at 343. Re-arithmetic resolves: criterion 600 + schema 300 + parity 300 + masking-probes 200 + Track 2 343 = **1,743 LOC** total, leaving 257 LOC headroom against the 2,000-LOC cap. The arithmetic balances **after** Track 2 is set to measured-actual instead of either the stale cap or BENCH §11.1's projection. | Surgery: §2 line 73 replace "Track 2 handwritten parser (≤500)" with "Track 2 handwritten parser (~350; substrate-API correspondence checklist per BENCH §10.6)". Total `bbnf-bench` budget stays at 2,000 LOC; the measurement holds. | REINVENT (carry SK-V1 #2 with empirical resolution path) |
| §6 line 487-489 (`xtask::loc`) | "the ≤500 LOC Track 2 handwritten probe budget" | xtask STILL enforces this cap (verified: `xtask lint-loc` in implementation prints `track2 handwritten json: 343/500 LOC`) | The xtask code at `xtask/src/main.rs:84-88` enforces a 500 cap. If BENCH dropped the cap and Track 2 grows past 500 LOC (e.g., adding CSS prior probe), xtask fails it. **The xtask is the dispatch gate** — if BENCH §1.2 says no cap but xtask enforces one, BENCH is overridden in practice. | Surgery: xtask should enforce the substrate-API correspondence checklist (calls `runtime::tape::*` + `simd_scan::*` directly) rather than the LOC cap; the spec change at §6 + the xtask code change must land together. | REINVENT (carry SK-V1 #3; sharpened — now load-bearing in code) |
| §10 row 14 line 590 | "`xtask lint-loc` gates ≤4,000 JSON generated LOC and ≤500 Track 2 LOC" | Same | Same | Surgery: same | REINVENT (carry SK-V1 #4) |
| §11 lines 596-604 closure conditions | "31,400 handwritten LOC plus ≤4,000 generated LOC if and only if…" | The total still has empirical headroom | The seven closure conditions are spec-time conditions; the empirical state satisfies all seven trivially. The spec should note: "Empirically (post-iteration), the prototype consumes ~5,000 handwritten LOC of the 31,400 ceiling; the gap is V1-destination shape headroom." | Surgery: §11 add an 8th closure condition: "8. The spec's LOC budgets describe V1-destination shape; the skinny prototype lands well under each per-crate ceiling. A budget overrun in the prototype is a Lens N graduation-mechanicality concern, not a Lens L scope-wrong signal — except for `bbnf-bench` which is empirically near the cap." | REINVENT (carry SK-V1 #5 + sharpen) |
| §8.1 mechanical-closure table | Five rows; INDEX has seven | Box<[T]>-sealing row still missing | **NEW**: the user identifies "lazy-tape route is the architectural amendment surface" — neither WORKSPACE §8.1 nor INDEX deviation ledger has a row for the lazy-tape route. REDRESS §17 marks the 12-byte skipless-token as rejected pending "lazy-offset tape replacement." This is a **deferred deviation candidate** the SK-V2 should surface. | Surgery: WORKSPACE §8.1 add a 7th row: "lazy-offset tape (DEFERRED) — Skinny shape: 16-byte aligned tape with stored skip column. V1-or-substrate-amendment closure: chunked or lazy-offset tape derives skip on traversal. Estimated LOC: 300-600 in `crates/runtime/src/tape/`." Mirror in INDEX. | REINVENT (NEW SK-V2 finding) |

### 8.5 Generated LOC budget

| Site | Item | Verdict |
|---|---|---|
| §2 line 76 + §3 line 180 | ≤4,000 generated; PASS-2:432 cited | Empirical: 809/4000 LOC, ~20% utilisation. PASS-2:432 still cited; orchestrator to verify. KEEP. |
| §10 last row + xtask | Lint-loc enforcement | KEEP — gate active. |

**Lane 6 verdict: HONOURED-with-empirical-amendment (1 REINVENT, 7 KEEP).** The SK-V1 C1 prediction is empirically refuted: `bbnf-bench` LOC cap held, not overflowed. But the spec amendments to ratify this empirical reality (strike stale Track-2 cap, re-arithmetic the `bbnf-bench` decomposition) **still have not landed**. Plus one new SK-V2 finding: §8.1 missing the lazy-tape route deviation row.

---

## §9 Lane 7 — Friction Forecast

| Friction surface | User | Mental model | Confusion point | Required artefact |
|---|---|---|---|---|
| `bbnf-bench` at 1998/2000 LOC | Implementer adding a 3-line bug fix to `parity.rs` | "this is a small change, no LOC implications" | `xtask lint-loc` fails the change with no diagnostic explaining the budget-cliff is post-iteration tight | Verbatim error from xtask: `BBNF-BUDGET-CLIFF: crates/bbnf-bench at 2,001/2,000 LOC. Post-iteration headroom is exhausted (was 1,998 LOC at last measurement). Apply one of: (a) hoist orchestration logic from gate.rs to xtask::gate; (b) drop the masking-probes module if no longer needed; (c) raise the cap per SK-V2 amendment.` |
| `passes` budget at 377/6000 (massive headroom) | Implementer adding HM | "HM is a few hundred LOC; should fit easily" | The headroom is real for the partial-HM skinny; the WORKSPACE.md §2.1 "binding signal" framing never fires because the skinny scope is so much smaller than the spec's V1-destination scope projects | A spec note: "The skinny prototype lands at ~16% of the WORKSPACE.md projected handwritten LOC. The §2.1 binding signal is intended for the post-prototype, fully-implemented skinny shape. For the prototype, do not interpret a `passes` LOC below 6,000 as 'scope is right'." |
| `xtask lint-loc` exit codes | CI | Pass/fail | Spec doesn't say (still) | xtask exit-code table per SK-V1 surgery 10 |
| 90s build-time miss | Implementer | "build is slow" | §5 lists causes + surgery but no diagnostic. Empirical: `cargo xtask lint-loc` build took ~0.86s (`ax-iter` profile); no clean release build measured at audit. | Diagnostic per SK-V1 surgery 10 |
| Workspace alias package-rename | New contributor | `bbnf-grammar` vs `grammar` confusion | Same | Cookbook entry per SK-V1 surgery 10 |
| **NEW** REDRESS-iteration narrative onboarding | Implementer reading WORKSPACE.md cold | "Track 2 ≤500 LOC; pair tokens canonical; 16-byte aligned token; close tokens emitted" | All four are pre-iteration spec claims; reality: Track 2 measurement-driven not capped; pair tokens KEPT (measured-and-rejected for fusion); 16-byte aligned KEPT (12-byte rejected); close tokens NOT emitted (close-token elision adopted). The WORKSPACE.md text has **not** been amended for these. | A "post-iteration state" callout box in WORKSPACE.md §0 or §11 naming the 18 redress decisions, with cross-references to SUBSTRATE.md §3.13 for the close-token + pair-token + 16-byte adjudications |

**Lane 7 verdict: silent-must-add (carried + sharpened).** WORKSPACE.md still silent on friction surfaces. The 1998/2000 LOC budget-cliff is the new dominant friction. **Surgery 17 (NEW)**: WORKSPACE §6 add the budget-cliff diagnostic + a "post-iteration state" callout pointing implementers at REDRESS.md.

---

## §10 Lane 8 — Carry & Deferral Audit

| Site | Carry | Receiver | Blocker | Receiving gate | Verdict |
|---|---|---|---|---|---|
| §1.1 row 2 (LSP/DAP) | Tranche I | LSP/DAP/incremental | I.W? | violated (SK-V1 #11 carry) |
| §1.1 row 3 (VM) | Tranche E | BIR debug-replay | E.W? | violated (SK-V1 #11 carry) |
| §10 row 3 (type-system) | (no receiver) | "JSON's grammar is monomorphic" | (no gate) | violated (SK-V1 #11 carry) — INDEX names Tranche D; WORKSPACE doesn't |
| §10 row 1 + row 9 (host-fn-related) | (cohesion fault) | (BENCH delegation) | (probe pass) | **REINVENT (NEW SK-V2)** — pre-redress single-probe phrasing |
| §10 row 4 (cost-model + e-graph + CSP) | H.W2/H.W3 (INDEX names; WORKSPACE delegates by mechanism) | "alternate-plan probes" | (mechanism-level) | KEEP via INDEX delegation |
| §11 (open contradictions) | Synthesis pass | (per contradiction) | (per contradiction) | KEEP — four contradictions named |
| §10 CSS prior probe absence (NEW) | (no row) | (no receiver) | "skinny implementation deferred this probe" | (no gate) | **REINVENT (NEW SK-V2)** — WORKSPACE.md §10 should disclose the CSS prior probe is absent from the current prototype |
| §10 lazy-tape route (NEW) | (no row) | "substrate amendment" or "Tranche A.W1 substrate rebuild" | "REDRESS §17 + §18 reject 12-byte skipless; need lazy-offset" | (no gate) | **REINVENT (NEW SK-V2)** — WORKSPACE.md §8.1 + §10 should disclose the deferred substrate amendment surface |

**Lane 8 verdict: violated-with-recommendation (4 violated, 4 KEEP).** Two new SK-V2 carries: CSS prior probe deferral; lazy-tape route deferral.

---

## §11 Lane 9 — Greenfield Discipline

| Site | Item | Greenfield-honour | Verdict |
|---|---|---|---|
| All §1.1 cuts | Skipped V1 crates with named receivers | "No quick solutions" | KEEP |
| §3.1 line 226 | samply-symbol-resolution discipline | "Idiomatic" | KEEP |
| §4.7 | `parse-that-regex` directory promotion | "Architectural transposition" | KEEP |
| §4.8 dead arches | "for parity" framing | Lens I flagged | KEEP-at-carry; REINVENT-at-framing (SK-V1 finding) |
| §2.1 binding signal | scope-wrong-evidence framing | "Root-cause" | KEEP |
| §6 xtask discipline | small dev tool | "Idiomatic" | KEEP |
| §7 shim-discipline rule | growth-trips-graduation | "No legacy uncontested" | KEEP |
| §8 mining plan | Selective mining + greenfield rebuild for IR/runtime | "No legacy uncontested" | KEEP |
| **NEW** REDRESS evidence | 18 redress decisions, including TWO rejected routes (pair-token fusion at REDRESS §16; function-pointer dispatch table at REDRESS §17) | "No workarounds; no contrivance; measurement-driven" | **KEEP-with-commendation** — the iteration's rejected-after-measurement decisions are exemplary greenfield discipline |

**Lane 9 verdict: honoured (1 REINVENT, 9 KEEP).** The iteration's two false-route invalidations strengthen Lane 9; the spec author and implementer refused to ship measured-and-rejected perturbations. SK-V2 reaffirms this as the load-bearing posture.

---

## §12 Lenses F-K (cross-cutting)

### Lens F — LLM bias

- 90s build-time still without provenance (SK-V1 surgery 15 not landed).
- **NEW**: SK-V1 C1 itself was an LLM-bias signal — the prediction "Track 2 measurement-driven would push past 2,000 LOC" hedged with no empirical model. The user's iteration refuted it. The healthy posture: when prediction and measurement disagree, measurement wins; SK-V2 ratifies the measurement.
- SK-V1's predictive framing is the failure mode Lens F warns about. The audit caught it; the iteration corrected it; SK-V2 documents the resolution.

### Lens G — Overfitting

JSON-overfit by design (the entire skinny premise). N/A. The CSS prior probe was the anti-overfit lever; its **absence from the current prototype** is now a Lens G signal at the implementation level rather than spec level. The user has implicitly chosen "ship JSON; defer CSS prior" — which is reasonable for the 2-4-week skinny window, but should be **disclosed**.

### Lens H — Hallucination + provenance

Two carry-forward provenance citations (PASS-2:432; PASS-2:435); orchestrator to verify. No new SK-V2 hallucination findings.

### Lens I — Contrivance

- `simd-scan` dead-arch carry: still flagged.
- **NEW**: WORKSPACE §4.8 sketches a directory tree (`scalar/`, `neon/`, `avx2/`, `avx512/`, `wasm/`, `dispatch/`) that the on-disk prototype does not implement (single file `lib.rs`, 584 LOC). Contrivance at the spec level (carries a layout the implementation hasn't earned) OR honest carry of V1-destination shape (depending on how §4.8 is framed). Surgery: §4.8 should clarify "skinny may ship as `lib.rs`; V1 promotion adds the directory tree."

### Lens J — Host-language leverage

Cargo idiomatic. SK-V1 `serde_json` workspace-vs-bench-dep finding still carries (#13 not landed).

### Lens K — Meta-grammar discipline

Delegated.

---

## §13 Lens L, M, N — Skinny-specific lenses

### Lens L — Premise fidelity

The empirical bench result and REDRESS log refine the FAITHFUL / MASKING classifications:

| §10 omission | SK-V1 classification | SK-V2 reclassification | Justification |
|---|---|---|---|
| Per-grammar declaration crates | FAITHFUL | FAITHFUL | No host fns in main JSON grammar; Probe A passed sub-50ns |
| LSP / DAP / incremental | FAITHFUL | FAITHFUL | unchanged |
| GADT / DK13 / OutsideIn / CSP | FAITHFUL with V1-grammar caveat | FAITHFUL with V1-grammar caveat | unchanged — the skinny's `passes` is at 377/6000 LOC; HM-only never stressed |
| Cost-model + e-graph + CSP optimization graph | FAITHFUL-with-bound | **FAITHFUL-with-bound + ONE NEW MASKING route** | REDRESS §16-§18: three perturbations measured and rejected (pair-token fusion, function-pointer dispatch table, 12-byte skipless token). REDRESS §317: lazy-offset tape is the deferred route. The cost-model probe set bounds the **current canonical plan** vs measured alternates; it does NOT bound the lazy-offset tape route which has not been measured. This is **bound-by-current-set; unbound-on-deferred-route**. |
| Pratt auto-detection | FAITHFUL | FAITHFUL | unchanged |
| SIMD auto-detection | FAITHFUL | FAITHFUL | unchanged; REDRESS §2 ratifies the parse-index/structural-scan split |
| WASM / TS backends | FAITHFUL | FAITHFUL | unchanged |
| Path / select macros | FAITHFUL | FAITHFUL | unchanged |
| Host fns + chains | FAITHFUL conditional on probe pass | **MASKING-confirmed for V1 eager-decode** | REDRESS §19 + bench result: Probe A passes (dispatch sub-50ns); Probe B fails (eager-decode at 5-18% MASKING band exceeded on all three corpora). The host-fn-free skinny is FAITHFUL only for a V1 path that **keeps string decode lazy**. WORKSPACE.md §10 row 9 + row 1 should reflect this — currently both still cite the single-probe 2% threshold. |
| Recovery / `@error` directives | FAITHFUL | FAITHFUL | unchanged |
| Multiple grammars | FAITHFUL with V1-grammar caveat | FAITHFUL with V1-grammar caveat | unchanged |
| `egraph-derive` / proc-macro | FAITHFUL | FAITHFUL | unchanged |
| Workspace metadata cross-grammar coherence | FAITHFUL | FAITHFUL | unchanged |
| Generated LOC budget at scale | FAITHFUL-with-caveat | FAITHFUL-with-caveat | unchanged; current 809/4000 LOC |
| **NEW: CSS prior probe absence** | (not in §10) | **MASKING-without-bound** | The CSS prior probe was BENCH §11.1 + SK-V1 C1's anti-overfit lever. It's not implemented in the prototype. The skinny's JSON-only finding lacks the **only structural anti-overfit measurement** the spec proposed. Lens G says CSS prior probe is "strongest anti-overfit lever"; without it, JSON SOTA-beat results don't bound CSS L4 substrate viability. |
| **NEW: lazy-offset tape route** | (not in §10) | **DEFERRED-MASKING-CANDIDATE** | REDRESS §316 commits the lever; no measurement exists. The substrate-failure verdict from the current bench (G/NO-GO) routes here. |

**Lens L verdict: FAITHFUL with two new MASKING signals.** Both are Lens L MASKING residue the spec must surface:
1. **CSS prior probe deferred** (anti-overfit unbound).
2. **Eager-decode confirmed MASKING in measurement** (REDRESS §19; bench results).

### Lens M — Falsifiability

Delegated to BENCH. **Empirically validated**: the bench DID return outcome G/NO-GO on all three corpora. Lens M's load-bearing function (matrix can return NO-GO) is **measurement-confirmed**, not just specced. This is the strongest possible Lens M honour.

### Lens N — Graduation mechanicality

| Deviation | SK-V1 row in WORKSPACE §8.1 | SK-V2 update needed |
|---|---|---|
| HM hierarchy inversion | ✓ row 1 | No change |
| JSON host-fn-free | ✓ row 2 | **Update LOC estimate**: REDRESS §19 + bench result suggest the V1 host-fn-free closure must include lazy-decode preservation. Closure cost 150-250 LOC unchanged; add a sentence: "V1 graduation must retain lazy string decode; eager-decode probe failure binds this." |
| `parse-that-regex` directory promotion | ✓ row 3 | No change |
| `passes` HM-only constraint | ✓ row 4 | No change |
| `wasm = false` metadata | ✓ row 5 | No change |
| **Tape Box<[T]> sealing inversion** | ✗ MISSING (SK-V1 surgery 6) | Still missing. Surgery carries. |
| **HM-as-top-level inversion of Lock 2** | partial (covered by row 1) | KEEP |
| **NEW: lazy-offset tape route** | ✗ MISSING (SK-V2 finding) | Surgery: add row. "Skinny shape: 16-byte aligned tape with stored skip column (private-Vec sealed). V1 closure: chunked or lazy-offset tape rebuilds skip at traversal time. Estimated cost: 300-600 LOC in `crates/runtime/src/tape/` + Lock 1 amendment if traversal cost regresses." Sub-classification: this is **MECHANICAL-conditional-on-bench** (it might still be a substrate redesign if traversal cost regresses; but the *current* skinny code does not move). |

**Lens N verdict: MECHANICAL with seven (not five) rows and one route-conditional row.** The graduation closure cost survives steelman for the six original rows + lazy-offset tape. WORKSPACE §8.1 must mirror the seven-row INDEX deviation ledger + add a deferred lazy-tape row (or INDEX adds it and WORKSPACE mirrors).

---

## §14 Punch list (SK-V2)

Ordered surgical edits to apply BEFORE WORKSPACE.md advances to SK-V3 (or to SK-READY if these close cleanly).

| # | Target | Edit | Source verdict | Lane(s) | Status |
|---|---|---|---|---|---|
| 1 | `restart/skinny/WORKSPACE.md:36` (§1.1 row 9) | Strike "≤500 LOC"; replace with "Track 2 handwritten substrate probe (measured: 343 LOC; substrate-API correspondence per BENCH.md §10.6)" | REINVENT | Lane 6 | **Carry SK-V1 #1; sharpen with empirical value** |
| 2 | `restart/skinny/WORKSPACE.md:73` (§2 table `bbnf-bench` row) | Replace decomposition with empirical: Criterion harness ~600 + reproducibility schema ~315 + parity matrix runner + materialization + scan + probes ~640 + Track 2 ~343 = ~1,998 LOC at 2,000-LOC ceiling. Note: cap held empirically; CSS prior probe deferred. | REINVENT | Lane 6 | **Carry SK-V1 #2; sharpen** |
| 3 | `restart/skinny/WORKSPACE.md:487-489` (§6 xtask `mod loc`) + `xtask/src/main.rs:84-88` | Replace LOC-cap enforcement with substrate-API correspondence check: assert `track2/json.rs` `use`s `runtime::tape::*` + `simd_scan::*` directly. **Both the spec and the code must change together.** | REINVENT | Lane 6, Lane 7 | **Carry SK-V1 #3; sharpened (now load-bearing in code)** |
| 4 | `restart/skinny/WORKSPACE.md:590` (§10 last row) | Strike "≤500 Track 2 LOC"; replace per surgery 3 | REINVENT | Lane 6 | Carry SK-V1 #4 |
| 5 | `restart/skinny/WORKSPACE.md:596-604` (§11) | Re-arithmetic per surgery 2; add 8th closure condition naming the empirical headroom; add 9th condition naming CSS prior probe deferral. | REINVENT | Lane 6, Lens L | Carry SK-V1 #5 + sharpen |
| 6 | `restart/skinny/WORKSPACE.md:537` (§8.1 mechanical-closure table) | Add row 6: Tape Box<[T]> sealing inversion (per INDEX row 6). Add row 7: lazy-offset tape route (DEFERRED-MASKING-CANDIDATE; estimated 300-600 LOC in `runtime::tape`; substrate amendment surface per REDRESS §317). | REINVENT | Lens N, Lane 6 | **Carry SK-V1 #6; add NEW lazy-tape row** |
| 7 | `restart/skinny/WORKSPACE.md:142` (§3 `host_fns.default_registry = "host::primitives"`) + `:51` (§1.1 row 4) | Settle: either symbol-only validator (no path lookup) or rename to `"skinny-stub"`. **Sharpened**: the live `Cargo.toml` carries `host::primitives` literal; whichever resolution lands must propagate to `skinny/Cargo.toml`. | REINVENT | Lane 1, 3, 5 | Carry SK-V1 #7 (C18) |
| 8 | `restart/skinny/WORKSPACE.md:140` + `:166` (`host_registry = "skinny-none"`) | Spec the schema extension; cross-reference the `grammar` validator's TODO. | REINVENT | Lane 1 | Carry SK-V1 #8 |
| 9 | `restart/skinny/WORKSPACE.md:170-175` (§3 optimization overrides) | Pick one mechanism: (a) fold into `profile = "skinny-json-curated"` with resolver, or (b) extend ARCH §5 schema enum. | REINVENT | Lane 1, Lens I | Carry SK-V1 #9 |
| 10 | `restart/skinny/WORKSPACE.md:139` (§3 `fixture_root`) | Reconcile with BENCH §3.2 (`tests/fixtures/json/`). | REINVENT | Lane 1, 3 | Carry SK-V1 #10 |
| 11 | `restart/skinny/WORKSPACE.md:46-50` (§1.1 rows 2, 3) + `:579` (§10 row 3) | Append wave-granular receivers per INDEX cross-references. | REINVENT | Lane 8 | Carry SK-V1 #11 |
| 12 | `restart/skinny/WORKSPACE.md:55` (§1.1 row 11) + `:503` (§7 row 4) | Settle pipeline shim location. **Sharpened**: on disk `crates/bbnf/src/parse/pipeline.rs` does NOT exist; the §7 path is fiction. Recommended phrasing: "inlined as orchestration in `bbnf-bench/src/probes.rs` for the bench harness path + `xtask/src/main.rs::regen_json` for the regen path; deferred a `bbnf::compile` public function to V1 graduation." | REINVENT | Lane 3 | **Carry SK-V1 #12 (C17); sharpened with on-disk evidence** |
| 13 | `restart/skinny/WORKSPACE.md:130-132` | Move `serde_json` to dev-dep only. | REINVENT | Lane 3, Lens J | Carry SK-V1 #13 |
| 14 | `restart/skinny/WORKSPACE.md:287` (§4.4 `passes/src/layout/types/`) + `:319` (§4.6 `runtime/src/grammars/`) | Add Lock-13 ratification text for single-child mount-points. | REINVENT | Lane 1 | Carry SK-V1 #14 |
| 15 | `restart/skinny/WORKSPACE.md:444-453` (§5 build-time) | Add provenance for 90s target. | REINVENT | Lens F, Lane 4 | Carry SK-V1 #15 |
| 16 | `restart/skinny/WORKSPACE.md:600-604` (§11) | Add condition 7: cross-quadrant deviation-ledger consistency. | REINVENT | Lens N | Carry SK-V1 #16 |
| 17 | **NEW** `restart/skinny/WORKSPACE.md:578` (§10 row 1) + `:586` (§10 row 9) | Update both rows: row 1 strike "within 2 percent of the direct path"; row 9 expand to "JSON-FAITHFUL only after both probes pass: dispatch ≤50ns AND eager-decode bands per BENCH §7.8.1. REDRESS §19: eager-decode currently exceeds expected bands; the host-fn-free cut is FAITHFUL only for a V1 path that keeps string decode lazy." | REINVENT | Lane 3, Lens L | **NEW SK-V2 (replaces pre-redress wording)** |
| 18 | **NEW** `restart/skinny/WORKSPACE.md:573-591` (§10 table) | Add row: "CSS prior probe — deferred from skinny prototype. Spec'd as anti-overfit lever in BENCH §9.1 / §11.1. Risk: JSON-only result without CSS prior probe does not bound substrate viability for non-JSON grammars. Mitigation: route to V1 H tranche per multi-grammar V1-closure." | REINVENT | Lens L, Lane 8 | **NEW SK-V2** |
| 19 | **NEW** `restart/skinny/WORKSPACE.md:573-591` (§10 table) | Add row: "Lazy-offset tape route — deferred substrate amendment. REDRESS §17-§18 reject three perturbations (pair-token fusion, function-pointer dispatch table, 12-byte skipless token); §316 names the remaining lever. Bench currently NO-GO on substrate ceiling; this route is the unmeasured candidate. Impact: V1 substrate may require Lock 1 amendment if lazy-offset tape lands as the SOTA-class shape." | REINVENT | Lens L, Lens N, Lane 8 | **NEW SK-V2** |
| 20 | **NEW** `restart/skinny/WORKSPACE.md:0-22` (§0 + new §0.1) OR `:596-614` (§11) | Add a "Post-iteration state" callout: "The on-disk skinny prototype consumes ~5,000 handwritten LOC against this 31,400 ceiling; the gap is V1-destination headroom. `bbnf-bench` is the only crate near its cap (1998/2000 LOC). REDRESS.md records 18 implementation decisions, including TWO rejected routes (pair-token fusion, function-pointer dispatch table) and the bench verdict G/NO-GO on twitter / citm / canada." | REINVENT | Lane 7, Lens H | **NEW SK-V2** |
| 21 | **NEW** `xtask/src/main.rs:48-90` (lint-loc body) | Add structured diagnostic for the 2000-LOC budget-cliff: when `bbnf-bench` LOC is in [1980, 2000], print a yellow warning naming the cliff before pass; when over, print the SK-V2-surgery-17 diagnostic. | REINVENT | Lane 7 | **NEW SK-V2** |

Total: **21 surgical edits**. 16 carry from SK-V1; 5 are new SK-V2 findings (17, 18, 19, 20, 21). All mechanical.

---

## §15 Final readiness verdict

> **Decision: SK-AMENDMENT-REQUIRED-NARROW.**
>
> WORKSPACE.md SK-V2 survives the 17-lens audit (Lanes 1, 3, 4, 5, 6, 7, 8, 9 + Lenses F, G, H, I, J, K, L, M, N) with no architectural rewrites required. The dominant fault class is **doubled post-iteration drift**: the SK-V1 punch list (16 narrow edits) **did not land** before the implementation iterated, and the iteration itself introduced new amendment surfaces (CSS prior probe absent from prototype; lazy-offset tape route committed in REDRESS but absent from WORKSPACE §8.1 / INDEX deviation ledger; §10 row 1 + row 9 still carry pre-redress two-percent-host-probe wording).
>
> The SK-V1 C1 prediction (Track 2 measurement-driven would push `bbnf-bench` past 2,000 LOC) is **empirically refuted**. `xtask lint-loc` reports `bbnf-bench` at 1998/2000 LOC; Track 2 at 343/500 LOC. The cap held by **implementation discipline** (Track 2 leaner than BENCH §11.1's 800-1,500 LOC projection) plus **deferral discipline** (CSS prior probe not implemented). The 31,400-LOC handwritten ceiling carries ~26,000 LOC of headroom in the prototype state. Spec amendments to ratify this empirical reality (strike stale Track-2 cap, re-arithmetic the `bbnf-bench` row, disclose CSS prior probe deferral, disclose lazy-offset tape route deferral) remain unapplied.
>
> Lens L gains one **confirmed MASKING** signal: REDRESS §19 + the bench result document that the eager-decode probe (Probe B) exceeds its expected bands on all three corpora, so the host-fn-free cut is FAITHFUL only for a V1 JSON path that keeps string decode lazy. WORKSPACE §10 row 1 + row 9 must be updated. Lens N gains one **DEFERRED-MASKING-CANDIDATE**: the lazy-offset tape route (REDRESS §316) is committed as the remaining substrate-redesign lever but unmeasured.
>
> Lane 9 (greenfield discipline) **strengthens**: the user's iteration produced two false-route invalidations (pair-token fusion; function-pointer dispatch table) measured-and-rejected with public evidence in REDRESS. This is exemplary greenfield posture — no workarounds, no contrivance, measurement-driven scope adjudication.
>
> Lens M (falsifiability) **empirically validates**: the bench returned outcome G/NO-GO on all three corpora. The matrix's load-bearing function (can return NO-GO; is not confirmation-biased) is measurement-confirmed.
>
> Of the 21 surgical edits in §14, 16 carry from SK-V1 (none have landed) and 5 are new SK-V2 findings (post-iteration ratification, CSS prior probe + lazy-tape disclosure, §10 post-redress phrasing, xtask budget-cliff diagnostic). All are mechanical and close within a single amendment cycle.
>
> Hereupon: dispatch SK-V2 amendment cycle against the 21-item punch list. After SK-V2 amendments land, dispatch SK-V3 verification cohort (per skinny HARDENING.md §6 SK-V3+ cycle definition). The remaining substrate gap (bench G/NO-GO) is **a separate question** — the skinny WORKSPACE is now narrow-amendable-to-coherent, but the V1 SOTA-beat probability cannot rise above the current measured ceiling until the lazy-offset tape route is implemented and measured. WORKSPACE.md's job is to surface this fact in §10 + §8.1, not to resolve it; that work belongs to SUBSTRATE.md or to a Lock 1 amendment.

---

## Critical Files for Implementation

- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/WORKSPACE.md` (target; 21 surgical edits required)
- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/INDEX.md` (deviation ledger needs lazy-tape + cross-mirror per surgery 6)
- `/Users/mkbabb/Programming/bbnf-lang/skinny/xtask/src/main.rs` (lint-loc body — surgery 3 + 21; spec and code must change together)
- `/Users/mkbabb/Programming/bbnf-lang/skinny/REDRESS.md` (the load-bearing iteration evidence; spec must cross-reference §16-§19 + §316)
- `/Users/mkbabb/Programming/bbnf-lang/restart/skinny/audit/HARDENING-WORKSPACE-SK-V2.md` (the materialization target; this audit's body)
