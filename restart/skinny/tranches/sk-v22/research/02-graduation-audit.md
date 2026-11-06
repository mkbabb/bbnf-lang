# R2 — The graduation audit: skinny's gap to tree-of-record, the legacy port-in, the wave re-map

Charter: SK-V22 PASS-ALPHA. Skinny won the SK-V21 adjudications but is edition-2021, has known
in-tree walls, reaches OUT of its own workspace for grammar sources and two crate deps, and was
frozen as a differential oracle — never as the ship vehicle. This audit reads the live tree at
HEAD `f8e375be3` (skinny code untouched since the freeze; last skinny content mtime 2026-06-20) and
answers the three graduation questions: (1) what skinny LACKS, (2) what ports IN from legacy
`crates/`, (3) how the SPEC §11 manifest re-maps onto the graduation.

**The founding accounting.** Skinny is **64,751 LOC of src across 11 workspace crates** (`skinny/`,
not `restart/skinny/` which is docs). This is 30× smaller than legacy `crates/core` alone (216,818
src). The composition:

| Skinny crate | src LOC | Role |
|---|---:|---|
| bbnf-bench | 35,933 | bench/oracle harness (NOT product; largest by far, carries generated bench payloads + 18 inline-test files) |
| codegen | 10,201 | the emitter — general_descent + json_sink_direct + json_templates + css_scan_direct |
| runtime | 6,522 | tape substrate + 3 generated grammar dirs (json 2,271 / sheets 1,908 / css_l4 974) |
| parse-that-regex | 3,022 | forked scanner (Eisel-Lemire + unescape) — DELETE target per §1 addendum |
| passes | 2,750 | decision engine (egraph/csp consumers) |
| bbnf-simd | 2,324 | the NEON kernel plane (aarch64 only — the win) |
| grammar | 2,142 | the frontend (bbnf parser) |
| ir | 1,020 | lowering IR |
| test-fixtures | 404 | corpus loader |
| bbnf-regex | 322 | stub (322 LOC) — DELETE target per §1 addendum |
| bbnf | 111 | facade |

Product-carrying src (excluding the bench harness) is **~28.8K LOC**. That is the real graduation
surface, and its smallness is the thesis: the tree of record is small because 4-of-5 legacy lines
were xtask payload (research/38 §1) that regenerates for free.

---

## 1. THE GAP LIST — what skinny LACKS to be the tree of record

### G1 — The two general_descent walls + WART-C are NOT dissolved in-tree (PROTO-B1 unlanded)

PROTO-B1 proved all three dissolvable by grammar-neutral primitives (SPEC §9:489-501, 7/7 TOML
round-trip), but the proof ran in a **worktree at skinny submodule `efc6b875f`** and was never
landed. The live tree still carries every wall:

- **WALL A (whitespace hardcode) — PRESENT.** `crates/codegen/src/general_descent/emit.rs`: the
  emitted `skip_ws` (near emit.rs:123-135) is unconditional `byte.is_ascii_whitespace()` with the
  comment "uniform between sequence elements in this whitespace-insensitive grammar." No `?w`-derived
  `WsPolicy`. Newline-significant grammars (TOML) mis-parse. Wiring needed: `WsPolicy` threaded
  through `lower_program` (SPEC §9:500).
- **WALL B (leaf-regex approximation) — PRESENT.** `emit.rs` (regex_first.rs, 230 LOC) still emits a
  delimiter-scan `match_leaf` approximation, not a runtime regex interpreter. RFC3339 datetimes,
  escaped/multiline strings, `[\s\S]*?` break. `bbnf-regex` has zero matcher symbols. Wiring needed:
  the bounded interpreter swapped into the `RegexProgram` arm.
- **WART-C (the value-rule name-coupling / JSON fork) — PARTIALLY addressed, NOT folded.** The
  recognizer was re-keyed on structure (`36ca10646` / `f4633d468`): `runtime_generator.rs:98` now
  dispatches `if !sink_only.has_rule("value")` — structural, "never a grammar name." **But the
  fork itself survives**: a `value`-owning grammar still routes to the verbatim
  `json_templates/{generated,parser,value,view,visitor}.rs` + `json_sink_direct/` path (runtime_
  generator.rs:100-140), while everything else routes to `general_descent`. SPEC §9:483-486 is
  explicit that neutralization is a **JSON-into-general_descent FOLD plus json_template retirement**,
  not the sniffer swap that landed. The two emit paths are still two emit paths.

**Verdict:** the three §9 fixes are the pre-condition for TW6 (TOML as fleet-member-ten) and for the
"ONE emit path serves all grammars" TW2 close. None are in-tree. This is the single largest in-tree
gap and it is regen-wiring, not research.

### G2 — The value plane is JSON-only prototype, not a general fused/lazy substrate

The fused eager sink exists **only as a JSON-owned trait**: `crates/runtime/src/grammars/json/sink.rs`
(120 LOC, `JsonSink`), with the lazy lane in `json/view.rs` (488) + `json/value.rs` (172) over
`tape/cursor.rs` (36). There is:
- no grammar-neutral value plane (sheets has `view.rs` 279 but no sink; css_l4 has `sink.rs` 16 only);
- no serde bridge, no `StructRegistry` projection fence, no projection-totality invariant
  (`PROJECTION_DIRECT_TO_STRUCT.len() == materializers == consumers`, DEFERRED-LEDGER W4);
- no CSS depth closure / Unparsed escape hatch.

This is exactly TW3's scope (unlanded). The prototype (P2) corroborated the thesis cold; the plane
itself is future work.

### G3 — The kernel/cost-table plane: kernels live, cost-table absent, simd-scan crate is an empty shell

- **Kernels ARE landed** — `crates/bbnf-simd` (2,324 LOC) carries the real aarch64 NEON family
  (`structural_classify_64`, `bracket_depth_mask_64`, `comment_body_mask_64`, `unescape_uxxxx`,
  `classify_tbl4`, `match_tiny_plain_string`), each with a scalar fallback. This is the SK-V21 win,
  in-tree.
- **The `KernelCostTable` does NOT exist** — grep for `KernelCostTable`/`KernelShape`/`CostTable`
  across `crates/**/*.rs` returns nothing. TW4.1's cost-table substrate is unbuilt.
- **`crates/simd-scan/src/` is an EMPTY directory** (zero files) and is NOT a workspace member. The
  kernel home is `bbnf-simd`; `simd-scan` is a dead stub to delete.

### G4 — The grammar fleet is THREE, and only ONE source lives in-tree

Generated in `runtime/src/grammars/`: **json, sheets, css_l4_declaration_values** (3 grammars).
SK-V21 speaks of a "9-grammar" fleet and TW6 of "ten grammars" — skinny is at three. Worse, the
**grammar SOURCES reach OUT of the skinny workspace** into the legacy tree:
- `skinny/grammars/json.bbnf` — the ONLY source inside skinny.
- Sheets source: `xtask/src/main.rs:191` `SHEETS_SOURCES = ["grammar/google-sheets/google-sheets.bbnf"]`
  — legacy top-level `grammar/`.
- CSS source: `xtask/src/regen_css.rs:5-20` `CSS_L4_SOURCES` = 15 files under legacy `grammar/css/l4/`.

When the legacy tree archives, **the sheets + css_l4 grammar sources (16 `.bbnf` files) must port
IN** or regen breaks. The bbnf/bnf/ebnf/csv/toml/math self-hosting grammars (legacy `grammar/`,
`grammar/misc/`) are candidate fleet members that skinny has never regenerated.

### G5 — xtask carries the legacy per-grammar FORK (B2's disease), not the one-generator regen host

`skinny/xtask` is 5,401 LOC across a 2,780-LOC `main.rs` + `real_typed_schema.rs` (1,014) +
`skv15_w0.rs` (729) + `regen.rs` (187) + `regen_css.rs` (98). Each grammar has its own hardcoded
const roster and entry point: `JSON_SOURCES`/`SHEETS_SOURCES` in main.rs, a separate `regen_css`
TARGET table in regen_css.rs, `real_typed_schema.rs` hand-carrying a sheets schema. There is no
single metadata-driven regen host; `[workspace.metadata.bbnf.grammars]` in `skinny/Cargo.toml`
declares **only json**. This IS the fork B2 diagnosed. The charter's answer: rebuild the regen-host
pattern against the ONE generator; the legacy per-grammar fork archives with the legacy tree.

### G6 — Edition 2021, workspace-wide; PROTO-A3's 2024 recipe lives only in a dead worktree

`skinny/Cargo.toml:19` `edition = "2021"`; all 11 crates + xtask inherit `edition.workspace = true`.
No `rust-toolchain.toml` in skinny. PROTO-A3 (SPEC §3 / TW-S) proved the edition-2024 + stable-pin
recipe, but applied it in an isolated worktree, never in-tree. The graduated tree must move to 2024
and pin stable.

### G7 — Inline-test violations: 32 src files carry `#[cfg(test)]` (SPEC W0.7 counted "21+")

Live count: **32 files** with inline `#[cfg(test)]` under `crates/**/src/` — worse than the SPEC's
"21+ skinny" (SPEC §10:522). 18 of them are in `bbnf-bench/src/` (the harness), the rest across ir,
grammar, runtime, passes, codegen (lower/sink_only.rs, lower/css_scan.rs), bbnf, parse-that-regex,
test-fixtures. All must relocate to `tests/` per the no-inline-tests precept.

### G8 — Workspace/deps hygiene: TWO path-deps reach OUT of skinny into legacy `../crates/`

`skinny/Cargo.toml:36-37`: `egraph = { path = "../crates/egraph" }` and
`csp-solver = { path = "../crates/csp-solver" }` — consumed by `passes` (backend_egraph.rs,
decision_csp.rs). `xtask/Cargo.toml:42,46` also carry `path = "../crates/..."` (these are
intra-skinny and benign). The egraph/csp edges are the load-bearing ones: **when legacy archives,
these two crates must vendor IN** (see §2 P4). Also present: the `parse-that-regex` (3,022) +
`bbnf-regex` stub (322) DELETE-to-one-regex-home debt (SPEC addendum §1:17), and the smallbox[nightly]
blocker rides the legacy parse-that dep, not skinny's fork (moot at cutover).

### G9 — No wasm bindings, no TS arm, no path/cursor crate, no LSP/VM/gorgeous

Skinny has **zero** of: a wasm crate (playground prerequisite), a `CompileTarget::Ts` emitter
(grep-confirmed absent — the codegen "ts/wasm" grep hits are substring noise), a `bbnf-path`/cursor
alphabet crate, an `lsp`/VM/debug constellation, `gorgeous`. Every one exists only in legacy. Whether
each ports-in, stays-dormant-in-legacy, or dies is a §2 disposition.

---

## 2. THE LEGACY PORT-IN LIST — the ~40K hand seam, bucketed with LOC + verdict

The legacy `crates/` tree is 13 crates; `core` is 216,818 src (81.6% regenerable payload, research/38
§1). The graduation ports IN only what the small tree LACKS and cannot regenerate. Per research/38's
seam buckets:

| Legacy surface | src LOC | Skinny has it? | Port-or-rewrite verdict |
|---|---:|---|---|
| **Grammar SOURCES** `grammar/` (google-sheets, css/l4 ×15, bbnf, bnf, ebnf, csv, toml, math) | ~30 `.bbnf` | json only (G4) | **PORT-IN (move).** Relocate the `.bbnf` corpus into `skinny/grammars/`; repoint xtask consts. Blocks all regen once legacy archives. Zero rewrite — data. |
| **egraph + egraph-derive** | 1,885 + 343 | consumed via `../crates` path (G8) | **PORT-IN (vendor).** TW4.2's engine; already a skinny dep. Move into skinny workspace (or `general-infra` sub-tree per the general-infra-crates precept). No rewrite. Addendum §1:29 KEEP. |
| **csp-solver** | 7,393 | consumed via `../crates` path (G8) | **PORT-IN (vendor), then PRUNE.** Vendor to keep `passes` building; but `decision_csp` is the tautological stage marked DELETE at TW4.2 (addendum §1:18). Vendor the solver, retire the CSP decision stage. The csc411 re-vendor checkpoint (SPEC W0.5) governs. |
| **core hand-written frontend/grammar/lower/backend seam** | ~31K of the 39.9K hand LOC | skinny has its OWN grammar(2,142)+ir(1,020)+codegen(10,201)+runtime(6,522) | **REWRITE-SUPERSEDED (do NOT port).** Skinny's un-forked codegen IS the SK-V21-adjudicated replacement (structural dispatch, grammar-derived projections). Porting legacy core's fork back in would re-introduce the exact pathology the graduation exists to shed. The legacy seam dies with the tree; only its LESSONS (research corpus) carry. |
| **bbnf-path + bbnf-path-ts** (cursor/ascent alphabet) | 918 + 1,012 | NO path crate (G9); skinny has tape/cursor.rs 36 | **REWRITE-DEFERRED.** research/38 buckets `path/` 2,234 as moves-to-value-plane (TW3). Skinny's TW3 value plane subsumes the cursor alphabet natively; do not port the legacy crate — rebuild the needed cursor surface inside `runtime` at TW3. `bbnf-path-ts` deferred with WASM authority (addendum §1:33). |
| **lsp + ir/vm + @debug constellation** | 1,772 (lsp) + 1,772 (vm) | NO (G9) | **KEEP-DORMANT-IN-ARCHIVE, port-in later (TW-P).** Addendum §1:30 KEEP-DORMANT: novel grammar-debugging surface. Under graduation it cannot stay in a live legacy tree — it rides the archive branch and ports into the graduated tree only when TW-P (shopwindow) opens. Not a graduation-blocker. |
| **gorgeous** (VM-interpreted formatter) | 604 | NO (G9) | **KEEP-DORMANT-IN-ARCHIVE.** Addendum §1:31: re-binds at TW-P as the fused parse+format consumer; the 87× claim unmeasured. Archive-branch; port at TW-P. `ser` archives outright (Lock-12). |
| **analysis** | 4,975 | folded (skinny has passes) | **REWRITE-SUPERSEDED.** Addendum §1:28 folds analysis→ir + diagnostics→lsp. Skinny's `passes` + `ir` already carry the live analysis. Do not port; the analysis-consolidation item closes by the graduation itself. |
| **backend/ts arm** (`CompileTarget::Ts`) | 1,719 | NO (G9) | **KEEP-DORMANT-IN-ARCHIVE.** Addendum §1:32 / §3:2: the arm is the Emitter-seam proof, no new TS work pre-V1. Ports into the graduated multi-backend Emitter only if/when a TS target re-opens; not a blocker. |
| **bootstrap** | 465 | NO | **DELETE.** Husk (addendum §1:14 / SPEC W0.6). |
| **ser** | 530 | NO | **DELETE/ARCHIVE.** Lock-12 archive (addendum §1:31). |
| **simd-scan (legacy)** | 2,607 | skinny simd-scan EMPTY; kernels in bbnf-simd | **DELETE both.** Legacy carries x86/wasm bodies (952 LOC + 92 AVX2, W5d DELETE); skinny's stub dir deletes; the live kernels are `bbnf-simd`. |
| **ir (legacy)** | 33,779 | skinny ir 1,020 | **REWRITE-SUPERSEDED** except the VM sub-tree (above). The 33.7K is mostly generated/legacy-IR; skinny's 1,020-LOC ir is the adjudicated lowering. Do not port. |

**Port-in critical path (graduation-blocking):** grammar sources (~30 `.bbnf`), egraph+egraph-derive
(2,228 LOC), csp-solver (7,393 LOC). Everything else is either superseded (dies with the tree) or
dormant-deferred (rides the archive branch, ports at TW-P). **The hand-seam that actually moves is
~10K LOC of vendored infra + a `.bbnf` corpus — not 40K of frontend.** The 39.9K "hand seam" of
research/38 is overwhelmingly legacy `core` machinery that skinny has already re-authored.

---

## 3. THE WAVE RE-MAP — SPEC §11 manifest, wave by wave, under graduation

SK-V21 order: **TW0 → TW-S → TW1a → TW1b → TW1c → TW2 → TW4.1 → {TW3 ∥ TW4.2} → TW5 → TW6**. The
graduation's structural inversion (skinny IS the tree; legacy archives) supersedes the porting
DIRECTION baked into TW1a and the addendum §3:1 "TW2+ work happens in `crates/` after the port"
resolution. Per-wave disposition:

| Wave | Disposition | How / why |
|---|---|---|
| **TW0 W0.0** (D-G reconcile, `sk-v19-frozen` tag, supersession pointer) | **SURVIVES, RE-TARGETED.** | The DEFERRED-LEDGER §0 REDRESS 51/53/246/247 entries survive verbatim. But the tag semantics invert: `sk-v19-frozen` froze skinny as read-only oracle; graduation UN-freezes skinny as worksite. W0.0 becomes the archival-branch cut + graduation-worksite declaration. |
| **TW0 W0.1** (24-crate `core` supersession doc) | **SUPERSEDED.** | The doc named the six-seam `core` split target; that target no longer exists (core archives whole). Replaced by the great-archival disposition manifest + the new documentation home (restart/ archives; tranche docs need a durable home per charter). |
| **TW0 W0.2** (Y.13 gate template), **W0.8** (MASTER-PLAN reconcile), **W0.9** (oracle hygiene), **W0.10** (NO-GO letter, junk ignore) | **SURVIVE UNCHANGED.** | Substrate-neutral hygiene; re-home onto skinny paths verbatim. |
| **TW0 W0.3** (five-lock falsifier audit; widened Lock-14 keyed on `core` dirs) | **RE-HOMES.** | Lock-14 was to key on the phantom NEW `core` crate dirs; now keys on skinny's ACTUAL crate dirs (grammar/codegen/runtime/passes/bbnf-simd/ir). `lock14_baseline.rs:1287` already references `crates/codegen/src/general_descent/` — re-point, don't invent. |
| **TW0 W0.4** (Lock-15 + target-cpu), **W0.11** (quiet truth-floor bench) | **RE-HOMES + is the graduation baseline.** | Charter correction 5: profiling under load with PMU accounting is this tranche's empirical leg. W0.11's cold capture becomes the load-accounted skinny-at-HEAD baseline. loadavg<2 relaxed to load-robust ratio certification (charter §5). |
| **TW0 W0.5** (Lock-4 + csc411 egraph→csp) | **SURVIVES, folds into §2 port-in.** | The re-vendor checkpoint IS the mechanism for vendoring egraph+csp-solver INTO skinny (G8/§2). |
| **TW0 W0.6** (deletions: x86/wasm bodies, bootstrap, ser) | **SUPERSEDED by the great archival.** | These don't need per-surface deletion — the whole legacy tree archives. The presize DELETE family had zero `core` occurrences anyway (research/38 §1); in skinny, `simd-scan` empty-stub deletes. |
| **TW0 W0.7** (inline-test relocation) | **SURVIVES, count corrected to 32** (G7). | Re-home onto skinny; the "7 core" half is moot (core archives). |
| **TW-S** (stack abrogation → edition 2024 + stable pin) | **SURVIVES, RE-HOMES = G6.** | PROTO-A3 recipe applied to the live skinny workspace (currently 2021). The criterion-ingestion re-path (TW-S.2) and smallbox discharge survive. This is now the edition-2024 graduation step. |
| **TW1a** (the 247K `core` topology split) | **SUPERSEDED — the known supersession.** | The entire premise (split legacy core in place across six seams, land waves into it) is dissolved. Skinny is already decomposed into 11 concern-separated crates. TW1a's *value* (the six-seam decomposition) is ALREADY REALIZED in skinny's topology. Nothing to split; the wave dies. Its Lock-14 re-keying survives as W0.3. |
| **TW1b** (one-tape AoS substrate + eager-builder retirement) | **RE-HOMES, LARGELY ALREADY DONE.** | The tape hardening was ALWAYS skinny-side: `runtime/src/tape/` (mod 233 + assembler 124 + cursor 36 + offsets 6) is already the single AoS tape; json runtime asserts zero payload writes/allocs (lib.rs test). The ~1,080-hit eager-builder retirement was a legacy-`core` regen artifact (research/38: 95.1% regen-owned) — in skinny there is no `TapeStructBuilder` to delete. TW1b collapses to: confirm the single-writer `frame_depth` invariant + Lock-1 falsifier on skinny's tape. Mostly a gate, not a workload. |
| **TW1c** (StructLayout→Layout rename; N24 reclaim; presize DELETE) | **MOSTLY SUPERSEDED.** | The 960-hit rename was 88.5% legacy-`core` regen (research/38); skinny never carried the `StructLayout` name at that scale. The presize family (`Sampled`/`Exact`/`OneShotSimd`) had zero `core` hits and lives in the empty skinny simd-scan stub — deletes with it. Residual: a grep-zero hygiene gate on skinny. |
| **TW2** (the ONE generator: BackendExpr→IR, Emitter trait, xtask un-fork, one regex home, css_l4 108K trace, loc-budget) | **SURVIVES as the CENTRAL wave — RE-HOMES + absorbs G1/G5.** | This is where skinny's own gaps land: the general_descent WART-C JSON FOLD (G1), the xtask per-grammar un-fork → metadata-driven regen host (G5), the one-regex-home (delete parse-that-regex fork + bbnf-regex stub, G8). The "css_l4 108K regression" is a legacy-`core` figure — in skinny css_l4 generated is 974 LOC; the loc-budget gate re-homes but the target number shrinks 100×. "ONE emit path serves all grammars" becomes the WART-C-fold close. |
| **TW4.1** (Eisel-Lemire + KernelCostTable substrate) | **SURVIVES, RE-HOMES = G3.** | Eisel-Lemire decode already lives in `parse-that-regex` (the number decode); the `KernelCostTable` is genuinely absent (G3) and binds to skinny's `bbnf-simd` dispatch ladder, not the legacy `simd-scan::KernelShape::select` the SPEC names. Re-target the binding to `bbnf-simd/src/dispatch.rs`. |
| **TW3** (the value plane: fused sink default + lazy cursor + serde + CSS depth + projection fence) | **SURVIVES UNCHANGED in scope = G2.** | The single largest genuine forward workload. Skinny has the JSON prototype only; TW3 generalizes it. The bbnf-path cursor alphabet rebuilds here rather than porting the legacy crate (§2). |
| **TW4.2** (decision engine + kernel plane; scanner unification; decision_csp retirement) | **SURVIVES, RE-HOMES.** | One engine over skinny's `passes` (backend_egraph.rs); retire `decision_csp.rs`; scanner unification over `bbnf-simd`'s live NEON family. Kernels already unified aarch64-side, so C4/C5 shrink. |
| **TW5** (type resolution + `@host`) | **SURVIVES UNCHANGED.** | The B3 width patch (PROTO-B3) and `@host` surface land on skinny's `grammar`/`ir`. Substrate-neutral. |
| **TW6** (language + fleet + third language; TOML via the three §9 fixes) | **SURVIVES, now GATED on G1 + G4.** | TOML-as-member-ten needs the WALL A/B/WART-C fixes (G1, TW2) AND the grammar-source port-in (G4/§2). The fleet re-baselines from 3 (not 9) toward ten; the source-corpus port-in (§2) is the precondition the SPEC didn't anticipate because it assumed the legacy `grammar/` tree stayed live. |

**Net wave shape under graduation:** TW1a DIES (supersession). TW1b/TW1c COLLAPSE to gates (their
blast radius was legacy-`core` regen, absent in skinny). TW0 W0.1/W0.6 SUPERSEDED by the great
archival. TW-S becomes the edition-2024 step. **TW2 grows** (absorbs the xtask un-fork + the WART-C
fold + one-regex-home + the grammar-source port-in wiring). TW3/TW4.1/TW4.2/TW5/TW6 survive as the
real forward substrate, TW3 and G1's wall-dissolution being the load-bearing work. The gate wording
changes everywhere the SPEC said "vs the legacy `core` split" or "both workspaces build" — there is
now ONE workspace, and every no-regression clause binds to the W0.11 load-accounted skinny baseline.

---

## 4. The four graduation-blockers, ranked

1. **G1 — land PROTO-B1 in-tree** (WALL A `WsPolicy` + WALL B interpreter + WART-C JSON fold). Regen
   wiring, proven, unlanded. Gates TW2's "one emit path" and TW6's TOML. Highest leverage.
2. **§2 port-in critical path** — grammar `.bbnf` corpus + egraph/egraph-derive + csp-solver vendor.
   Without it, archiving legacy breaks skinny's regen and `passes` build. Pure move/vendor.
3. **G5 — xtask one-generator regen host.** Kills B2's per-grammar fork; metadata-driven from
   `[workspace.metadata.bbnf.grammars]` (today: json only).
4. **G6 + G7 + G8 hygiene** — edition 2024, 32 inline-test relocations, delete parse-that-regex fork
   + bbnf-regex stub + empty simd-scan stub. Mechanical, un-blocked.

Everything else (G2 value plane, G3 cost-table, G4 fleet growth) is the forward TW3/TW4/TW6 substrate
the SPEC already owns — re-homed onto skinny, not newly discovered.
