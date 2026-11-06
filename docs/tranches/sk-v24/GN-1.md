# GN-1 — THE LOOM

*The greenfield workspace: the new tree of record, woven from skinny's proven organs under
the colocation grand edict, born with its measured wins installed.*

SK-V24 greenfield novum codex · tranche of record · aarch64/Apple-M only.

---

## 1. Charter

### 1.1 Mission

GN-1 mints the greenfield workspace at the repository root — the totality tree of record, not
a prototype pass — and ports skinny's five proven organs into it under the colocation grand
edict: the generator (`grammar`/`ir`/`passes`/`codegen`), the SIMD kernel plane (`bbnf-simd`),
the tape + value fabric (`runtime`), the regen host (`xtask`), and the cold canon benches
(`bbnf-bench`). The port is a **relocation with restructuring**: every organ moves into a
directory-module tree that owns its kernels, templates, and constants — no flat sibling sprawl,
no god modules — and the six born-verified prototype patches land onto the ported organs during
the weave, so the greenfield is born with its performance-proven features installed rather than
bolted on afterward. Skinny retires as a tree the instant this port completes (SPEC §1; GN-0
retains `skinny/` tagged until GN-1 closes, then archives it with the rest).

The productionization law binds here first: skinny's organs arrive proof-grade, and GN-1 lands
them production-grade — public API surface intact, `@generated` provenance preserved, the
checkasm parity oracle re-homed per kernel, the canon-bench instrument promoted from a
worktree-only file to a tracked, committed graduation instrument. Prototype residue that cannot
be productionized this tranche is **named** with its consuming wave, never left dormant and
silent (§1.4, §7).

### 1.2 Precept bindings

- **P8, P40** (substrate lands with its consumer in the same wave): every wave below names the
  consumer that reads its output *in that wave* (§4, the Y.13 line per wave). The one kernel
  whose end-to-end consumer is a later tranche — PROVE-01's `SimdMode::Prefilter` — lands with
  its kernel-grade consumer (parity oracle + `prefilter_bench`) and a **named** downstream wave
  (GN-2.W1); this is a declared seam, not a hidden stub (P7, P10).
- **P14, P15** (no god modules; splits use directory modules, cross-crate isomorphism): the
  tree cut (W1) replaces skinny's backend-split `bbnf-simd` (`aarch64/*.rs` ∥ `scalar/*.rs`
  flat siblings) with kernel-owning directory-modules (`kernels/<k>/{mod,aarch64,scalar}.rs`),
  and decomposes the 10.2 KLOC `codegen` crate into `emit/`, `templates/`, `schema/`, `lower/`.
- **P16, P54** (tests in `tests/`, never `#[cfg(test)]` in `src/`): every relocated and
  new-authored test lands under the crate's `tests/`.
- **P53** (generated files are fresh-regen output, byte-identical on unchanged input): the W1
  relocation is proven by regen producing **byte-identical** output post-move; PROVE-12/13's
  generated deltas reproduce bit-for-bit from their mutated templates.
- **P43–P49** (the profiling standing law): every number in this tranche is cold-per-parse,
  PMU cyc/B certified, paired-interleaved, N≥200, multi-window, load-accounted. Warm figures
  are forbidden as certified evidence (§1.3).
- **P5, P6, P50** (one path, no legacy, no stubs): the port carries no skinny code that fails
  to re-earn its place; the dead-probe accretion in `bbnf-bench` (35.9 KLOC, most of it prior-
  tranche graveyard) archives with skinny rather than porting (W2, §6 ledger).
- **P19** (a hard gate is valid only with an artefact): every gate below is a build, a regen
  byte-diff, a checkasm parity run, or a cold canon-bench table — never a document.

### 1.3 Refutation guards (SPEC §0 laws that constrain GN-1)

These are the laws GN-1 must not violate; each names the measured refutation that forbids it.

1. **No digit-scan SIMD.** PROVE-02 (Mula/Lemire NEON digit parse) is **NO_GO** — latency-bound
   CPI inversion regresses the product lane. GN-1 lands no digit-scan kernel; the number path
   stays the SWAR-blocked `scan_digit_run` (parse-that-regex `number/mod.rs:106`). The canada
   float tax is owned by GN-2.W5 (the `materialize_f64` / Eisel–Lemire lane), never the digit
   scan.
2. **No block-scheduling pursuit.** PROVE-04 (multi-block software pipelining) is **refuted as a
   lever** (1.05–1.06× only; the M-class OoO core already pipelines the single-block loop). GN-1
   lands no multi-block scheduling. The classify-floor lever — movemask-narrow + vectorized
   compaction — is GN-2.W4, not this tranche.
3. **No shim perf claims.** PROVE-09 (`portable_simd`→NEON shim) is **refuted** — the 1.34–1.57×
   did not reproduce (parity-to-slower). GN-1 does not land PROVE-09; the de-nightly port rides
   parse-that's PT-N3 as **stability value, zero perf claim**. The vendored `parse-that-regex`
   arrives as-is (W3), not shim-ported.
4. **Fold-over-cursor is dead** (2.3–4.9× behind, carried law). No lazy-cursor fold appears in
   any ported organ or new-authored code.
5. **Tape-navigation is NOT the get lane** (measured at 150–1700× the stop-early cost). PROVE-06
   lands as **stop-early tape-free get** (`runtime/.../json/path.rs`), never tape navigation; the
   value fabric's get lane resolves at 0.032% of the document.
6. **Warm benches are forbidden as certified evidence.** The ported canon bins (`json_canon_bench`,
   `css_canon_bench`) are cold-per-parse, system-allocator (family A). The criterion benches
   (family B, mimalloc) are **parity oracles only** — used to prove a new arm's correctness before
   timing it, never to quote a figure. The CSS paired-mode extension (W2) is cold.

### 1.4 Supersessions

- Skinny's **backend-split** `bbnf-simd` layout is superseded by kernel-owning directory-modules
  (W1). The PROVE-01/03 patches, authored against `aarch64/prefilter.rs`/`scalar/prefilter.rs`
  and `aarch64/string_block.rs`, are **adapted** to the new kernel-dir paths on landing (W4,
  §4.4 adaptation notes).
- The **worktree-only** `json_canon_bench.rs` (present only at
  `.claude/worktrees/wf_e940452a-431-7/...`, never committed — R2 §0) is superseded by a
  **tracked, committed** canon instrument (W2).
- Skinny's ad-hoc **vendored** `parse-that-regex` copy is superseded by a **gate-pinned synced
  vendor** modeled on the outer-repo csp-solver canonical-diff policy (W3).
- SK-V22's "skinny graduates as tree of record" premise is superseded (HANDOFF §1 directive 2):
  skinny's organs port, its accretion archives.

---

## 2. Interlocks

### 2.1 Depends-on

| Upstream | Gate that must be green before GN-1 opens |
|---|---|
| **GN-0** (Great Archival + x86 purge) | Rescue checks green; repo top-level is the new shape; `crates/simd-scan/{avx2,avx512,wasm}.rs` purged; `skinny/` tagged-and-retained (archives on GN-1 close). Owner gate: §3 checkbox 3 — archival executes immediately post-freeze, before GN-1. |

The GN-0 dependency is strict: GN-1 relocates skinny's organs *into* the top-level `crates/`
tree that GN-0 clears (the legacy pre-restart `crates/{analysis,core,...}` moves to
`archive/legacy-crates` as a branch). GN-1 cannot cut the floor plan until that space is vacant.

### 2.2 Feeds

| Downstream consumer | What GN-1 hands it (named wave.artifact) |
|---|---|
| **GN-2.W1** (prefilter end-to-end) | GN-1.W4 lands PROVE-01's kernel + `SimdMode::Prefilter` IR variant (`ir/src/.../recognizer`); GN-2.W1 wires `passes::derive_recognizers` → `find_member` lowering. |
| **GN-2.W3** (CSS animate redress) | GN-1.W4 lands PROVE-05's CSS shufti kernel **flag-OFF**; GN-2.W3 hoists the per-call LUT + short-span guard, then flips the flag ON. |
| **GN-3.W1–W3** (type plane) | GN-1.W1 relocates `codegen/schema/` (`direct_schema`, `json_typed_direct`) and preserves the `ExprKind::Project` seam; GN-3 threads it into `infer`/`DirectBuild`. The hand file `real_typed_schema.rs` ports as a **named crutch** (§7) that GN-3.W2 deletes. |
| **GN-4.W1–W4** (value fabric) | GN-1.W4 lands PROVE-06 `path.rs` (stop-early get) and PROVE-07 direct-to-struct sink; GN-4 productionizes them as the public path/struct APIs and the TS-backend seed. GN-4.W2 owns PROVE-06's deep-path 2.0× loss (SIMD key scanner). |
| **GN-5.W1–W2** (generality) | GN-1.W4 lands PROVE-13's CSV grammar (`grammars/csv/`) + `xtask regen-csv`; GN-5.W1 re-measures under fat-LTO release; GN-5.W2 authors TOML atop the same regen host. |
| **GN-6** (shopwindow + close) | GN-1.W2's canon-bench tables become the PERFORMANCE-THESIS evidence surface. |

### 2.3 parse-that interlock (letter BBNF-PT-G, placement-law-bound)

The one-regex-home law (SPEC §4) makes `parse-that/rust/regex` the canonical regex engine.
skinny vendored a `parse-that-regex` copy (3022 LOC — R1 §1). GN-1.W3 vendors it under a
sync gate pinned to a parse-that rev **preceding PT-1.W1** (the PROVE-10 prefilter landing). The
pin is deliberately pre-PT-1 at port time; once **PT-1.W1 lands** the prefilter tier on the
parse-that side (under letter `BBNF-PT-G-2026-07-16`), a **named GN-1.W3 follow-up re-vendor**
bumps the pin to the PT-1.W1 rev so the greenfield inherits `Dfa::find` — the same rule PT-1 §2
states (land the prefilter before the vendor step, or re-vendor post-PT-1.W1). Owner gate: letter placement is owner/parse-that-side only (SPEC §2b,
§3 checkbox 1); GN-1 places nothing in parse-that and commits nothing there (non-clobber law).

---

## 3. Wave manifest

| id | name | scope (one line) | LOC (+new/−del) | gate type | hard cap |
|---|---|---|---|---|---|
| **GN-1.W1** | THE FLOOR PLAN | Mint the greenfield crate set + directory-module tree; relocate the 5 organs; regen byte-identical. | +180 / −0 (moves ~13.9 KLOC hand + ~5.2 KLOC gen) | born-verified (structural-lock) | 6 h |
| **GN-1.W2** | THE INSTRUMENT | Restore `json_canon_bench` tracked; extend CSS canon to paired mode (R2 gap); re-anchor the §0 baseline on the ported-unpatched tree. | +420 / −~2,500 (dead-probe reclaim) | born-verified (baseline reproduction) | 5 h |
| **GN-1.W3** | THE REGEX SEAM | Vendor `parse-that-regex` under a csp-solver-pattern sync gate; adjudicate one-regex-home; re-vendor to PT-1.W1 on landing. | +300 / −0 | born-verified (byte-diff + compile) | 3 h |
| **GN-1.W4** | THE PATCH SEQUENCE | Land PROVE-01/03/06/07/12/13 born-verified (adapted to kernel-split paths) + PROVE-05 flag-OFF. | +~3,300 / −~200 | born-verified (per-patch delta) | 8 h |
| **GN-1.W5** | THE ASSAY *(challenge)* | Adversarial re-measure of §0 on the full ported+patched tree; prototype-residue hunt. | +120 / −0 | **born-RED** (regression or hidden residue fails) | 5 h |

Total directional LOC: **+~4,320 new/restored** / **−~2,700 reclaimed**, atop a
~19-KLOC relocation payload that moves without rewrite. See §6 for the full ledger.

---

## 4. Per-wave specification

### 4.0 The target tree (normative for W1; the colocation grand edict made concrete)

The greenfield workspace root is the repository root; the workspace manifest is
`/Users/mkbabb/Programming/bbnf-lang/Cargo.toml` (a fresh member list, the legacy one archived
by GN-0). Crate boundaries and their reason for existing:

```
crates/
  bbnf/                  # thin public facade — re-exports the union surface (skinny bbnf, 111 LOC)
  grammar/               # .bbnf front-end → GrammarIr (the ONLY parser of grammar source)
    src/{lib.rs, lexer/, parser/, ast/}          tests/
  ir/                    # GrammarIr, BackendIr, the cost model — the shared vocabulary
    src/
      lib.rs
      cost/              # CostFacts, SubstrateTarget, DecisionCspFacts, PriorityStep (skinny ir/cost.rs)
      recognizer/        # Recognizer, SimdMode, SimdSite  (skinny ir/lib.rs:416-430)
      shape/             # BackendShape (EagerTape|OffsetTape|EventTape|SinkOnly|CollapsedStage)
    tests/
  passes/                # infer → shape → CSP → egraph — the decision driver
    src/{lib.rs, infer/, shape/, recognizers/, cost/}          tests/
  codegen/               # THE generator — decomposed from skinny's 10.2 KLOC god-crate
    src/
      lib.rs             # emit_from_source / emit_typed_from_source entries (skinny lib.rs:101,130)
      lower/             # IR→Rust lowering (eager_tape, offset_tape, event_tape, sink_only,
                         #   collapsed_stage, css_scan, schema_direct, tape_plan)
      emit/              # shape emitters, each its own dir-module
        json_sink_direct/    # the JSON un-fork emitter (dispatch, first_set, recognize, skeleton)
        css_scan_direct/     # drivers, projection
        general_descent/     # emit, view, regex_first  (PROVE-13 CSV routes here)
      templates/         # colocated raw-string templates + runtime_generator (the emit spine)
        json/  css/  csv/    # CSS_GENERATED_RS (PROVE-12/05 target) lives under css/
        runtime_generator.rs
      schema/            # direct_schema, json_typed_direct (1245 LOC — flagged colocation debt, §7)
    tests/
  runtime/               # the tape + value fabric + generated-grammar host
    src/
      lib.rs             # wires generated_json / _css_l4_declaration_values / _sheets (/ _csv W4)
      tape/              # Tape, PayloadArena, ValueRef, assembler, cursor, offsets (skinny runtime/tape/)
      value/             # DocumentView, typed cursors, path/ (PROVE-06 path.rs lands here)
      scan/              # runtime_scan (CSS balanced-component shell), runtime_simd bridge
      grammars/          # @generated OUTPUT root: json/ css_l4_declaration_values/ sheets/ (csv/ W4)
    tests/
  bbnf-simd/             # per-kernel directory-modules; each kernel OWNS its scalar parity oracle
    src/
      lib.rs             # prim:: facade + scan_dispatch (skinny lib.rs:105,250)
      dispatch.rs        # PrimitiveKernels fn-ptr table, SelectedBackend enum, select_backend
      kernels/
        structural_classify/  { mod.rs, aarch64.rs, scalar.rs }   # vceqq fan (was aarch64/+scalar/ split)
        classify_tbl4/        { mod.rs, aarch64.rs }              # vqtbl4q lo6 classifier
        bracket_depth/        { mod.rs, aarch64.rs, scalar.rs }
        comment_body/         { mod.rs, aarch64.rs, scalar.rs }
        string_block/         { mod.rs, aarch64.rs }              # PROVE-03 extends this dir
        match_tiny_string/    { mod.rs, aarch64.rs }
        unescape_uxxxx/       { mod.rs, aarch64.rs }
        movemask/  utf8/  digit_mac/  quad_load/                  # the shared toolkit dirs
        prefilter/            { mod.rs, aarch64.rs, scalar.rs }   # PROVE-01 lands NET-NEW here
    tests/               # checkasm parity per kernel (prefilter_parity, checkasm_string_terminal_block, ...)
  bbnf-regex/            # thin facade — re-exports the vendored engine's public API (skinny 322 LOC)
  parse-that-regex/      # VENDORED, gate-pinned (W3); number/, eisel_lemire/, string kernels
  bbnf-bench/            # cold canon bins (family A) + criterion parity (family B) + probes
    src/{bin/, direct_struct.rs, real_typed_struct.rs, css_l4_corpus.rs, parity.rs, gate.rs, scan.rs}
    benches/{json_parity.rs, simd_scan.rs}
  test-fixtures/         # corpus loader + corpus/{json,...}
  xtask/                 # ONE regen host + gate + checkasm CLI
    src/{main.rs, regen/{json,css,sheets,csv,real_typed}.rs, gate/}
```

**Crate-boundary rationale** (no god crates, no flat sprawl): `grammar` is the sole reader of
`.bbnf` source; `ir` is the shared vocabulary both `passes` and `codegen` depend on (a diamond
apex, not a dumping ground); `codegen`'s four concerns — lowering, shape-emission, template
residualization, schema derivation — become four directory-modules rather than 10.2 KLOC of
flat siblings; `runtime` splits cleanly into the offset-tape *fabric* (`tape/`), the *value*
surface (`value/`), and the generated *host* (`grammars/`); `bbnf-simd` inverts skinny's
backend-first split (which scattered each kernel's NEON body and scalar oracle across two flat
dirs) into kernel-first ownership (each kernel dir holds its own `aarch64.rs` + `scalar.rs`
parity anchor, cross-crate-isomorphic with the checkasm test names). `bbnf-regex` stays a thin
facade over `parse-that-regex` per the one-regex-home law — no combinator surface grows in
bbnf.

---

### 4.1 GN-1.W1 — THE FLOOR PLAN

**Objective.** Cut the greenfield tree (§4.0) and relocate the five organs into it, restructured
under the colocation edict, proving by byte-identical regen that the relocation moved no behavior
and no number.

**File-level plan.**
- Author the workspace manifest `crates/../Cargo.toml` (member list, resolver 2, edition 2021,
  the profiles carried verbatim from skinny `Cargo.toml:72-101`: `release` = opt3/fat-LTO/cgu1/
  panic=abort/debug=true; `bench` inherits release; `ax-iter` = thin-LTO/cgu16 for iteration).
  Cross-repo path-deps `egraph`, `csp-solver` resolve to the outer-repo `crates/` (unchanged).
- Relocate by bulk move, then rewrite module paths:
  - `grammar`, `ir`, `passes` → `crates/{grammar,ir,passes}` with the `cost/`, `recognizer/`,
    `shape/`, `infer/` directory-module cut (skinny had these as flat `cost.rs`, `lib.rs` inlined).
  - `codegen` → `crates/codegen` with the `emit/`, `templates/`, `schema/`, `lower/`
    decomposition. `runtime_generator.rs` and the `CSS_GENERATED_RS`/`JSON`/CSV raw-string
    templates move under `templates/`; `json_sink_direct/`, `css_scan_direct/`, `general_descent/`
    move under `emit/`; `direct_schema.rs`, `json_typed_direct.rs` under `schema/`.
  - `runtime` → `crates/runtime` with the `tape/` ∥ `value/` ∥ `scan/` ∥ `grammars/` split
    (skinny had `tape/`, `runtime_scan.rs`, `runtime_simd.rs` as flat siblings; the value surface
    was intermixed into `tape/mod.rs`).
  - `bbnf-simd` → `crates/bbnf-simd` with the **kernel-first** cut: each `aarch64/<k>.rs` +
    `scalar/<k>.rs` pair becomes `kernels/<k>/{aarch64.rs, scalar.rs}` + a `mod.rs` that wires the
    `#[cfg(aarch64)] neon else scalar` dispatch shape (skinny `lib.rs:301-309` pattern, now
    per-kernel-local). The `prim::` facade in `lib.rs:250` re-exports from the kernel dirs.
  - `bbnf-bench`, `test-fixtures`, `xtask` → their crates; the xtask regen dispatch
    (`main.rs:39-52`) is rehomed into `xtask/src/regen/{json,css,sheets,real_typed}.rs`
    submodules (one file per grammar target, not one flat `main.rs` switch).

**Seams (path:line citations).** IR recognizer extension point `ir/src/lib.rs:416-430`
(`Recognizer::SimdScan`, `SimdMode`, `SimdSite`); dispatch table `bbnf-simd/dispatch.rs:11-16,
50-56,63,89,101`; codegen entries `codegen/src/lib.rs:101,114,130,194`; tape struct
`runtime/src/tape/mod.rs:92,102` (`from_offsets`), lazy-projection invariant
`runtime/src/lib.rs:42-44`; xtask regen mechanics `xtask/src/regen.rs:19,36`, source paths
resolved against the outer bbnf-lang `grammar/` tree (R1 §10 path trap — preserved).

**Gate.** Structural-lock class (a pure move must not move a number):
```
cargo build --workspace                                  # green, both aarch64 profiles
cargo xtask regen-json && cargo xtask check-json         # byte-identical
cargo xtask regen-css  && cargo xtask check-css-l4-declaration-values
cargo xtask regen-sheets && cargo xtask check-sheets
cargo xtask primitive-checkasm                           # NEON==scalar, every kernel dir
```
**Threshold / failure semantics.** `check-*` must exit 0 with **zero** diff bytes against the
pre-move golden (regen against unchanged `.bbnf` produces byte-identical `generated.rs`). Any
non-empty diff, or any `primitive-checkasm` parity mismatch after the kernel-first restructure,
**fails the wave** — a relocation that changes generated output or breaks NEON/scalar parity is a
behavior change masquerading as a move. This gate *can* fail: a botched module-path rewrite that
drops a template hunk, or a kernel dir that mis-wires its `#[cfg]` dispatch, produces a diff or a
parity break.

**Consumer in this wave (Y.13).** The regen host and `primitive-checkasm` are the consumers: the
emitted parsers consume `codegen` (byte-identical proves the relocation preserved the emit spine);
the checkasm harness consumes every relocated kernel (parity proves the kernel-first cut preserved
each NEON body against its scalar oracle).

**Oracles / benches.** No timing in W1 (a move must not be timed for a win); the differential
oracle is regen byte-equality + checkasm parity.

---

### 4.2 GN-1.W2 — THE INSTRUMENT

**Objective.** Restore the JSON cold-canon instrument as tracked source, extend the CSS canon
bench with the built-in paired mode it lacks (R2's named gap), and re-anchor the SPEC §0 /
sk-v22-research-04 graduation baseline on the ported-but-unpatched tree — the non-regression
anchor every later wave binds to.

**File-level plan.**
- **Restore `json_canon_bench.rs`** as tracked source at
  `crates/bbnf-bench/src/bin/json_canon_bench.rs`. Source of record: the worktree copy
  `.claude/worktrees/wf_e940452a-431-7/skinny/crates/bbnf-bench/src/bin/json_canon_bench.rs`
  (298 lines, R2 §0). It compiles against the ported runtime unchanged: it calls
  `runtime::generated_json::{parse, parse_only}`, `bbnf_bench::direct_struct::{track1_digest,
  sonic_digest, serde_digest}`, `real_typed_struct::locate_fixture` — all preserved by W1.
  `src/bin/*.rs` autodiscovers; no `[[bin]]` stanza needed. **Commit it** — this is the
  supersession of the never-committed worktree file (P53: the instrument is now regen-clean and
  tracked, not a floating artifact).
- **Extend the CSS canon bench to paired mode.** `css_canon_bench.rs` has cold + PMU but **no
  paired mode** (R2 §3: "CSS has NO paired mode"). Mirror the JSON paired block
  (`json_canon_bench.rs:226-262` per-sample four-lane interleave + `emit_paired` :289-298: per-
  sample ratio A_ns/B_ns, mean, 95% CI normal-approx `mean ± 1.96·sqrt(var/n)`) into
  `css_canon_bench.rs`, interleaving `track1_full`, `track1_rich`, `lightningcss`, `cssparser`
  back-to-back inside one sample, emitting `track1/lcss` and `rich/lcss` ratios with CI. This
  closes the R2 gap and lets W4/W5 quote CSS wins as load-certified ratios, not baseline-vs-
  modified-binary hand-pairing.
- **Reclaim the dead-probe accretion.** `bbnf-bench` is 35.9 KLOC (R1 §1), the overwhelming
  majority prior-tranche probe graveyard (`lock14_baseline`, `json_w9`, `w1_tape_typed_bench`,
  `w2_rich_cssom_bench`, `xctrace_probe`, and kin). Port only the **canon-bench dependency
  closure**: the two canon bins, `direct_struct`, `real_typed_struct`, `css_l4_corpus`, `parity`,
  `gate`, `scan`, `sonic_skipper`, `materialization`, and the family-B `benches/{json_parity,
  simd_scan}.rs`. Everything outside that closure archives with skinny. W5 audits that nothing
  live was dropped.

**Seams.** PMU capture `css_canon_bench.rs:44-102,224-260` and `json_canon_bench.rs:24-83,190-221`
(byte-identical `RusageInfoV5` FFI); cold sample discipline `json_canon_bench.rs:140-175`,
`css_canon_bench.rs:159-190`; paired block to mirror `json_canon_bench.rs:226-262,289-298`;
fixture resolution `real_typed_struct.rs:1372,1394-1408`; CSS corpus `css_l4_corpus.rs:22-58`.

**Gate.** Born-verified baseline reproduction:
```
cargo build --release -p bbnf-bench --bin json_canon_bench --bin css_canon_bench
./target/release/json_canon_bench paired 200          # ≥3 windows
CSS_CANON_PMU=1 ./target/release/css_canon_bench 800
./target/release/css_canon_bench paired 200           # the NEW mode
```
**Threshold / failure semantics.** On the ported-unpatched tree, the JSON paired ratios must
reproduce the graduation baseline (R2 §7) within noise: twitter `tape/sonic` **0.515 ± window
CI (target ≤0.55)**, canada `tape/sonic` **0.726 (≤0.78)**, citm `tape/sonic` **0.529 (≤0.56)**,
with every window's 95% CI excluding 1.0 in the same direction (P44). CSS PMU cyc/B must
reproduce R2 §7 within noise: bootstrap track1 **≤16 (baseline 15.02)**, tailwind **≤14 (13.29)**,
material **≤14.5 (13.48)**, animate **≤15.5 (14.56)**. Any lane regressing beyond noise on the
*unpatched* port means W1's relocation silently moved a number — **fails**, and re-opens W1. The
new CSS paired mode must emit finite ratios with CI on all four corpora (a mode that emits no
ratio is a defect). This gate can fail: a relocation that perturbed an inline hint, a mis-set
profile, or a fixture-path regression all surface as a baseline miss here.

**Consumer in this wave (Y.13).** The canon bins are the consumers of the ported runtime parse
paths; the reproduced baseline table is the artifact and the standing anchor for W4/W5.

**Oracles.** Family-B `json_parity.rs` (`assert_direct_struct_parity`) and the CSS parity checksum
(`CSS_CANON_PROFILE` XOR-fold) assert byte-exact output before any timing is trusted (P51).

---

### 4.3 GN-1.W3 — THE REGEX SEAM

**Objective.** Vendor `parse-that-regex` into the greenfield under a sync gate that keeps the
copy faithful to a pinned parse-that revision and proves the pinned code still compiles every
bbnf consumer — modeled precisely on the outer-repo csp-solver canonical-diff policy — and
adjudicate the one-regex-home consumption decision as a named GN-1.W3 re-vendor once PT-1.W1 lands.

**File-level plan.**
- Relocate `parse-that-regex` (3022 LOC — `number/`, `eisel_lemire/`, string-span kernels;
  R1 §7) to `crates/parse-that-regex` as the vendored copy; `bbnf-regex` stays the thin facade
  (`crates/bbnf-regex`, re-export only — one-regex-home, no combinator surface in bbnf).
- Author `scripts/sync-parse-that-regex-vendor.sh`, isomorphic to
  `scripts/sync-csp-solver-vendor.sh` (270 LOC, the proven pattern). Three modes:
  - `--check` (default): byte-diff `crates/parse-that-regex/src` (+ any vendored `tests/`)
    against the pinned parse-that rev, extracted with `git archive <rev>` from the parse-that
    sibling (never the moving working tree). The pin lives in exactly one place — a 40-hex sha
    in `crates/parse-that-regex/Cargo.toml`'s `description` (`read_pin` idiom). Fails loud on any
    drift (P53's provenance gate, cross-crate form).
  - `--update [<rev>]`: re-vendor `src/` (+ `tests/`) from parse-that `<rev>` (default HEAD),
    rewrite the pin, point at `--verify`. Reproducible from `git archive`.
  - `--verify`: compile the bbnf consumers of the regex engine — `{bbnf-regex, bbnf-simd,
    codegen, runtime}` (the crates whose emitted string/number paths call the vendored kernels)
    — plus a structural tripwire: the vendored public API surface (`match_string_at_quote_*`,
    `scan_digit_run`, `NumberSpan`, `compute_f64`) diffed against a recorded
    `scripts/.parse-that-regex-api.baseline`, warning (not failing) on a field/signature delta so
    the human sweeps consumers and refreshes the baseline (the SolveConfig-field-set idiom).
  - Wire `--check` + `--verify` into `scripts/hooks/pre-push` (mirroring the csp-solver hook).
    Never pushes (parse-that origin untouched — non-clobber).

**One-regex-home adjudication.** The law names `parse-that/rust/regex` canonical. Two states:
*now* — parse-that has not yet landed PT-1.W1 (PROVE-10's prefilter tier, `Dfa::find`, the 245-line
additive patch, CONFIRMED 8.8–29.5×); the greenfield consumes the **vendored** copy, gate-pinned
to a rev **preceding** PT-1.W1, so the vendor is a faithful mirror of a stable upstream point.
*after PT-1.W1* — once the owner/parse-that-side session places letter `BBNF-PT-G-2026-07-16` and
PT-1.W1 lands, the **named GN-1.W3 re-vendor** fires: either bump the pin to the PT-1.W1 rev (inheriting
the prefilter tier by re-vendor + `--verify`) or, if parse-that publishes a consumable crate
identity, drop the vendored copy for a path/registry dep. GN-1 does **not** force this — it
installs the gate that makes either move a one-command re-vendor. The generator inherits the
prefilter tier by emitting `dfa.find` regardless of vendoring mode (SPEC §2b, PT-1.W5).

**Gate.**
```
scripts/sync-parse-that-regex-vendor.sh --check       # byte-identical to pin
scripts/sync-parse-that-regex-vendor.sh --verify      # consumers compile + API tripwire
```
**Threshold / failure semantics.** `--check` must report **zero** drift bytes against the pinned
rev; `--verify` must compile all four consumer crates under both profiles and the API tripwire
must find no unacknowledged surface delta. A drifted vendor (someone hand-edited the copy) fails
`--check`; a pin bump that breaks a consumer fails `--verify`. This gate can fail on exactly the
class it guards: silent divergence of the vendored copy from its upstream truth.

**Consumer in this wave (Y.13).** `--verify` compiles the real consumers (`codegen`-emitted
string/number paths, `bbnf-simd` unescape kernel, `runtime`); the vendored kernels are consumed,
not dormant.

---

### 4.4 GN-1.W4 — THE PATCH SEQUENCE

**Objective.** Land the six born-verified prototype patches onto the ported organs, each adapted
to the greenfield tree and gated on reproducing its own VERIFY-certified delta, plus PROVE-05
flag-OFF as a declared, named seam.

**Landing order and per-patch spec.** (Ordered to respect file dependencies: generated-template
patches before the benches that measure them; the CSV crate last as it is additive-new.)

**PROVE-12 — Emit-spine residualization (CONFIRMED).** Patch touches
`codegen/src/runtime_generator.rs` (now `codegen/src/templates/runtime_generator.rs`) +
`runtime/.../css_l4_declaration_values/generated.rs`. *Adaptation:* the `CSS_GENERATED_RS`
template moved under `templates/css/`; re-home the two hunks (the `find_component_delim<const
DELIMS: u128>` const-generic + the `delim_mask` const-fold helper) there; regen reproduces
`generated.rs` bit-for-bit. *Gate:* `cargo xtask regen-css && cargo xtask
check-css-l4-declaration-values` byte-identical (+18 generated LOC, 910→928); CSS paired mode
(W2) shows track1 cyc/B ratio **≤0.98× vs the W2 baseline** (VERIFY: 0.81–0.98×, ins/B −3.7…−6.4%
deterministic); parity checksum MATCH (track1_full=1, track1_rich=17839).

**PROVE-03 — Fused NEON string scan (CONFIRMED).** Patch touches
`bbnf-simd/src/aarch64/string_block.rs`, `parse-that-regex/src/lib.rs`,
`parse-that-regex/Cargo.toml`, + two test files. *Adaptation:* the string-block kernel moves to
`bbnf-simd/src/kernels/string_block/aarch64.rs`; the checkasm test to
`bbnf-simd/tests/checkasm_string_terminal_block.rs`. The `parse-that-regex` edit lands **through
the vendor gate** (W3): it must be an *upstream* change (bump the pin to a parse-that rev
carrying it) or explicitly ledgered as a vendored-local delta the API tripwire acknowledges —
**not** a silent hand-edit of the vendored copy (P53, non-clobber). *Gate:*
`checkasm_string_terminal_block` parity PASS; `json_canon_bench paired 200` shows twitter
`parse_tape` cyc/B **≤0.93× vs W2 baseline** (VERIFY: 0.898–0.931×, won both interleave slots,
ins/B −9…−12%).

**PROVE-01 — NEON shufti/truffle prefilter (CONFIRMED, kernel-grade).** Patch touches
`bbnf-simd/src/{aarch64/prefilter.rs, scalar/prefilter.rs, aarch64/mod.rs, scalar/mod.rs,
dispatch.rs, lib.rs}`, `bbnf-simd/tests/prefilter_parity.rs`, `ir/src/lib.rs`,
`bbnf-bench/src/bin/prefilter_bench.rs`. *Adaptation:* the NEON body + scalar oracle collapse
into `bbnf-simd/src/kernels/prefilter/{aarch64.rs, scalar.rs}` (kernel-first, the parity anchor
adjacent); `prim::find_member` exposed from `lib.rs:250`; the `SimdMode::Prefilter` variant added
at `ir/src/recognizer/` (was `ir/src/lib.rs:425`). *Declared seam:* `SimdMode::Prefilter` is a
**consumer-less enum variant at GN-1** — the live scan still emits `Exact`; its end-to-end
consumer is **GN-2.W1** (`passes::derive_recognizers` → `find_member` lowering). GN-1's consumer
is kernel-grade: the parity oracle + `prefilter_bench`. *Gate:* `cargo test -p bbnf-simd --test
prefilter_parity --release` 3/3 PASS (NEON==scalar, sets 1..=16, align 0..64, corpus byte-exact);
`prefilter_bench` reproduces dense-neutrality (twitter/bootstrap **≥1.0×**, VERIFY: 1.03–1.09×)
and skip-heavy **≥4.5×** (VERIFY: shufti ~7.5×, truffle ~4.7×).

**PROVE-06 — Stop-early path get (CONFIRMED).** Patch touches
`codegen/src/json_templates/view.rs`, `codegen/src/runtime_generator.rs`,
`runtime/.../json/{mod.rs, path.rs, scan.rs, view.rs}`, `bbnf-bench/src/bin/json_path_bench.rs`.
*Adaptation:* `path.rs` lands under `runtime/src/value/path/` (the value fabric, refutation guard
5 — stop-early, NOT tape navigation); the template edits move to `codegen/src/templates/json/`
and `codegen/src/emit/.../view`; regen reproduces `path.rs` bit-for-bit. *Named downstream:* the
deep-path 2.0× loss (SIMD key scanner owed) is **GN-4.W2**. *Gate:* `cargo xtask regen-json &&
check-json` byte-identical; `json_path_bench` byte-exact parity vs sonic `get_from_str` on every
path; shallow **≤0.85×**, missing **≤0.72×** vs sonic get (VERIFY: shallow 0.78–0.79, missing
0.68); deep loss disclosed, owned by GN-4.W2.

**PROVE-07 — Direct-to-struct fused sink (CONFIRMED).** Patch touches
`bbnf-bench/src/bin/json_canon_bench.rs` (the restored instrument). *Adaptation:* the new
`fused`/typed lane merges into the W2-restored canon bin (add the arm at the lanes block
`:87-107`, register in `WORKLOADS`, add its paired vector at `:257-260`). *Gate:*
`json_canon_bench paired 200` shows direct-to-struct **≤0.79× vs sonic typed** (VERIFY:
0.746–0.786) and beats serde on all three corpora; the 2.0× serde bar on twitter is **honestly
unmet** (thin projection — named §7, owned by GN-3.W3 grammar-driven schema). family-B parity
oracle green before timing.

**PROVE-13 — CSV through the one generator (CONFIRMED generality; perf UNVERIFIABLE→GN-5.W1).**
Patch touches `grammar/misc/csv.bbnf`, `codegen/src/{general_descent/emit.rs,
general_descent/regex_first.rs, lower/sink_only.rs, runtime_generator.rs}`,
`runtime/src/grammars/csv/{config,generated,mod,parser,view}.rs`, `runtime/src/lib.rs`,
`xtask/src/main.rs`, + the `csv-prove` crate. *Adaptation:* codegen edits map to `emit/
general_descent/`, `lower/`, `templates/`; the `csv/` generated dir lands under
`runtime/src/grammars/csv/`; the xtask target rehomes to `xtask/src/regen/csv.rs`
(`regen-csv`/`check-csv`); `csv-prove` becomes `crates/csv-prove`. The **name-gate cure** is
load-bearing: `emit_sink_only` routes on the structural predicate `dispatch_alt_count > 0 &&
direct_shapes.len() >= 2` (the old `!has_rule("value")` is gone — VERIFY attack 2 PASS). *Gate:*
`cargo xtask regen-csv && cargo xtask check-csv` byte-identical; `csv-prove` correctness gate
`PASS (quoted/escaped-quote/CRLF/empty-field)`; field-count parity delta = quoted-field count
(disclosed, not a defect). Perf (0.948× vs csv-core) is **directional** — the fat-LTO release
re-measure is **GN-5.W1, born-RED** (not gated here).

**PROVE-05 — CSS shufti token kernel (CONFIRMED_DELTA, LANDS FLAG-OFF).** Patch touches
`bbnf-simd/src/aarch64/classify_tbl4.rs`, `bbnf-simd/src/lib.rs`,
`codegen/src/runtime_generator.rs`, `runtime/.../css_l4_declaration_values/generated.rs`,
`runtime/src/runtime_simd.rs`. *Adaptation:* kernel edits map to `kernels/classify_tbl4/`; the
generated + template edits gate behind a compile-time flag (a codegen policy bit, default OFF)
so the CSS scan path emits the **existing** scalar/tbl4 route. *Rationale:* the animate +13.4%
regression is STRUCTURAL (+30% ins) — redress owed before landing (SPEC §0). *Declared seam:* the
flag is flipped ON by **GN-2.W3** after it hoists the per-call LUT build + short-span scalar
guard and proves animate ≤1.0× with the 3-corpus wins retained. *Gate (GN-1):* with the flag OFF,
`check-css-l4-declaration-values` byte-identical to the W1 golden (the kernel is dormant-but-
present, zero live-path change); the kernel's own checkasm parity PASS (it is a real, tested
kernel, not a stub). This is **feature-flag limbo by declaration** — named §7, cleared GN-2.W3.

**Gate (wave-level).** Each patch reproduces its VERIFY delta on the W2-anchored baseline; all
regen targets byte-identical; all checkasm parity PASS; no §0 lane regresses beyond noise.

**Consumer in this wave (Y.13).** PROVE-12/05 → CSS canon bench; PROVE-03 → checkasm +
json_canon_bench twitter lane; PROVE-01 → prefilter_parity + prefilter_bench (kernel-grade;
end-to-end GN-2.W1); PROVE-06 → json_path_bench; PROVE-07 → json_canon_bench fused lane;
PROVE-13 → csv-prove + check-csv.

**Oracles / benches.** Cold paired PMU per patch (§1.2 P43–49); family-B parity before every new
timed arm; checkasm for every kernel.

---

### 4.5 GN-1.W5 — THE ASSAY *(challenge wave)*

**Objective.** Adversarially re-measure the tranche's own claims: independently reproduce the
SPEC §0 numbers on the *fully ported and patched* greenfield tree (not the prototype worktrees),
and hunt the prototype residue the productionization law names as a defect class.

**Method.**
- **Re-measure §0 cold, fresh windows.** A non-author agent rebuilds `json_canon_bench` and
  `css_canon_bench` under `--release` (fat-LTO) and runs `paired 200` ≥3 separated windows on the
  greenfield tree, comparing against the W2 anchor and each patch's VERIFY delta. Every certified
  ratio must reproduce within noise with CI excluding 1.0 in the same direction (P44/P46/P48).
- **Residue hunt (the named defect classes).** Enumerate and adjudicate every piece of prototype
  residue, each either productionized-in-place or **named** with its owning wave:
  1. *Dormant kernel* — `SimdMode::Prefilter` is a consumer-less IR variant (VERIFY-01 attack 8:
     "a declared seam, not a hidden stub"). Confirm the live path still emits `Exact` and the
     seam's consumer is **GN-2.W1**; a variant with neither a live consumer nor a named wave
     **fails**.
  2. *Feature-flag limbo* — PROVE-05's CSS shufti kernel behind an OFF flag. Confirm the flag is
     OFF, the kernel is checkasm-tested (not a stub), and the clearing wave is **GN-2.W3**.
  3. *Hand-schema crutch* — `real_typed_schema.rs` (the hand-written direct schema PROVE-07/08
     lean on). Confirm it is named for deletion at **GN-3.W2** (derived ≡ hand schema, then the
     hand file dies).
  4. *Honestly-unmet bar* — PROVE-07's 2× serde on twitter (thin projection) and PROVE-06's
     deep-path 2.0× loss. Confirm both are documented non-goals-for-now with owning waves
     (GN-3.W3, GN-4.W2), not silently claimed.
  5. *Bench-only wiring* — grep the ported tree for arms wired only in `bbnf-bench` with no
     runtime/product caller. The restored `json_canon_bench` fused lane must trace to
     `runtime::generated_json::parse_direct` (a real product path), not a bench-local shim.
  6. *Dropped-live audit* — confirm W2's dead-probe reclaim dropped nothing the canon bins or
     parity oracles depend on (rebuild + link proves the closure was complete).
- **Overfit / name-gate grep.** `grep`-gate zero grammar-name predicates in `codegen` routing
  (PROVE-13 cured `!has_rule("value")`; the disclosed `json_sink_direct` `name == "value"`
  residue is PRE-EXISTING JSON-only, named for GN-5.W3 — confirm it did not spread).

**Gate (born-RED).**
```
cargo build --release -p bbnf-bench --bin json_canon_bench --bin css_canon_bench
./target/release/json_canon_bench paired 200            # ×3 windows, fresh
./target/release/css_canon_bench paired 200             # ×3 windows, fresh
grep -rn 'has_rule("value")\|== "value"\|name == "json"\|name == "css"' crates/codegen/src
```
**Threshold / failure semantics.** The tranche **fails** if: (a) any certified §0 ratio regresses
beyond window CI on the greenfield tree relative to its VERIFY delta; or (b) any residue is found
with neither an in-place productionization nor a **named** owning wave (an undisclosed dormant
kernel, a bench-only arm with no product caller, a silently-dropped live probe); or (c) the
name-gate grep finds a *new* grammar-name predicate in codegen routing. This is genuinely
falsifiable: a botched patch adaptation that regressed a lane, or a residue smuggled in without a
named home, turns this gate RED and re-opens the offending wave.

**Consumer in this wave (Y.13).** The challenge itself consumes every prior wave's output and the
§0 evidence table; its artifact is the re-measured, residue-adjudicated certification that the
greenfield tree carries its wins honestly.

---

## 5. LOC ledger

| Wave | New/restored | Reclaimed/deleted | Relocation payload (moved, not written) |
|---|---:|---:|---:|
| W1 | +180 (workspace manifest, module-path wiring, `mod.rs` glue) | 0 | ~13.9 KLOC hand + ~5.2 KLOC generated |
| W2 | +420 (json_canon restore 298 + CSS paired ~80 + wiring) | −~2,500 (dead-probe closure) | — |
| W3 | +300 (sync gate script + hook wiring + facade) | 0 | ~3.0 KLOC (parse-that-regex vendored) |
| W4 | +~3,300 (six patches: mostly PROVE-13 CSV generated ~900 + PROVE-01 ~893 + PROVE-06 ~500 + 03/07/12/05) | −~200 (name-gate + superseded routing) | — |
| W5 | +120 (residue-audit notes as tracked doc-adjacent gate scripts) | 0 | — |
| **Total** | **+~4,320** | **−~2,700** | **~22 KLOC relocated** |

Generated-size budget (P53): the CSS generated delta is **+18** (PROVE-12); the CSV generated dir
is net-new and bounded by its grammar; `lint-loc` gate enforces per-grammar budgets on regen. No
O(N) generator regression is permitted — the monomorphs PROVE-12 introduces are compiler-emitted,
not template-duplicated.

## 6. Exit criteria

1. `cargo build --workspace` green, both aarch64 profiles (`release`, `ax-iter`); no x86 tokens
   anywhere (GN-0 purge held).
2. All four regen targets (`json`, `css-l4-declaration-values`, `sheets`, `csv`) byte-identical;
   `primitive-checkasm` parity PASS for every kernel dir.
3. `parse-that-regex` sync gate `--check` byte-identical to pin, `--verify` compiles all
   consumers; pre-push hook wired.
4. The W2 §0 baseline reproduces the sk-v22 graduation numbers within noise on the ported-
   unpatched tree; the CSS paired mode emits CI ratios on all four corpora.
5. All six born-verified patches reproduce their VERIFY deltas; PROVE-05 flag OFF and
   checkasm-tested; PROVE-01's `SimdMode::Prefilter` seam named to GN-2.W1.
6. W5 born-RED gate GREEN: §0 re-measured on the greenfield tree within CI; every residue named
   with an owning wave; zero new grammar-name predicates in codegen.
7. `skinny/` archived (the port is complete; GN-0's retain-until-GN-1 condition satisfied).

## 7. Risks with named counterexamples

- **R1 — the kernel-first restructure breaks a NEON body.** *Counterexample:* moving
  `aarch64/bracket_depth_mask_64.rs` + `scalar/bracket_depth_mask_64.rs` into
  `kernels/bracket_depth/{aarch64,scalar}.rs` mis-wires the `#[cfg(target_arch="aarch64")]`
  dispatch in the new `mod.rs`, so the scalar path runs on aarch64 and `bracket_depth_mask`
  silently returns the reference result at NEON-absent speed. *Guard:* W1's `primitive-checkasm`
  gate — NEON==scalar per kernel — turns RED on any mis-wire; the parity anchor is adjacent to
  its body by construction (the reason for the kernel-first cut).
- **R2 — the relocation silently moves a §0 number.** *Counterexample:* the `codegen`
  decomposition drops a `#[inline(always)]` on the CSS `find_component_delim` when rehoming the
  template under `templates/css/`, and tailwind track1 cyc/B drifts +8% — invisible to a build-
  green check. *Guard:* W2's baseline-reproduction gate on the *unpatched* port catches any lane
  regression before patches muddy the signal; a miss re-opens W1.
- **R3 — the vendored parse-that-regex drifts from upstream truth.** *Counterexample:* PROVE-03's
  `parse-that-regex/src/lib.rs` hunk is hand-applied to the vendored copy (not upstreamed), so the
  copy silently diverges from its pin and a later `--update` clobbers it. *Guard:* W3's `--check`
  byte-diff fails loud on any un-pinned drift; PROVE-03's regex-side change must be an upstream pin
  bump or an acknowledged vendored-local delta (§4.4), never a silent edit.
- **R4 — a dormant kernel masquerades as landed.** *Counterexample:* `SimdMode::Prefilter` is
  read as "the prefilter is wired" when the live scan still emits `Exact`, and GN-2.W1 finds an
  unconsumed variant it assumed was live. *Guard:* W5's residue hunt explicitly confirms the seam
  is consumer-less-by-declaration with GN-2.W1 named; the born-RED gate fails if the naming is
  absent.
- **R5 — the dead-probe reclaim drops a live dependency.** *Counterexample:* W2 archives
  `materialization.rs` as graveyard, but `real_typed_struct` transitively links it, and
  `json_canon_bench` fails to build only under `--release`. *Guard:* W2's gate builds both canon
  bins under `--release`; W5's dropped-live audit rebuilds the full closure.

---

*GN-1 closes when the loom holds the whole cloth: five organs rehomed under the colocation edict,
six wins installed and re-measured, one regex seam gated to its upstream truth, and skinny
retired to the archive — the greenfield born production-grade with its performance proven, not
promised.*
