# GN-2 — THE KERNEL PLANE

The tranche wherein the proven NEON organs, ported dormant into the loom, are given their
consumers and driven onto real grammar scan sites — the prefilter earns its skip floor
against an expensive consumer, the string kernel closes the last scalar leaf, the CSS
tokenizer sheds its animate regression, the movemask stitch is narrowed at the classify
floor, the canada float lane is redressed at the conversion tax, and every kernel is put
under a checkasm-liveness harness that adjudicates asm against intrinsics per the dav1d law.

Status: born of the SK-V24 codex (`restart/skinny/tranches/sk-v24/SPEC.md` §2, FROZEN
2026-07-16). Executes after GN-1 (the loom) lands, in parallel with GN-3 (the type plane).

---

## 1. Charter

### 1.1 Mission

GN-1 ports the certified kernels into the greenfield workspace with their PROVE patches
installed but their live consumers unwired — a deliberate, disclosed residue. The prefilter
primitive exists (`PrefilterSet::find_member`, cost-driven backend) but the recognizer pass
still emits `SimdMode::Exact` and no codegen lowering reads it; the fused string terminal
kernel serves the trusted long-string path but the generated scalar short-string leaf bypasses
it; the CSS shufti tokenizer is landed behind a flag OFF because it regresses animate; the
structural classify path, wired into `scan_dispatch`, runs at a 0.78–0.84 cyc/B floor bound
by the per-16B movemask stitch; the canada eager float lane still loses to
sonic-rs by ~1.20×. GN-2 is the wave series that converts each dormant kernel into an
end-to-end, measured, production win — or, where the prototype refuted the lever (PROVE-02,
PROVE-04), redresses the ACTUAL bottleneck the refutation named.

The tranche's product is the kernel plane at production grade: every SIMD organ has a live
grammar-side consumer, a cold paired PMU gate that can fail, a checkasm-liveness parity anchor,
and a documented asm-earn adjudication. No dormant kernel survives GN-2. No bench-only wiring
survives GN-2. That prototype residue — the named defect this campaign hunts — is precisely the
`SimdMode::Prefilter` seam PROVE-01 disclosed, the `match_tiny_plain_string` scalar bypass
PROVE-03 disclosed, and the animate redress PROVE-05 owed; W1–W3 exist to extinguish them.

### 1.2 Precept bindings (sk-v23 research/07)

- **8, 37, 40 — substrate lands with its consumer, same wave (Y.13).** Every wave here closes
  on a live grammar-side consumer reading the kernel through a runtime gate, not on a
  microbench in isolation. W1's four-hop pipeline — IR fact (`SimdMode::Prefilter`) → cost
  model (`select_prefilter_backend`) → emitter selection (recognizer lowering) → consumer reads
  the const (a real grammar scan site benched) — closes all four hops in the wave or the wave
  does not open. This is the tranche's governing law; the master pathology (§4 empirical spine:
  substrate-without-consumer, 14/18 dead) is what it exists to defeat.
- **14, 15 — no god modules; directory modules.** New kernels, lowering, and harnesses live in
  directory-modules owning their submodules (`codegen/src/lower/recognizer/`,
  `bbnf-simd/tests/checkasm/`), never flat siblings, never a `utils.rs` dump.
- **16, 54 — tests live in `tests/`.** Every parity oracle and bench is a `tests/` or
  `bench/` binary; zero `#[cfg(test)]` in `src/`.
- **19 — a hard gate is valid only with an artefact.** Each gate below cites an exact command
  and a numeric threshold over a build/test/bench/disasm/regen-diff artefact; grep checks are
  supplementary only.
- **43–49 — the profiling standing law.** PMU cyc/B via `proc_pid_rusage(RUSAGE_INFO_V5)
  .ri_cycles`; paired per-sample interleaving; cold-per-parse N≥200; ≥3 separated windows;
  per-block loadavg recorded; ins/B the load-invariant check. Ratios certified, absolutes
  directional.
- **53 — generated files are fresh-regen output.** W2 and W3 touch generated parsers ONLY via
  the codegen template + `cargo xtask regen`; the on-disk file is a regen artefact, never
  hand-patched, and the wave carries a `generated_loc_budget`.
- **2, 4, 6, 50 — no workarounds/stubs/legacy; abrogate before patch.** W4 and W5 confront
  refuted levers; the disposition is to attack the named real bottleneck or delete, never to
  patch a dead lever into apparent life.

### 1.3 REFUTATION GUARDS (SPEC §0 laws that constrain this tranche)

These are binding negative constraints. A wave that violates one is rejected at integration
regardless of its numbers.

- **No digit-scan SIMD (PROVE-02 NO_GO, now LAW).** The Mula/Lemire SIMD digit kernel is
  latency-bound on the cold parser: −37/−48% instructions but CPI inverts 0.13→0.30, regressing
  parse_tape +50%. W5 targets the FLOAT CONVERSION (`materialize_f64` / Eisel–Lemire tax), never
  the digit scan. Any W5 route that re-introduces a SIMD digit-run accumulator is refused.
- **No block-scheduling pursuit (PROVE-04 refuted as a lever).** Multi-block software pipelining
  buys only 1.05–1.06× — the M-class OoO core already pipelines the single-block loop. W4's
  lever is the MOVEMASK NARROW + vectorized compaction at the 0.78–0.84 cyc/B classify floor,
  NOT deeper block staging. A W4 that chases a third block in flight is refused.
- **No shim perf claims (PROVE-09 refuted).** The portable_simd→NEON shim's 1.34–1.57× did not
  reproduce (it emits 2× the movemask reduces). No wave here claims speed from a portability
  shim; where portability is engineered its value is stability, stated as such.
- **Fold-over-cursor is dead** (2.3–4.9× behind sonic). No wave revives a lazy-cursor fold.
- **Tape-navigation is NOT the get lane** (PROVE-06 measured it at 150–1700× the stop-early
  cost). GN-2 kernels feed the eager tape and the fused sink; the get lane is GN-4's, not built
  on tape navigation here.
- **Warm benches are disinformation.** Every number in this tranche is cold-per-parse, paired,
  multi-window. A warm or cached figure voids the wave.

### 1.4 Supersessions

GN-2 supersedes the PROVE-01/03/04/05 REPORT "integration cost / seam" sections as forward
plans: those sections named the wiring owed; this tranche IS that wiring, specified at
file level. PROVE-02's REPORT verdict (NO_GO for the digit kernel) stands unamended — W5 does
not relitigate it, it executes the redress PROVE-02 §7 pointed at. PROVE-04's "route the F1
effort at the movemask narrow, not the block schedule" is promoted from a closing remark to
W4's charter.

---

## 2. Interlocks

### 2.1 Depends-on

- **GN-1.W-final (the loom, born-verified).** GN-2 requires, present in the greenfield tree
  and reproducing §0 numbers within noise: the prefilter primitive (`PrefilterSet`,
  `find_member`, `select_prefilter_backend`, `bbnf-simd/src/kernels/prefilter/`,
  PROVE-01 patch landed); the fused string terminal (`bbnf-simd/src/kernels/string_block/aarch64.rs::
  first_string_terminal` + rewired `skip_string_plain_trusted`, PROVE-03 patch landed,
  born-verified); the CSS shufti kernel + bridge (`bbnf-simd/src/kernels/classify_tbl4/`,
  `runtime/.../runtime_simd::find_css_significant`, PROVE-05 patch landed behind the animate
  flag OFF); the structural classify path W4 narrows — the vceqq fan
  (`bbnf-simd/src/kernels/structural_classify/`) + the `vshrn_n_u16::<4>` narrow
  (`bbnf-simd/src/kernels/movemask/`), wired through `scan_dispatch` (`bbnf-simd/src/lib.rs:105`);
  the ported Eisel–Lemire module (`parse-that-regex/src/number/eisel_lemire/`); the
  canon benches `json_canon_bench` (TRACKED) + `css_canon_bench` + the paired-mode CSS
  extension (R2 gap closed in GN-1). If any is absent GN-2 does not open — it is a GN-1
  regression, escalated, not worked around.

### 2.2 Runs-parallel

- **GN-3 (the type plane), one shared file, disjoint functions.** GN-3 owns the type-plane
  inference (`infer_expr`'s `Project` arm), the `type_plane/` schema derivation, and the
  `json_typed_direct.rs` typed-struct renderer. GN-2 owns the recognizer/kernel lowering and
  the number-conversion lane. In `codegen` the two proceed as disjoint directory-modules and
  files (precept 15): GN-2.W1 adds `codegen/src/lower/recognizer/`; GN-3.W2/W3 grows
  `codegen/src/type_plane/` + `codegen/src/json_typed_direct.rs`. The genuine collision is one
  shared FILE, `crates/passes/src/lib.rs`: GN-2.W1 edits `recognizers::derive_recognizers`
  (`:338`); GN-3.W2 edits `infer_expr` (`:204`) + `derive_materialization_plan`
  (`:1149-1287`). These are disjoint functions in the same file — the orchestrator serializes
  `passes/src/lib.rs` writes and the second lander rebases on the first's passes commit
  (worktree isolation, cherry-pick not merge). Named risk: GN-3 R3 (§7).

### 2.3 Feeds

- **GN-4 (the value fabric).** GN-2.W1's `find_member` lowering and GN-2.W2's routed string
  kernel are consumed by GN-4.W1 (public path API) and GN-4.W3 (direct-to-struct public API).
  GN-2.W5's redressed float lane feeds GN-4.W3's struct sink. GN-2 hands GN-4 kernels that are
  already live on the eager path, so GN-4 productionizes an API over a working substrate, not a
  dormant one.

### 2.4 Cross-repo interlock

- **PT-1.W5 ⇄ GN-2.W1 (parse-that regex track, no double-emit).** Both waves land codegen
  prefilter-lowering into the same `codegen/src/lower/recognizer/` dispatch. GN-2.W1's
  `lower/recognizer/` owns the single dispatch that selects `PrefilterSet::find_member` (bbnf-simd,
  `SimdMode::Prefilter` / `SelfLoop`) for self-loop scan sites; PT-1.W5 adds the `dfa.find` branch
  (bbnf-regex prefilter) for unanchored regex-leaf scan sites (SPEC §2b PT-N1, "generator inherits
  by emitting `dfa.find`"). One recognizer dispatch decides which lowering fires per site — it
  emits exactly one lowering per site, self-loop XOR regex-leaf, never both (no double-emit). The
  two waves are file-adjacent in `codegen`; the orchestrator lands GN-2.W1's dispatch skeleton
  before PT-1.W5's branch so the branch has a home. PT-1.W5's branch is counted in PT-1's LOC
  ledger, not GN-2's — this interlock is coordination, not new GN-2 code.

### 2.5 Owner gates

- The profiling standing law's **quiet-host certification is an execution-time gate** (precept
  49): ratios are decision-grade at loadavg 6–25 via per-process PMU + paired interleaving;
  the quiet-host re-capture of absolutes is a GN-6 carried residual, not a GN-2 blocker.
- No §3 SPEC checkbox blocks GN-2 (checkbox 3, GN-0 timing, is already resolved by the freeze).

---

## 3. Wave manifest

| id | name | scope (one line) | LOC (+new/−del) | gate | hard cap |
|----|------|------------------|-----------------|------|----------|
| GN-2.W1 | Prefilter End-to-End | wire `SimdMode::Prefilter` recognizer → `find_member` lowering onto a real self-loop scan site | +190 / −15 | born-RED | 30 min |
| GN-2.W2 | String Kernel Follow-On | route generated scalar `match_tiny_plain_string` to `first_string_terminal` via template + regen | +55 / −30 | born-RED | 25 min |
| GN-2.W3 | CSS Animate Redress | hoist per-call LUT build + short-span scalar guard in `find_css_significant`, then flip PROVE-05 ON | +70 / −35 | born-RED | 30 min |
| GN-2.W4 | Movemask-Stitch Narrow | narrow the 0.78–0.84 cyc/B classify floor via cheaper mask + compaction, wired into `scan_dispatch` | +130 / −70 | born-RED | 30 min |
| GN-2.W5 | Canada Float Redress | attack the `materialize_f64` / Eisel–Lemire conversion tax; NO digit-scan SIMD | +180 / −45 | born-RED | 30 min |
| GN-2.W6 | Checkasm-Liveness + Asm-Earn (CHALLENGE) | adversarial re-measure of W1–W5 + per-kernel checkasm harness + asm-earn adjudication | +340 / −10 | born-RED | 30 min |

Net: **+965 / −205**. `generated_loc_budget` (precept 53): W2 ≤ +40 generated LOC, W3 ≤ +25
generated LOC; overflow blocks the wave until traced to an O(N) generator regression.

---

## 4. Per-wave spec

### GN-2.W1 — Prefilter End-to-End

**Objective.** Convert PROVE-01's disclosed residue (`SimdMode::Prefilter`, an unconsumed enum
variant with the live scan still emitting `Exact`) into a wired lane: the recognizer pass
detects self-loop scan sites, the codegen lowers them to `PrefilterSet::find_member`, and a
real grammar scan site beats 8× against the expensive consumer it replaces. This closes the
master pathology's canonical instance — the certified 0.115 cyc/B skip floor becomes an
end-to-end parser win, not a microbench.

**Why 8× and not 15×.** PROVE-01/VERIFY established the arithmetic (0.115 cyc/B floor × 15 =
1.725 cyc/B; against a 1.03–1.07 cyc/B membership scan the physical ceiling is ~8.96×). The 15×
bar presupposed a ≥1.75 cyc/B consumer. W1's gate is therefore 8× against a REAL grammar scan
site whose per-byte cost is a bare membership skip (≈1.0 cyc/B) — the honestly achievable
ceiling — with the ≥15× reserved for GN-4's string/DFA-scanner consumers (2–5 cyc/B). Setting
8× against a 1.0 cyc/B consumer is a gate that CAN fail: an unwired lowering, a backend
mis-selection, or a gate-16 mis-tune all leave it RED.

**File-level plan.**
- `ir/src/lib.rs:425` — extend `enum SimdMode { Exact }` → `{ Exact, Prefilter }` and
  `enum SimdSite { PreEntry }` → `{ PreEntry, SelfLoop { terminators: StructuralAlphabet } }`.
  The `SelfLoop` site carries the terminator alphabet (the small member set that ENDS an inert
  run); the recognizer skips the run to the next terminator. (+8/−2)
- `passes/src/lib.rs:338` `recognizers::derive_recognizers` — add a second detection arm:
  a `ExprKind::Repetition`/Kleene over a character-class or negated-set whose complement
  cardinality ≤ 16 is a self-loop scan site; emit `Recognizer::SimdScan { mode: Prefilter,
  alphabet: <complement = terminators>, site: SelfLoop { terminators } }`. Keep the existing
  `Exact`/`PreEntry` arm untouched. The detection is grammar-structural — zero grammar-name
  gates (precept 9; PROVE-01/VERIFY attack 6). (+55/−5)
- `codegen/src/lower/recognizer/mod.rs` — NEW directory-module (precept 15). Reads the
  `Recognizer` vector (today consumed only as a header comment at
  `codegen/src/templates/json/generated.rs:10`) and, for a `Prefilter`/`SelfLoop` recognizer,
  emits a `PrefilterSet::new(terminators)` construction at parser init + a `find_member` call
  replacing the scalar self-loop body. `mod.rs` owns the lowering; `mod.rs`'s `emit.rs`
  submodule owns the token construction. This is the shared recognizer dispatch PT-1.W5 co-lands
  in: PT-1.W5 adds a `dfa.find` branch for unanchored regex-leaf sites into this same
  `lower/recognizer` dispatch, which emits exactly one lowering per site — self-loop `find_member`
  XOR regex-leaf `dfa.find`, never both (no double-emit; §2.4 interlock). GN-2.W1 lands the
  dispatch skeleton first so PT-1.W5's branch has a home. (+70)
- `codegen/src/templates/json/` + the grammar carrying the self-loop site — regen via
  `cargo xtask regen`; the generated delta is the `find_member` call site. (+~35 generated,
  within a raised W1 generated budget of +50 since W1 introduces the lowering)
- `bbnf-bench/src/bin/prefilter_e2e_bench.rs` — NEW cold paired PMU harness benching the wired
  grammar scan site (fused `find_member` lane) vs the scalar membership self-loop it replaced,
  parity-asserted (identical structural positions) before timing. (+ bench, not counted in src
  budget)

**Seam citations.** `passes/src/lib.rs:338` (derivation), `ir/src/lib.rs:425` (SimdMode/Site),
`codegen/src/templates/json/generated.rs:10` (the recognizer is stamped but not lowered),
`bbnf-simd/src/lib.rs:250` (`prim` facade housing `PrefilterSet`), PROVE-01 REPORT §6 (the seam
narrative), R7 §3 + local-reuse map (the ported shufti/truffle kernel lineage now living at
`bbnf-simd/src/kernels/prefilter/aarch64.rs`; its archived legacy bodies — shufti, truffle — are
recoverable via `git show pre-greenfield-2026-07:crates/simd-scan/src/neon.rs` at `:208`/`:335`,
not live files).

**PROVE patch adaptation.** The PROVE-01 patch (9 files, +893/−0) lands whole in GN-1. W1 lands
NO part of that patch — it lands the CONSUMER the patch disclosed as owed. The `SimdMode::
Prefilter` variant PROVE-01 added (+4) is superseded by the richer `SelfLoop`-carrying variant
here; GN-1 ports the PROVE-01 variant, W1 replaces it (no legacy alias, precept 6).

**GATE (born-RED).** `cargo build --release -p bbnf-bench --bin prefilter_e2e_bench &&
./target/release/prefilter_e2e_bench 200` over ≥3 windows: the wired `find_member` lane must
beat the scalar self-loop it replaces by **≥8.0× cyc/B** on the skip-heavy real grammar site,
with every window's 95% CI excluding 8.0 in the same direction, AND remain **≥0.95× (neutral)**
on the dense corpus (twitter) — the dense-neutrality invariant PROVE-01 certified. Failure
semantics: <8.0× on the skip site → the lowering or backend selection is defective (RED);
<0.95× dense → the gate-16 fast path regressed dense (RED); parity mismatch before timing →
correctness failure, wave halts. Artefact: the bench stdout + `cargo test -p bbnf-simd
prefilter_parity` (3/3).

**Consumer IN this wave (Y.13).** The wired grammar scan site read by `prefilter_e2e_bench` IS
the consumer; the four hops (IR variant → `select_prefilter_backend` → `lower/recognizer` emit
→ generated `find_member` call) all close inside W1.

**Oracles/benches.** Parity oracle `prefilter_parity` (byte-identical positions, NEON vs scalar,
set sweep 1..=16, alignment 0..64) rebuilt and re-run; `prefilter_e2e_bench` cold paired PMU,
N≥200, ≥3 windows, per-block loadavg recorded, ins/B reported as the load-invariant check.

---

### GN-2.W2 — String Kernel Follow-On

**Objective.** PROVE-03 (GO, CONFIRMED) landed the fused `first_string_terminal` on the trusted
LONG-string path in GN-1. Its disclosed follow-on: `parse_tape`/`fused` short strings still
route through the generated SCALAR `match_tiny_plain_string` (a byte-at-a-time loop), bypassing
the NEON kernel. W2 routes that generated scalar leaf to `first_string_terminal` via a template
change + regen, compounding the twitter parse_tape win.

**File-level plan.**
- `codegen/src/templates/json/` (the template emitting `match_tiny_plain_string` at
  `codegen/src/templates/json/generated.rs:93`) — replace the scalar byte-loop body with a call
  into `bbnf_simd::kernels::string_block::first_string_terminal` for the ≥16-byte prefix, SWAR-8
  + scalar tail for the remainder (mirroring the `skip_string_plain_trusted` shape PROVE-03
  already proved bit-identical). (+~20 template)
- `codegen/src/emit/json_sink_direct/skeleton.rs:156` — the direct sink's
  `match_tiny_plain_string_direct` twin routed identically (the fused lane's short-string leaf).
  (+~15 template)
- Regen: `cargo xtask regen` → `runtime/src/grammars/json/generated.rs:93` and the direct-sink
  generated file carry the new call site. Generated delta ≤ +40 LOC (budget). (+~40 generated,
  −~30 scalar loop deleted)
- No new kernel: W2 reuses the GN-1 `first_string_terminal`. The change is purely which leaf the
  generator emits (precept 5: one path — the scalar short-string leaf collapses into the same
  kernel the long path uses).

**Seam citations.** `runtime/src/grammars/json/generated.rs:93` + `codegen/src/templates/json/
generated.rs:93` (the scalar `match_tiny_plain_string` call), `codegen/src/emit/json_sink_direct/
skeleton.rs:156` (`match_tiny_plain_string_direct`), `bbnf-simd/src/kernels/match_tiny_string/
aarch64.rs` (the existing scalar/NEON tiny-string kernel — evaluate whether its NEON twin already
suffices vs `first_string_terminal`; if so, route to it and DELETE the scalar-only emit),
PROVE-03 REPORT §"Follow-on" (the exact disclosure).

**PROVE patch adaptation.** No PROVE patch lands here (PROVE-03's patch is GN-1's). W2 is a
template edit + regen only.

**GATE (born-RED).** `cargo build --release -p bbnf-bench --bin json_canon_bench &&
./target/release/json_canon_bench pmu 400` interleaved vs the GN-1 baseline (PROVE-03-landed
tree), ≥3 windows: twitter **parse_tape cyc/B compound win** — the routed short-string leaf must
push twitter parse_tape strictly below the GN-1 baseline ratio (GN-1 landed ~0.898× vs the
pre-PROVE-03 base; W2 must improve parse_tape further, CI excluding 1.0× vs GN-1 baseline in the
same direction across all windows), with deterministic ins/B strictly lower (load-invariant
proof). Failure semantics: parse_tape not below GN-1 baseline → the short-string leaf was not on
the hot path or the kernel dispatch mis-fired (RED); ins/B not lower → no work was removed
(RED). Artefact: bench stdout + `cargo xtask regen` producing a byte-identical file on re-run
(idempotent regen proof, precept 53).

**Consumer IN this wave (Y.13).** `json_canon_bench` parse_tape twitter lane is the live
consumer; the generated parser calling `first_string_terminal` on the short-string leaf is the
wired hop.

**Oracles/benches.** Reuse `prove03_string_kernels` (7 tests) + `checkasm_string_terminal_block`
(5 tests, 200k fuzz) — both must pass post-regen. `json_canon_bench` cold paired PMU, N≥400,
≥3 windows.

---

### GN-2.W3 — CSS Animate Redress

**Objective.** PROVE-05 (PARTIAL / CONFIRMED_DELTA) landed the CSS shufti tokenizer in GN-1
behind the animate flag OFF: it widens the lightningcss band on bootstrap/tailwind/material
(tailwind ratio 3.25→**4.16×** verified) but REGRESSES animate +13.4% cyc/B / +29.9% ins —
STRUCTURAL, not noise (verifier-confirmed). animate.css is `@keyframes`-dense with 5–10-byte
components; the per-call 64-byte shufti classify + LUT build is not amortized. W3 hoists the LUT
build out of the per-call path and adds a short-span scalar guard, then flips the tokenizer ON.

**File-level plan.**
- `runtime/src/runtime_simd.rs` `find_css_significant` — the LUT build (`fixed ∪ delimiters` →
  two `uint8x16_t` shufti tables) currently happens per-call. Hoist it: build the two LUTs ONCE
  per parse (at component-scan entry) and thread the two `uint8x16_t` through
  `find_css_significant` as parameters, so keyframe-dense corpora stop paying the un-amortized
  table construction on every short span. (+~30/−15)
- `runtime/src/runtime_simd.rs` + the codegen template — add a **short-span scalar guard**: when
  the remaining span to the next likely delimiter is < a threshold (the 64-byte block is not
  amortizable), fall to the scalar switch walk rather than engaging the vector classify. The
  threshold is a NAMED const (`CSS_SHUFTI_MIN_SPAN`, e.g. 32 bytes) exposed as a cost-model
  value, not a heuristic literal buried in the loop (precept: pluggable decision points).
  (+~25/−5)
- `codegen/src/runtime_generator.rs` `CSS_GENERATED_RS` — regen `find_component_delim` /
  `consume_balanced_at` to pass the hoisted LUTs + honor the guard. `cargo xtask regen-css`.
  Generated delta ≤ +25 LOC (budget). (+~15 generated/−15)
- Flip the GN-1 animate flag OFF→removed: the tokenizer is unconditionally ON once the redress
  clears animate (precept 6: no feature-flag-and-keep; the flag DIES, not lingers). (−~15 flag
  plumbing)

**Seam citations.** `runtime/src/runtime_simd.rs:14` (`find_css_significant` bridge),
`codegen/src/runtime_generator.rs` `CSS_GENERATED_RS` (`find_component_delim` /
`consume_balanced_at` call sites), `runtime/src/runtime_scan.rs:92` (the delim scan),
PROVE-05 REPORT §5 (the exact redress prescription: "hoist the LUT build out of the per-call
path … and/or fall to the scalar walk for short remaining spans"), PROVE-05/VERIFY attack 2
(the +29.9% ins structural cost the redress must erase).

**PROVE patch adaptation.** PROVE-05's patch (5 files, +175/−22) lands in GN-1 flag-OFF. W3
amends `runtime_simd.rs` + the template + regen and removes the flag. The kernel
(`classify_tbl4`) and prim facade are unchanged.

**GATE (born-RED).** Build BOTH arms (redressed vs GN-1 flag-OFF baseline), interleaved
back-to-back, `CSS_CANON_PMU=1 … 800`, ≥6 windows per PROVE-05's own method, all 4 corpora:
**animate track1_rich ≤ 1.0× (neutral-or-better) vs the pre-tokenizer scalar baseline** — the
+13.4% regression erased — AND the three-corpus wins RETAINED (bootstrap/tailwind/material each
still ≥ their GN-1 landed cyc/B improvement, tailwind lcss ratio still ≥ 4.0×). Lead with
deterministic ins/B: animate ins Δ must return to ≤ +2% (from +29.9%). Failure semantics:
animate cyc/B > 1.0× or ins Δ > +2% → the hoist/guard did not amortize (RED); any of the three
big corpora dropping below its GN-1 win → the guard over-triggered and killed the dense win
(RED). Artefact: the interleaved A/B PMU table + `cargo xtask regen-css` idempotent-on-disk
proof.

**Consumer IN this wave (Y.13).** `css_canon_bench` all-4-corpora lane is the consumer; the
generated CSS parser calling the hoisted-LUT `find_css_significant` is the wired hop.

**Oracles/benches.** `neon_significant_skip_matches_scalar` (exhaustive: 5 corpora × 4 delimiter
sets × every offset) + the `css_l4_*` route/projection suite (8 tests) must pass post-regen.
`css_canon_bench` cold paired A/B, ≥6 windows, cssparser/lightningcss as unchanged controls.

---

### GN-2.W4 — Movemask-Stitch Narrow

**Objective.** PROVE-04 refuted multi-block pipelining as the F1 lever (1.05–1.06× only; the OoO
core already pipelines the tight loop) and named the ACTUAL lever from the disassembly: the
classify stage runs at 0.78–0.84 cyc/B, ALU-bound on the per-16B movemask STITCH (3 masks × 4
chunks = 12 `zip/ushl/fmov/orr` reconstructions per 64B block). W4 narrows that stitch and
feeds compaction directly, cutting the classify floor, then wires the improved classify into the
live scan dispatch.

**File-level plan.**
- `bbnf-simd/src/kernels/structural_classify/` + `bbnf-simd/src/kernels/movemask/` — the vceqq
  classify fan and the `vshrn_n_u16::<4>` movemask narrow GN-1 ports (§4.0 kernel list; PROVE-04
  landed NO kernel — refuted as a lever, §1.3 guard 2). W4 reworks the classify path's mask
  production: replace the 12-stitch `vshrn`→`zip1`→`ushl`→`fmov`→`orr`
  reconstruction with a **cheaper narrow** — the two candidate mechanisms PROVE-04 named:
  (a) keep the 4-bits/byte `vshrn_n_u16::<4>` nibble mask and feed compaction directly (no
  reconstruction to a 1-bit/byte movemask), or (b) a `vaddv`-reduce narrow re-implemented in the
  greenfield kernel from the archived legacy body (`vaddv` reduce at
  `crates/simd-scan/src/neon.rs:690`, archived — recoverable via
  `git show pre-greenfield-2026-07:crates/simd-scan/src/neon.rs`, not a live file GN-2.W4 reads).
  The wave TESTS both under the harness and keeps the one
  that measurably lowers the 0.78–0.84 floor; the loser is deleted (precept 5: one path).
  (+~80/−60)
- `bbnf-simd/src/lib.rs:105` `scan_dispatch` — replace the block body (currently
  `prim::byte_class_from_table_64` → scalar + `compact_mask`) with `classify_block_neon` +
  the narrowed classify mask production, per PROVE-04's ~10-line seam. This is the live rewiring:
  the classify improvement reaches the already-wired scan dispatch, not just the microbench.
  (+~20/−10)
- `bbnf-simd/tests/structural_classify_bench.rs` — NEW paired PMU microbench over the ported
  classify kernel: the narrowed-classify lane vs the GN-1 single-block classify (0.78–0.84
  baseline), parity-asserted (oracle == single == narrowed, byte-identical positions on
  twitter/citm/canada). (+ bench)

**Seam citations.** PROVE-04 REPORT §"Why it stops at ~1.06×" (the disassembly locating the
0.78–0.84 floor at the movemask stitch), §"Integration cost + exact seam" (the `scan_dispatch`
seam at `bbnf-simd/src/lib.rs:105`; the `vaddv` reduce it names lives in the archived legacy body,
`git show pre-greenfield-2026-07:crates/simd-scan/src/neon.rs` at `:690`, not a live file),
`bbnf-simd/src/kernels/structural_classify/` + `bbnf-simd/src/kernels/movemask/` (the ported
classify fan + narrow), `bbnf-simd/src/lib.rs:250`
(`prim`), R7 §1 + local-reuse map (`movemask_u8x16` via `vshrn #4`, archived at
`git show pre-greenfield-2026-07:crates/simd-scan/src/neon.rs:683`).

**Refutation-guard compliance.** W4 does NOT add a third block in flight or deepen the schedule
(PROVE-04 refutation guard). The lever is strictly the narrow + compaction. A W4 patch that
touches the loop staging without changing the mask production is refused.

**GATE (born-RED).** `cargo test --release -p bbnf-simd --test structural_classify_bench --
--nocapture --exact prove04_narrowed_classify`, N≥200, iters=8, ≥3 windows: the narrowed
classify lane must lower the classify-only cyc/B **below 0.78** (from the 0.78–0.84 floor,
CI excluding 0.78 in the same direction across all windows) AND the full-scan cyc/B must not
regress vs the GN-1 single-block full-scan. Additionally, disasm verification (`otool -tV` of
the narrowed `scan_dispatch`) must show the 12-stitch reconstruction REMOVED (fewer
`zip1`/`ushl`/`fmov` per block) — a mandated visual artefact (precept 19). Failure semantics:
classify ≥ 0.78 → the narrow bought nothing, the floor is elsewhere (RED, and the finding is
that classify is memory-bound not stitch-bound — a documented negative result, not a silent
pass); parity mismatch → correctness halt. Artefact: bench stdout + the `otool -tV` diff.

**Consumer IN this wave (Y.13).** `scan_dispatch` at `bbnf-simd/src/lib.rs:105` is the live
consumer of the narrowed classify — GN-1 already wired the classify path through `scan_dispatch`
at the 0.78–0.84 floor; the wave IS the movemask narrow at that floor. (PROVE-04's block-schedule
lever was refuted and landed NO kernel, §1.3 guard 2; W4's defect-cure attacks the actual
bottleneck the refutation named — the stitch — not a dormant kernel.)

**Oracles/benches.** Parity: oracle == single == narrowed byte-identical positions on
twitter/citm/canada (non-string structural counts 30 318 / 93 731 / 223 235 as fixtures).
`structural_classify_bench` cold paired PMU, ≥3 windows.

---

### GN-2.W5 — Canada Float Redress

**Objective.** PROVE-02 refuted the SIMD digit kernel (NO_GO, latency-bound) and located the
canada `fused/sonic` loss precisely: eager `materialize_f64` — the Eisel–Lemire / float
CONVERSION tax (+1.48 cyc/B), NOT the digit scan. W5 attacks that conversion lane. The ported
`number/eisel_lemire/` module already has a fast path (`FAST_PATH_POW10`, `try_fast_path`,
`compute_f64`) and a slow fallback that hands off to libstd `text.parse::<f64>()` at
`number/mod.rs:271`. The primary hypothesis to TEST first: canada's coordinate floats are
flooding the libstd fallback rather than the EL fast/slow paths, paying a strtod-class tax.

**File-level plan.**
- **W5.a diagnostic (first hop, non-optional).** Instrument the fast-path hit rate on canada:
  `number/eisel_lemire/mod.rs` `try_fast_path` + `compute_f64` — count, on the canada corpus,
  how many of the 111,080 floats hit `try_fast_path`, how many hit the EL slow path, and how
  many fall to `number/mod.rs:271` `text.parse::<f64>()`. This is the load-bearing measurement:
  it decides which of the two SPEC-named levers applies.
- **W5.b (lever, route decided by W5.a).** Two routes, exactly the SPEC's two:
  - If canada floods the libstd fallback: **complete the Eisel–Lemire slow path** so zero canada
    numbers reach `text.parse::<f64>()` — implement the full 128-bit EL slow path (per Lemire /
    Nigel Tao) at `number/eisel_lemire/mod.rs`, with a Clinger/strtod fallback reserved only for
    the genuinely hard <0.1% (subnormals, >19 sig-digit). Delete the `text.parse::<f64>()`
    hand-off from the common path. (+~120/−30)
  - If the fast path already dominates: **deferred materialization** for the tape/lazy lanes —
    store the pre-computed `(mantissa, decimal_exp, negative)` span facts on the tape (already
    produced by `NumberParts`) and defer `compute_f64` to get-time for `parse_tape`, so the
    eager tape stops paying conversion; the FUSED sink (which needs the f64) keeps the completed
    EL path from the first route. (+~60/−15 additional)
  - NO digit-scan SIMD in either route (refutation guard). The mantissa is accumulated by the
    retained SWAR ladder / `NumberParts::push` exactly as today.
- `parse-that-regex/tests/float_materialize_parity.rs` — NEW: sweep every canada / twitter /
  citm numeric token through the redressed `materialize_f64`, assert `f64::to_bits()` equality
  vs libstd `f64::from_str` (the existing `eisel_materializer_matches_representative_bits` test
  at `number/mod.rs:431` is the seed; promote it to a full corpus sweep in `tests/`). (+ test)
- `bbnf-bench/src/bin/json_canon_bench.rs` — the canada `fused/sonic` and `tape/sonic` lanes are
  the gate (already present).

**Seam citations.** `parse-that-regex/src/number/mod.rs:261` (`materialize_f64`), `:271` (the
`text.parse::<f64>()` slow hand-off — the suspected tax), `number/eisel_lemire/mod.rs:60`
(`FAST_PATH_POW10`), `:112`/`:121` (`try_fast_path`), `:176` (`compute_f64` bit assembly),
`number/mod.rs:280` (`NumberParts` — the span facts already available for deferral), PROVE-02
REPORT §7 ("the canada redress must attack `materialize_f64`, not `scan_digit_run`"), §9
(salvage direction), R7 §6 (Eisel–Lemire reference, fast_float ~1.2 GB/s M1).

**PROVE patch adaptation.** PROVE-02's patch does NOT land (NO_GO). W5 discards it entirely and
authors a fresh redress at the conversion lane. This is the "abrogate before patch" precept (4)
applied: the digit kernel is deleted, not salvaged.

**GATE (born-RED).** `cargo build --release -p bbnf-bench --bin json_canon_bench &&
./target/release/json_canon_bench paired 300`, ≥3 windows, canada: **fused/sonic ≤ 1.05×**
(skinny within 5% of sonic-rs, i.e. ≥0.95× its throughput) — down from the ~1.20× graduation
baseline — with every window's 95% CI excluding 1.20 in the improving direction; parse_tape must
NOT regress (deferred materialization, if taken, must keep tape/sonic ≤ its GN-1 value). Float
parity is a precondition (byte-exact `to_bits` on all three corpora) checked BEFORE timing.
Failure semantics: fused/sonic > 1.05× → the conversion tax survives, the wave is RED and the
finding is that the tax is not in the libstd fallback (redirect W5.b to the EL slow path or the
digest, documented); any float parity mismatch → correctness halt, no timing reported. Artefact:
`json_canon_bench paired` stdout (3 windows) + `float_materialize_parity` PASS.

**Consumer IN this wave (Y.13).** `json_canon_bench` canada fused/sonic lane is the consumer;
the eager direct sink calling the redressed `materialize_f64` is the wired hop.

**Oracles/benches.** `float_materialize_parity` (full three-corpus `to_bits` sweep) +
`parse-that-regex` existing 30-test suite must pass. `json_canon_bench paired` cold, N≥300,
≥3 windows; ins/B reported (though the win here is a CPI/latency win, ins/B is the parity check
that no work was silently added).

---

### GN-2.W6 — Checkasm-Liveness + Asm-Earn (THE CHALLENGE WAVE)

**Objective (adversarial re-measure + policy).** This is the tranche's one challenge wave. It
does two inseparable things: (1) **adversarially re-measures every W1–W5 claim** on a fresh
tree by a non-authoring lane — re-runs each gate bench interleaved, in both interleave orders,
across ≥3 fresh windows, and re-affirms or refutes the born-RED thresholds with load-immune
ins/B leading where cyc/B and ins/B disagree; (2) installs the **checkasm-liveness harness** and
the **per-kernel asm-earn policy** (the dav1d discipline) as standing infrastructure, so every
kernel this tranche wired carries a differential NEON-vs-scalar parity anchor that runs in CI
and an adjudication of whether hand-asm may exist for it.

**The asm-earn adjudication (the vshrn precedent).** The empirical spine's settled fact: the
`vshrn` intrinsic OBVIATES hand-asm for the mask-narrow idiom (+35% over scalar, beats the asm
variant by ~25%; register-file crossings bind, not ALU). W6 codifies this into policy: **the
intrinsic implementation is the default; a hand-asm kernel may exist ONLY if it beats the
intrinsic on a per-kernel checkasm bench, measured cold paired.** No kernel in GN-2 ships
hand-asm — every one is `core::arch::aarch64` intrinsics with a scalar parity twin. W6 proves
this is the right call by measuring: for each of the five wired kernels (prefilter `find_member`,
`first_string_terminal`, CSS `find_css_significant`, narrowed `scan_dispatch` classify, and the
number-conversion lane's SWAR ladder), the checkasm harness confirms the intrinsic is at or
above any asm variant, and NO asm is admitted.

**File-level plan.**
- `bbnf-simd/tests/checkasm/` — NEW directory-module (precept 15) housing the liveness harness:
  a differential runner that, for each registered kernel, fuzzes N blocks and asserts the NEON
  intrinsic output is byte-identical to the scalar twin (the checkasm parity anchor PROVE-03/05
  already seeded with `checkasm_string_terminal_block` and `neon_significant_skip_matches_
  scalar`), PLUS a cold paired PMU micro-comparison harness for the asm-earn adjudication.
  Consolidates the per-kernel parity tests scattered across GN-1 into one harness with a kernel
  registry (no god module — the registry is a thin dispatch; each kernel's fuzz generator is its
  own submodule). (+~200)
- `bbnf-bench/src/bin/gn2_remeasure_bench.rs` — NEW: re-runs the W1–W5 gate lanes in one
  binary, interleaved both orders, emitting the re-measured ratios beside the wave-claimed
  thresholds and a PASS/REFUTE per wave. (+~120)
- `docs/tranches/sk-v24/GN-2-ASM-EARN.md` — the adjudication record: per-kernel intrinsic-vs-asm
  measurement, the verdict (intrinsic default, asm not admitted), and the standing policy for
  GN-4+ kernels. Rides this wave (precept 17: doc rides its change). (+~20 doc)

**Seam citations.** `bbnf-simd/tests/checkasm_string_terminal_block.rs` (PROVE-03 seed anchor),
`neon_significant_skip_matches_scalar` (PROVE-05 seed anchor), `bbnf-simd/tests/prefilter_
parity.rs` (PROVE-01 seed), `structural_classify_bench.rs` (W4 classify-narrow bench), empirical spine §4
(vshrn obviates asm; dav1d checkasm-liveness discipline), R7 §12 (dav1d two-stage / asm-earned
posture).

**GATE (born-RED — the checkasm-liveness harness is the sole born-RED gate).** `cargo test
--release -p bbnf-simd --test checkasm` (all kernel parity anchors PASS, ≥200k fuzz blocks each):
scalar-vs-NEON byte-identical parity AND liveness across the five wired kernels (prefilter
`find_member`, `first_string_terminal`, CSS `find_css_significant`, narrowed `scan_dispatch`
classify, the number-conversion SWAR ladder) is the wave's sole falsifiable born-RED gate — a
parity or liveness fail is a correctness halt across the tranche. Alongside it, the adversarial
re-measure runs `cargo build --release -p bbnf-bench --bin gn2_remeasure_bench &&
./target/release/gn2_remeasure_bench 200` over ≥3 fresh windows in BOTH interleave orders:
**every W1–W5 born-RED threshold must re-affirm** (W1 ≥8.0× skip / ≥0.95× dense; W2 twitter
parse_tape below GN-1; W3 animate ≤1.0× + three-corpus wins retained; W4 classify <0.78; W5
canada fused/sonic ≤1.05×) with the re-measured ratio inside the wave's disclosed CI, OR the
wave that fails re-affirmation is REOPENED (the challenge's teeth: a wave whose claim does not
survive fresh non-author re-measure is not closed — precept 39, "complete with recorded misses
is not a close state"). **The asm-earn clause is a standing ADMISSION POLICY, not a gate:** no
asm ships in GN-2, so there is nothing to refuse in this wave — the policy binds the FUTURE. Any
asm variant, for any kernel, may exist ONLY if it beats the intrinsic on a cold paired per-kernel
micro-comparison; it is an admission bar on future code, never a pass/fail condition here (a
clause that cannot fail is not this wave's gate — the checkasm-liveness harness is). Failure
semantics: any W1–W5 threshold not re-affirmed → that wave reopens under a redress; any parity
anchor fail → correctness halt across the tranche. Artefact: `checkasm` test result +
`gn2_remeasure_bench` PASS/REFUTE table + `GN-2-ASM-EARN.md`.

**Consumer IN this wave (Y.13).** The five wired kernels ARE the consumers under audit; the
harness reads each through its live call site. W6 lands no new substrate — it audits the
tranche's own.

**Oracles/benches.** The consolidated `checkasm` harness (all five kernels, ≥200k fuzz each) +
`gn2_remeasure_bench` cold paired PMU, ≥3 windows, both interleave orders (neutralizing the
interleave-slot self-deception PROVE-01/VERIFY and PROVE-03/VERIFY both flagged).

---

## 5. LOC ledger

| wave | +new | −deleted | net | generated budget |
|------|-----:|---------:|----:|------------------|
| W1 | 190 | 15 | +175 | ≤ +50 (introduces lowering) |
| W2 | 55 | 30 | +25 | ≤ +40 |
| W3 | 70 | 35 | +35 | ≤ +25 |
| W4 | 130 | 70 | +60 | 0 (hand-written kernel) |
| W5 | 180 | 45 | +135 | 0 (parse-that-regex hand-written) |
| W6 | 340 | 10 | +330 | 0 (test/bench/doc) |
| **total** | **965** | **205** | **+760** | — |

Benches and `tests/` files are outside the src budget (precept 16/54). The `generated_loc_
budget` is enforced per wave (precept 53); overflow blocks the wave until traced to an O(N)
generator regression.

## 6. Exit criteria

GN-2 closes when, on the greenfield tree at the tranche's tip:

1. Every W1–W5 born-RED gate is GREEN and re-affirmed by W6's non-author re-measure across ≥3
   fresh windows in both interleave orders.
2. No dormant kernel remains: `SimdMode::Prefilter` is lowered and consumed (W1); the scalar
   `match_tiny_plain_string` leaf is gone (W2); the CSS animate flag is deleted, tokenizer
   unconditionally ON (W3); the structural classify is wired into `scan_dispatch` (W4); the
   float conversion lane is redressed with no libstd hand-off on the common path (W5).
3. The checkasm-liveness harness runs in CI over all five kernels (≥200k fuzz each, NEON==scalar)
   and the asm-earn policy is recorded — intrinsics default, no asm admitted (W6).
4. Full regen is byte-for-byte idempotent (precept 53); `cargo test` green across
   `bbnf-simd`, `runtime`, `parse-that-regex`, `codegen`.
5. Doc rides code (precept 17): `GN-2-ASM-EARN.md` present; the §0 numbers this tranche moved
   (twitter parse_tape, tailwind lcss ratio, canada fused/sonic) are re-stamped in the tranche
   close note as measured, not projected.

## 7. Risks with named counterexamples

- **W1 self-loop detection is too narrow / too broad.** Counterexample: a grammar whose only
  self-loop is the JSON whitespace-skip has a complement alphabet of the 6 structurals + quote
  — cardinality ≤ 16, so it IS detected; but if the sole scan site is a `.*`-style
  any-char loop (complement empty) there is no terminator set and no prefilter applies — the
  derivation must emit NOTHING (fall to `Exact`), not a degenerate all-byte prefilter. Mitigation:
  the detection arm requires complement cardinality in `[1, 16]`; empty or >16 → no Prefilter
  recognizer. If NO grammar in the fleet has a qualifying self-loop, W1's consumer is a
  purpose-authored minimal skip grammar (a real `.bbnf`, not a synthetic byte buffer) — still a
  real grammar scan site, satisfying the gate honestly.
- **W3's short-span guard kills the dense win.** Counterexample: a threshold set too high
  (`CSS_SHUFTI_MIN_SPAN = 64`) would route tailwind's medium spans to scalar and erase the
  4.16× lcss win. Mitigation: the gate REQUIRES all three big corpora retain their GN-1 win;
  the threshold is tuned against that constraint, and the named const makes it a cost-model
  value the wave can sweep, not a buried literal.
- **W4's narrow finds the floor is memory-bound, not stitch-bound.** Counterexample: if the
  classify stays at 0.80 cyc/B after removing the stitch, the floor was DRAM/load-port bound and
  PROVE-04's stitch attribution was incomplete. Mitigation: this is a legitimate RED with a
  documented negative result (the floor is elsewhere) — NOT a silent pass; the wave reports the
  finding and W4 is retired-with-rationale (precept 7), not force-closed.
- **W5's tax is in the digest, not the conversion.** Counterexample: if W5.a shows canada floats
  already hit the EL fast path (the libstd hand-off is cold) and completing the slow path buys
  nothing, the +1.48 cyc/B is in the sink digest / struct-fill, not `materialize_f64`.
  Mitigation: W5.a's hit-rate measurement is the FIRST hop precisely to catch this before code
  is written; if the tax is downstream, W5.b pivots to deferred materialization for the tape
  lane and the fused-lane redress re-homes as a documented GN-4 dependency (named destination,
  not silent defer, precept 7).
- **W6 re-measure refutes a W1–W5 magnitude (as PROVE-05/VERIFY refuted bootstrap −20.4%→−14.6%
  and animate +9.8%→+13.4%).** Counterexample: W3 lands claiming animate 0.98× but the fresh
  re-measure shows 1.03×. Mitigation: this is the challenge wave WORKING — the wave reopens; the
  born-RED threshold is the contract, and a magnitude inside its CI re-affirms, outside it
  reopens. The challenge wave has teeth by construction (precept 39).
