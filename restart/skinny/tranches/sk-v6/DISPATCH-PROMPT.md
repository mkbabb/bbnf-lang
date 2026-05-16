# SK-V6 Implementation Agent Prompt

Date: 2026-05-14.

This is your single source of dispatch authority. Read it fully before
acting. SK-V6 supersedes SK-V5 dispatch. You inherit the SK-V5 substrate
landings (BackendShape Rust state, generated SinkOnly, vendored
Eisel-Lemire, Canada scan floor) and the SK-V5 empirical refutations
(REDRESS entries 50-55). You do NOT inherit the SK-V5 Wave 3 UTF-8
fusion prescription — it was empirically refuted across five attempts.

## 1. What Happened (read this first)

The SK-V5 implementation pass ran 18 commits over 8 hours (3f22707c →
03fe6988). It produced:

**Wins**:
- Generated SinkOnly emission from BIR (`codegen/src/json_sink_direct.rs`)
  — Track 1 stopped being the bench-private parser; the gate is now honest.
- Eisel-Lemire vendored into `parse-that-regex/src/number/`. numbers
  direct row closed: 33% → 100.4% sonic-rs (PASS).
- Canada SIMD structural scan floor: 22,136 → 41,833 Mbps. Closed via
  consumed NEON classifier + `bulk_emit_positions_64` primitive.
- `BackendShape` enum + `derive_backend_shape` + `LayoutFacts.backend_shape`
  Rust state landed.
- simd-scan fossil crate deleted; eventcursor feature + cfg branches
  purged; `generated_eventcursor.rs` removed.
- Strictness columns disclosed in RESULTS.md
  (`Strictness | parse_utf8 | escape_complete | flaw_probe`).
- `parse-attribution` feature flag wired through 7 kernel boundaries.

**Regressions** (the part you must fix):

Parse outcome-G rows expanded from 4 (SK-V4) to **13** (SK-V5). Nine
rows that were A/PASS are now G/NO-GO. The largest regressions:

| Row | SK-V4 Mbps | SK-V5 Mbps | Δ |
|---|---:|---:|---:|
| twitter | 16294 | 12303 | −24% |
| citm_catalog | 29185 | 20775 | −29% (was A/PASS) |
| apache_builds | 17734 | 12341 | −30% (was A/PASS) |
| github_events | 25332 | 13161 | −48% (was A/PASS) |
| update_center | 18204 | 9430 | −48% (was C/PASS) |
| gsoc-2018 | 47481 | 21907 | −54% (was A/PASS) |
| instruments | 19946 | 11887 | −40% (was C/PASS) |
| distinct_values | 16241 | 6097 | −62% (was C/PASS) |
| y_string_unicode | 13109 | 6084 | −54% (was C/PASS) |

Two improvements on string-heavy rows: unicode_mixed +18%, unicode_basic +66%.

**Why the regression**: two distinct causes you must NOT conflate.

1. **Track 1 is now generated runtime, not bench-private SinkParser.**
   The SK-V4 measurements were optimistic — the bench was hand-tuned.
   The honest Track 1 has emission overhead the bench parser was
   hiding. This is a permanent baseline shift; reverting Wave 2 to
   re-hide the gap is non-canonical. The SOTA-beat target rebaselines
   here.

2. **SK-V5 Wave 3 UTF-8 fusion was empirically refuted.** Five shapes
   of the prescribed kernel landed below the existing baseline:
   - REDRESS 50: parse-time retained projection side tables
   - REDRESS 51: byte-class whitespace cursor (Lock 1 violation)
   - REDRESS 53: parser-local structural-mask cursor (twitter
     regressed to 6156 Mbps; reverted pre-commit)
   - REDRESS 54: exact decoded-string stats sink (escape-heavy rows
     regressed sharply)
   - REDRESS 55: quote-source fused streaming materializer (same
     regression class as 54)

   The SK-V5 GRAND-SYNTHESIS §2 named "fold UTF-8 validation INTO
   the NEON 16-byte body scan" as the single parse-G close. The
   prescription was wrong. The actual close shape is unknown — you
   will rediscover it by profile, not by hypothesis transfer.

## 2. Non-Negotiables

| Rule | Enforcement |
|---|---|
| No new BBNF directives | `rg -n "@(simd\|runtime\|backend\|shape\|asm\|sink\|direct)" grammars restart/skinny` shows no new directive surface. |
| No new BIR variant | Use existing `Alt { Dispatch }`, `TapeEmit`, `DirectBuild`, `CallHost`. |
| No parallel substrate | Mask streams transient; retained APIs seal `OffsetTape` / `EventTape`; direct-only APIs use `SinkOnly`. |
| No JSON code in generic crates | `bbnf-simd` / `parse-that-regex` / `codegen/lower` are grammar-neutral. JSON specifics live in `runtime/grammars/json/`, codegen-emitted `.data` tables, and the grammar definition file. |
| Scalar reference per primitive | Every SIMD/ASM primitive has scalar executable spec + checkasm parity BEFORE wiring. |
| Same-wave consumer | A primitive lands only with the generated/runtime consumer that exercises it on the hot path. |
| **Profile-first prescription** | No kernel intervention is proposed without a fresh PC-level profile of the NEW Track 1 (generated runtime) baseline naming the kernel boundary it targets. **Hypothesis transfer from SK-V5 is forbidden.** |
| Profiles cite c/B + corpus | Every SOTA claim cites profile path, c/B (or Mbps), and affected corpus rows on the new baseline. |
| Strict-vs-strict comparisons | Sidecar rows match the same strictness plane. Permissive-asmjson "beats" do not count. |
| Triumvirate discipline | Each wave separates research → plan → redress. Commit between roles. No single commit merges roles. |
| Hard cap per dispatch | Every research / profile / implementation dispatch carries an explicit minute cap. At 0.9× cap commit; at cap halt. |
| Same-row falsification gate | A kernel that does not lift a previously-named row on the new baseline is rejected; record in REDRESS with measurements and the next candidate shape. |
| No deferrals | Wave N closes on measurement, not "future phase will fix it." |

## 3. Pre-Blocked Routes (DO NOT RE-OPEN)

These have measurement evidence in REDRESS.md. They are non-canonical
unless a fresh before/after row at the new baseline overturns the
measurement. If you find yourself proposing one of these, stop and
write a different plan.

**From SK-V5 (entries 46-56)**:
- Parse-time retained projection side tables (50).
- Byte-class whitespace cursor (51) — Lock 1 violation.
- Parser-local structural-mask cursor (53).
- Exact decoded-string stats sink (54).
- Quote-source fused streaming string materializer (55).
- "Fold UTF-8 validation INTO NEON 16-byte body scan" as a single-kernel
  parse-G close — covered by all four attempts above.

**From SK-V4 and earlier**:
- Class A `match_tiny_plain_string` NEON wiring (REDRESS 28 + 33) —
  parity-green but wrong layer; previously wired and regressed twitter
  ~25%; reverted. The kernel stays in tree as grammar-generic primitive
  for future 8-byte-narrow-scan grammars; it is NOT a parse-G fix.
- 12-byte token width churn.
- Pair-token fusion.
- PSI / DTA Rust-codegen automaton (V9.5 PSI excavation).
- StructuralIndex sidecar parser prepass.
- EventCursor as parallel prepass mask producer (already deleted in W4).
- Function-pointer dispatch table.
- Capacity prescan one-shot / sampled.
- Generic SWAR whitespace skipper (different from the corpus-specific
  whitespace removal that did win).
- Separator elision.
- Raw `f64` shortcut (`raw.parse::<f64>()`).
- Eager retained tape as SOTA-beat substrate.
- Cost model as aspirational.
- asmjson DPDA as M5 Max close target (it's permissive + AVX-512 only).
- Primitive admission without same-wave consumer.

## 4. Legacy Documents to NUKE (Wave 0 deliverable)

The SK-V3 / SK-V4 / V1-V2 audit detritus is now superseded by the SK-V5
+ SK-V6 line. Delete the following files in Wave 0:

| File | Reason |
|---|---|
| `restart/skinny/tranches/IMPLEMENTATION-PACKET-SK-V3-SOTA-BEAT.md` | Superseded by SK-V5 packet + this SK-V6 prompt. |
| `restart/skinny/tranches/IMPLEMENTATION-PACKET-SK-V4-ASMJSON-BEAT.md` | Superseded; the Wave 3 UTF-8 prescription was refuted. |
| `restart/skinny/tranches/GRAND-SYNTHESIS-SOTA-BEAT-SK-V3.md` | Superseded. |
| `restart/skinny/tranches/WAVE-1-2-COHORT-DIGEST.md` | Superseded by SK-V5-COHORT/. |
| `restart/skinny/tranches/ASMJSON-DAV1D-GRAND-SYNTHESIS-SK-V4.md` | Superseded by GRAND-SYNTHESIS-SK-V5.md + this prompt's §1 verdict. |
| `restart/skinny/tranches/IMPLEMENTATION-PACKET-V2.md` | Pre-SK-V3 packet; superseded. |
| `restart/skinny/tranches/IMPLEMENTATION-PACKET-SOTA-BEAT.md` | Pre-SK-V3 packet; superseded. |
| `restart/skinny/tranches/LAZY-TAPE-DESIGN.md` | Superseded by tape ≡ projection union (Lock 1 canonical). |
| `restart/skinny/tranches/HARDENING-BENCH-SK-V1.md` | Pre-SK-V2 draft. |
| `restart/skinny/tranches/HARDENING-COMPILER-SK-V1.md` | Pre-SK-V2 draft. |
| `restart/skinny/tranches/HARDENING-CONSOLIDATED-SK-V1.md` | Pre-SK-V2 draft. |
| `restart/skinny/tranches/HARDENING-CONSOLIDATED-SK-V1-pre-redress.md` | Pre-redress draft. |
| `restart/skinny/tranches/HARDENING-INDEX-SK-V1.md` | Pre-SK-V2 draft. |
| `restart/skinny/tranches/HARDENING-SUBSTRATE-SK-V1.md` | Pre-SK-V2 draft. |
| `restart/skinny/tranches/HARDENING-WORKSPACE-SK-V1.md` | Pre-SK-V2 draft. |
| `restart/skinny/tranches/HARDENING-BENCH-SK-V2.md` | Superseded by skinny/BENCH.md fold-back. |
| `restart/skinny/tranches/HARDENING-COMPILER-SK-V2.md` | Superseded by skinny/COMPILER.md fold-back. |
| `restart/skinny/tranches/HARDENING-CONSOLIDATED-SK-V2.md` | Superseded by GRAND-SYNTHESIS-SK-V5.md. |
| `restart/skinny/tranches/HARDENING-INDEX-SK-V2.md` | Superseded by skinny/INDEX.md fold-back. |
| `restart/skinny/tranches/HARDENING-SUBSTRATE-SK-V2.md` | Superseded by SK-V5 Wave 4 split (canonical now in code). |
| `restart/skinny/tranches/HARDENING-WORKSPACE-SK-V2.md` | Superseded by skinny/WORKSPACE.md fold-back. |
| `skinny/profile/direct-sink-2026-05-12/` | Pre-SK-V5 profile; stale. |
| `skinny/profile/reprofile-sk-v3-wave1/` | Pre-SK-V5 profile; stale. |
| `skinny/profile/asm-string-unicode/` | Pre-SK-V5 string-unicode profile; stale (B3 confirmed these directories' artefacts are superseded by SK-V5-COHORT B-reports). |

Reading order after the nuke:
1. This prompt (SK-V6).
2. `restart/skinny/tranches/sk-v5/SYNTHESIS.md` with the 2026-05-14 post-assay header (the corrected baseline state).
3. `restart/skinny/tranches/sk-v5/research/` — cohort authority (15 reports, 5,559 LOC).
4. `restart/skinny/tranches/sk-v5/HANDOFF.md` — packet handoff (still applies to substrate intent; supplemented by this prompt's regression-recovery framing).
5. `restart/skinny/tranches/sk-v5/NUKE-PLAN.md` — the items not-yet-executed.
6. `restart/skinny/tranches/sk-v5/SPEC.md` — packet (Wave 1, 2, 4 are partially or fully landed; Wave 3 prescription refuted; Wave 5+ pending).

The remaining canonical docs are then: `restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`, `restart/HANDOFF.md`, `restart/locks/*.md`, `restart/MIGRATION.md`, the four GRAND-SYNTHESIS-SK-V5 + IMPLEMENTATION-PACKET-SK-V5 + NUKE-PLAN-SK-V5 + HANDOFF-SK-V5 audit docs, the SK-V5-COHORT directory, this SK-V6 prompt, and the skinny corpus (BENCH/COMPILER/INDEX/SUBSTRATE/WORKSPACE/HARDENING.md + RESULTS.md + REDRESS.md).

## 5. Wave Structure

### Wave 0 — Legacy purge + spec fold-back of SK-V5 outcomes

**Scope**: nuke the 23 legacy files in §4. Update the master spec docs
to record the SK-V5 outcomes honestly.

**Files to edit**:
- `restart/skinny/tranches/sk-v5/SYNTHESIS.md` — verify the
  2026-05-14 post-assay header at lines 9-19 is present and accurate;
  no further edits.
- `restart/skinny/tranches/sk-v5/SPEC.md` — annotate Wave
  3 as REFUTED (point to REDRESS 50-55); annotate Wave 1+2+4+5 as
  LANDED with their commit SHAs; keep Wave 6+7 as future scope.
- `restart/skinny/tranches/sk-v5/HANDOFF.md` — top-line "current state"
  reflects the latest SK-V6 accounting: 13 retained G rows, four retained A
  rows, four direct digest PASS rows, representative `real_typed_struct` PASS
  rows for `twitter` and `update_center`, and Canada scan floor restored.
- `restart/HANDOFF.md` (top-level) — same correction.
- `restart/ARCHITECTURE.md` §7.4 — update SK-V5 implementation status:
  `BackendShape` enum LANDED, `derive_backend_shape` LANDED,
  `LayoutFacts.backend_shape` LANDED, `codegen/src/lower/` LANDED (with
  reference to actual on-disk hierarchy).
- `restart/MASTER-PLAN.md` §13 — annotate H.W1+H.W2+H.W4+H.W5 as
  LANDED; H.W3 UTF-8 fusion as REFUTED (cite REDRESS 50-55);
  H.W4.LOCK14 partially LANDED (simd-scan + eventcursor purged; bbnf-simd
  god-module split still pending); H.W6 strict matrix as the SK-V6
  target.
- `restart/skinny/INDEX.md` — add SK-V6 prompt anchor; mark superseded
  docs as such; redirect to GRAND-SYNTHESIS-SK-V5 + SK-V5-COHORT for
  authority.
- `restart/skinny/BENCH.md` — confirm strictness columns spec lands; no
  new spec.
- `restart/skinny/COMPILER.md` — confirm parse-attribution feature flag
  spec lands; no new spec.
- `skinny/REDRESS.md` — add entry 57 (this SK-V6 prompt's regression-
  recovery framing); entry 58 (Wave 3 UTF-8 fusion class refuted with
  all five sub-routes cited).

**Hard cap**: 90 minutes for fold-back + delete pass.

**Exit gate**:
- `cargo build --workspace` green.
- `cargo test --workspace` green.
- No legacy files in §4 remain on disk.
- `rg -l "IMPLEMENTATION-PACKET-SK-V3\|HARDENING-.*-SK-V1\|LAZY-TAPE-DESIGN"
  restart/` returns zero hits except in commit messages (history).
- `git status` shows only the planned spec edits + deletions; no
  cargo-touched files outside owner paths.

**Commit message convention**: `chore(sk-v6-wave0): nuke legacy audit
docs + record SK-V5 LANDED/REFUTED status across master specs`.

### Wave 1 — Re-profile the NEW Track 1 baseline (research)

**You are forbidden from prescribing any kernel intervention until
this wave closes.**

The SK-V5 Wave 3 prescription failed because the diagnosis was made
against the SK-V4 baseline (bench-private SinkParser). The Track 1 hot
path is now generated runtime (`runtime::generated_json::parse` +
`runtime::generated_json::parse_direct`). The actual hot leaves on
this new baseline are unknown.

**Scope**: PC-level samply profiles of the new Track 1 across all 17
corpora. For each row, name the dominant kernel boundary + classify the
pathology + propose at most one candidate kernel intervention.

**Dispatch 6 parallel research agents** (per the SK-V5 cohort pattern,
not 12; you already have 15 reports of context; this is targeted
re-profile only):

- **R1 — parse-only PC attribution, 9 regressed-from-PASS rows**:
  citm_catalog, apache_builds, github_events, update_center, gsoc-2018,
  instruments, distinct_values, y_string_unicode, marine_ik.
  Build with `--features runtime/parse-attribution` so the 7 named
  kernel boundaries do not fuse. Per row: name the dominant
  `parse_value_at` callee at the symbol level. Compare to SK-V5-COHORT
  B1 attribution to determine what changed.
- **R2 — parse-only PC attribution, 4 original parse-G rows**:
  twitter, random, unicode_mixed, unicode_basic. Same methodology. If
  the hot leaf is no longer `validate_utf8_codepoint`, the SK-V5
  diagnosis was correct but the prescription was wrong; if the hot
  leaf is something else, the diagnosis itself was incomplete.
- **R3 — direct PC attribution, 13 N-direct rows**: same methodology,
  `profile_direct ... track1`. Compare to SK-V5-COHORT B2 attribution.
  Particular attention to whether generated SinkOnly emission overhead
  shows up as a named symbol (e.g. `emit_string`, `emit_number`,
  `emit_object_field`) at >10% self-time.
- **R4 — diff against the SK-V4 baseline**: pick 3 rows where
  regression is largest (gsoc-2018 −54%, distinct_values −62%,
  y_string_unicode −54%). Profile each at SK-V5 HEAD. Compare leaf-by-
  leaf to the SK-V4 attribution (skinny/profile/reassay-skv4-2026-05-13/).
  Name the specific overhead that appeared.
- **R5 — sidecar comparator refresh on the same machine, same
  strictness plane**: run yyjson / simdjson C++ / sonic-rs / asmjson
  SWAR through the same 17-corpus matrix. Record Mbps + hot leaf per
  row. The strict-vs-strict comparison is the SOTA-beat target; the
  permissive comparisons stay as flaw probes only.
- **R6 — own-binary i-cache + branch-mispredict PMU**: across the 9
  regressed rows, sample L1i miss rate, branch mispredict rate, and
  IPC. Per Lock 15, parse_value_at hot function should fit in 20 KiB
  i-cache. Verify on new baseline.

Each agent writes one report at `/tmp/skv6-R<n>-<topic>.md` and
returns the path. Hard cap: 30 minutes each. Dispatch all six in
parallel via background agents per the
`bg-then-monitor` discipline.

**Exit gate**:
- 6 reports archived to `restart/skinny/tranches/sk-v6/research/`.
- A synthesis at `restart/skinny/tranches/sk-v6/SYNTHESIS-WAVE-1-PLAN.md` that
  names ONE diagnosis-revision conclusion per major regression cluster.
- The synthesis lists at most THREE candidate kernel interventions for
  Wave 2 with: file path, expected row impact, falsifiability gate.
- The synthesis explicitly states which SK-V5 diagnoses are
  re-confirmed vs invalidated.

### Wave 2 — Ship ONE intervention, measure, decide

**Single intervention per Wave 2 dispatch.** No batching kernels.

**Scope**: implement the highest-impact candidate from Wave 1's
shortlist. Land with scalar reference + checkasm parity + same-wave
consumer.

**Falsifiability gate**: name the specific corpus rows the intervention
must lift, and the Mbps delta required. Examples:
- "gsoc-2018 must cross 50% sonic-rs after this change" (currently 44.9%).
- "twitter parse must recover at least 50% of the 16294 → 12303
  regression" (so ≥ 14300 Mbps).
- "citm_catalog must return to A/PASS" (≥ 100% sonic = 25413 Mbps).

If the falsifiability gate fails: revert. Record the rejected route in
REDRESS with measurements. Return to Wave 1 with the empirical
evidence to pick the next candidate.

**Hard cap**: 60 minutes for implementation; 15 minutes for measurement.

**Exit gate**:
- The intervention's named rows close (or improve enough to argue the
  next intervention compounds).
- No other row regresses by >5%.
- `cargo run -p xtask --release -- gate-json` reports the new state in
  RESULTS.md.
- The intervention is recorded in REDRESS with the measurement table.

Repeat Wave 2 until the parse-G count is back to ≤ 4 rows OR Wave 1's
shortlist is exhausted (in which case Wave 1 redispatches with a new
profiling angle).

### Wave 3 — Direct-NoGo string/Unicode close

Same shape as Wave 2 but targeting the 12 remaining direct N-direct
rows (numbers + 4 currently passing PLUS the rest). Specifically the
string-heavy rows: unicode_mixed (41.7%), unicode_escapes (36.5%),
distinct_values (47.0%), y_string_unicode (56.4%), gsoc-2018 (62.0%).

Pre-block: the five rejected SK-V5 direct-string routes (REDRESS
54-55). The next plan MUST beat the default allocate-then-contiguous-
hash baseline (entry 55's closing prose). Field-layout materializer or
same-loop SinkOnly that produces typed field representation directly
is the admissible class.

Same falsifiability discipline as Wave 2.

### Wave 4 — Strict workload matrix (SOTA-beat)

**Only dispatch when parse-G ≤ 4 AND direct N-direct ≤ 4.**

Full 17-corpus × 7-workload × N-sidecar matrix with strictness disclosed.
SOTA-beat means strict-vs-strict against sonic-rs / simdjson / yyjson
on M5 Max. asmjson is permissive on M5 Max SWAR and is flaw-probe only.

Exit gate per SK-V5 HANDOFF-SK-V5 §exit: zero parse-G, zero N-direct,
Track 1 generated runtime confirmed, Track 2 structurally different,
parse_value_at fuse-collapse explained at PC level.

### Wave 5 — bbnf-simd JSON god-module split + remaining 8 bbnf.asm primitive bodies

Only dispatch when Wave 4 closes. SK-V5 Wave 4 partially landed —
simd-scan fossil deleted + eventcursor purged — but `bbnf-simd/src/lib.rs`
716-LOC god-module split was NOT done. Per NUKE-PLAN §7. Plus the 8
remaining `bbnf.asm` primitive bodies (BYTE_CLASS_FROM_TABLE_64,
BITMAP_PREFIX_XOR_64, BITMAP_NEXT_SET_BIT, BULK_EMIT_COMPRESSED,
EOB_PAD_CLAMP, FSM_DISPATCH_THREADED, FRAME_PUSH_BOUNDED,
FRAME_POP_BOUNDED). Note `bulk_emit_positions_64` from REDRESS 56 is a
LANDED variant of BULK_EMIT primitive family — confirm whether it
already covers BULK_EMIT_COMPRESSED's contract.

This wave is durability work on top of a passing gate; not a SOTA-beat
prerequisite.

### Wave 6 — x86 CollapsedStage successor (optional)

Per SK-V5 IMPLEMENTATION-PACKET §9. Conditional on Zen 4 silicon access
+ NASM author + checkasm-green Layer 1. Not required for SK-V6 close.

## 6. Dispatch Posture

**Triumvirate discipline**: each wave separates RESEARCH (profile +
diagnose) → PLAN (synthesis + falsifiability gate) → REDRESS
(implement + measure + commit). Each role commits before the next
begins. Never merge roles in a single commit.

**Auto-trigger triumvirate**: if a dispatch JSONL goes quiet for >15
min OR a first-pass attempt produces no commit, dispatch a 3-agent
triumvirate without prompting the user.

**Status ticks**: emit one-line status tick every ~5 min of orchestrator-
silent wait. Never make the user ask status twice. Reconcile TaskList vs
ps + JSONL mtimes before every user-facing status reply; zombies are
frequent in this campaign per prior pattern.

**Hard cap discipline**: every dispatch carries "HARD CAP: N min. At
0.9N commit, at N halt." Defaults: research 30, plan 20, redress 60.

**Profile-first commitment**: kernel prescriptions without a fresh
attribution on the new Track 1 baseline are non-canonical. The SK-V5
Wave 3 failure proved this. Wave 1 (re-profile) is mandatory before
any Wave 2 (intervention) dispatch.

**Same-wave consumer**: every primitive lands with its hot-path caller
in the same commit. No orphan kernels — REDRESS records 30+ admission-
without-consumer rejections; the rule is a hard discipline now.

**Build / test mechanics**: use `--profile ax-iter` for iteration-loop
cargo invocations. At most one cargo invocation in flight per
`CARGO_TARGET_DIR` at any instant; per-agent CARGO_TARGET_DIR=
`/tmp/skv6-cargo/<agent-id>` to avoid lock contention.

**Long-running commands**: any bash invocation expected to take >60s
sets `run_in_background=true` and is followed by a `Monitor` call. Do
not poll via `sleep` loops or `ps aux`.

**Large-file reads**: `wc -l` before `Read` on files >2K LOC; use
grep+offset for `generated.rs`, transcripts, large audits.

**Empty sub-agent returns**: not a scope-reveal; redispatch verbatim
with prior-worktree pointer per the prior pattern memory.

## 7. Exit Condition (SK-V6 close)

The campaign closes when ALL of these hold simultaneously:

1. `skinny/RESULTS.md` parse matrix has zero outcome-G rows OR each
   remaining G row carries a falsifiability-tested rejection in
   REDRESS naming why no admissible kernel closes it.
2. `skinny/RESULTS.md` direct matrix has zero N-direct rows OR each
   remaining N-direct row carries the same falsifiability-tested
   rejection.
3. Track 1 calls generated runtime on every direct row (verified by
   `samply` symbol path = `runtime::generated_json::parse_direct`).
4. Track 2 is structurally different from Track 1 (different symbol
   path entirely).
5. Strictness column on every row is honestly disclosed (not all
   `deferred`; the parse-time UTF-8 either lands strict OR the
   `deferred` is justified per row with a flaw_probe entry).
6. Sidecar comparator table on the same strictness plane: sonic-rs
   typed direct, simdjson C++ DOM + On Demand, yyjson inlined DOM,
   asmjson SWAR (permissive; flaw probe only), serde_json. Each row
   carries API + output plane + Mbps + hot leaf.
7. `cargo run -p xtask --release -- primitive-checkasm` passes
   including register-clobber detection + rdtsc + stack-canary
   XOR-fold (dav1d-style hardening).
8. Lock 1 (substrate union) audit clean: no parallel substrate
   residue.
9. Lock 14 (zero overfitting) audit clean: no JSON code in generic
   crates. The bbnf-simd god-module is split (Wave 5 closure).
10. Lock 15 (i-cache budget) holds: hot function <20 KiB measured.
11. SOTA-beat declared per row: bbnf Track 1 strict-vs-strict beats
    sonic-rs / simdjson / yyjson on M5 Max with 1.10× slack.
12. SK-V6 final report at
    `restart/skinny/tranches/sk-v6/HANDOFF.md` matches the SK-V5
    HANDOFF format + adds: per-wave commit chain + per-wave Mbps
    delta on each row + the empirically-refuted SK-V5 Wave 3 routes
    in REDRESS canonical form.

## 8. Reading-Order Reminder

Before each new wave, re-read:

- This prompt (SK-V6).
- `restart/skinny/tranches/sk-v5/SYNTHESIS.md` (post-assay header
  + the unchanged diagnostic body).
- The latest REDRESS.md (every rejected route in the prior tranche).
- The latest RESULTS.md (the current gate state — single authority).
- The relevant SK-V5-COHORT report for the wave's domain.

Per the `read-size-preflight` memory: `wc -l` before any `Read` on
files >2K LOC.

## 9. Anti-Pattern Reminders

The following are specific traps the SK-V5 pass fell into. Do not
repeat:

- **Hypothesis transfer**: SK-V5 Wave 3 prescribed UTF-8 fusion based
  on SK-V4 baseline profiling. The new baseline disagrees. Always
  re-profile the actual hot path before prescribing.
- **Same-symbol fuse-collapse**: parse_value_at at 99.7% self-time is
  not a signal. Use the `parse-attribution` feature flag to break
  attribution apart before prescribing.
- **Primitive parity ≠ row impact**: a kernel that passes checkasm
  parity but doesn't move a named corpus row is rejected. Parity is
  necessary, not sufficient.
- **Substrate-without-consumer**: 30+ REDRESS entries record this
  failure mode. Same-wave consumer is non-negotiable.
- **Renamed-sidecar Lock 1 violation**: the "byte-class whitespace
  cursor" was a renamed mask producer. If a new component reads the
  source bytes a second time outside the canonical recursive descent
  path, it is a parallel substrate and is rejected.
- **Track 1 ≡ Track 2 dishonest gate**: Track 2 must be structurally
  different code from Track 1. Both calling the same private parser
  is rejected (REDRESS 34).
- **Bench-private parser as Track 1**: the SK-V4 baseline was
  optimistic. Generated runtime is the only admissible Track 1.

## 10. Final Posture

The architecture is correct. The substrate is correct. The 16 locks
govern. The five-shape `BackendShape` taxonomy is correct AND now
exists in Rust. Generated SinkOnly works. Eisel-Lemire closed numbers.
Canada scan floor closed.

The work that remains is THROUGHPUT RECOVERY on the new honest
baseline. The shape of that work is unknown until Wave 1 re-profiles.
No further kernel prescriptions until then.

Dispatch Wave 0.
