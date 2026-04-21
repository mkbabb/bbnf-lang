# AY-II-AUDIT-D — Predecessor + Successor Alignment + Perf Trajectory

Audit Agent D of the AY-II pre-close triumvirate. Read-only at master
HEAD `b5bbda6c`. No benches run in this pass — perf claims reconstruct
from cited artefacts + infer against landed source changes, marked
"(inferred from: ...)" where bench numbers are not direct measurements.

## 1. Scope + methodology

How AY-II/W0's landed state composes forward (BA/BB/BC) and back
(AY-I, B0). Perf-truth without running benches — source inspection
+ prior AUDIT-D at `62de21c4` + W5/W6 captures (`post-AY-W{5,6}-
bench.txt`). Bench numbers explicitly marked "(inferred from: ...)"
where they are inferences, not measurements.

## 2. Q1 — AY-I FINAL.md debt reconciliation

Per `docs/tranches/AY-I/FINAL.md` §Hard gates — pass-I status table,
eleven rows route to AY-II/W0 or AY-II/W1. Status at HEAD `b5bbda6c`:

| AY-I Gate | Target wave | Landed status | Evidence |
|---|---|---|---|
| Canonical packed substrate + direct JSON write | AY-II/W0 | **PARTIAL** | W0.a retired `note_push` + `SIB_SKIP_STAMPED_BIT` + `open_stack` + `OpenFrame` (`a13840a0`); W0.a added `begin_compound`/`end_compound` + `Columns::rollback_to` (source: `crates/tape/src/builder.rs:324,364,401`). `push_compound` still present at `builder.rs:247` (legacy surface; used by `visitor.rs` + `dedup.rs`). Direct JSON write NOT active — shape emitters allocate `ValueBuilder` but don't thread it (see Q2). |
| `view()` / `to_value()` / `get()` unified | AY-II/W0 | **PARTIAL** | `Parsed::to_value()` at `crates/core/src/runtime/parsed.rs:348` routes to `R::project_value_output(&self.value_builder_output, self.input)`. `Parsed::get()` at line 376 still routes to `R::query(self.view(), path)` — tape cursor. Unified at interface level; semantically divergent (to_value reads empty slab, get reads tape). |
| Grammar-derived direct-to-struct + Pratt lowering | AY-II/W0 | **PARTIAL** | W0.d retired `__named_type_shim_*` + emits materializer per admission (`db979564`). Pratt reducers still on `push_compound` (`emitter/shapes/pratt.rs` unchanged from W6.c). |
| Every mined / emitted surface has a production consumer | AY-II/W0 | **PARTIAL** | `navigate_tape` retired from `runtime/path.rs` per W0.c (`4f42f6bb`). `STRUCTURAL_SCAN_POLICY` emitted by W0.e but NO consumer — per PROGRESS §W0.e concern 4. `ValueBuilder` allocated but not populated — per-shape threading deferred. Three consumerless surfaces. |
| twitter ≤ 1.15 × sonic | AY-II/W1 | **OPEN** | W1 planned; current post-W0 ratio is effectively unchanged (inferred from: Q2 analysis — lockstep not threaded). |
| canada ≤ 1.20 × sonic | AY-II/W1 | **OPEN** | Same. |
| citm ≤ 1.20 × sonic | AY-II/W1 | **OPEN** | Same. |
| 5-fixture geomean ≤ 1.20 × sonic | AY-II/W1 | **OPEN** | Same. |
| CSS / Sheets / BBNF preserve functional guarantees | AY-II/W0 | **LIKELY DISCHARGED** | All 15 retry-IIFE sites in emitter call `columns_mut().rollback_to(...)` (see Q3 grep). No raw `truncate` calls in the emitter — the class of panic at `columns.rs:409` is extinguished at source. Not validated by bench run. |
| Structural scan as first-class same-path | AY-II/W0 | **SUBSTRATE-ONLY** | W0.e promoted cursor API + `STRUCTURAL_SCAN_POLICY` const (`61d0338c`, `487b17b7`). No consumer in `__path_walk` emission — PROGRESS §W0.e concern 4 is explicit. |
| B0 closes, no parity-critical work parked | AY-I/W* | **PASS** | Unchanged from AY-I FINAL. |

**Net**: 1 PASS, 1 LIKELY DISCHARGED, 4 PARTIAL, 5 OPEN. AY-II/W0
lands substrate and interface-level unification; semantic activation
(fused value construction on JSON/CSS L4/Sheets parse paths) is the
uncovered residual.

## 3. Q2 — Performance trajectory

### Baseline (AY-I close, HEAD `62de21c4`)

From `audit/AUDIT-D-perf-truth.md`: `bbnf_value_twitter` = 538 MB/s /
5.942 cyc/byte / **3.995× sonic**. `bbnf_visitor_twitter` = 1,917
MB/s / 1.669 cyc/byte / **1.12× sonic** — geomean 0.99× across 5
fixtures; inside gate on 3 of 5. Parse-only twitter = 543 MB/s
(`post-AY-W6-bench.txt:7`). Gap is `to_value` reconstruction, not
parse-time cycles/byte.

### Post-W0 current state (HEAD `b5bbda6c`)

1. **W0.a** — `note_push` + `SIB_SKIP_STAMPED_BIT` + `open_stack` +
   `OpenFrame` deleted (`a13840a0`). Per-push stamp overhead gone.
2. **W0.a** — `Columns::rollback_to` landed; all 15 emitter retry
   sites use it (zero raw `columns_mut().truncate` calls).
3. **W0.b** — shape emitters use `begin_compound`/`end_compound`
   (6-arg per `f8ac2cd7` fix); `end_compound_post_order` for walker-
   parity triplets.
4. **W0.c** — `ValueBuilder<R>` allocated at parse entry
   (`generated.rs:32932-32934`, `emitter/grammar.rs:1101-1104`);
   `Parsed::to_value()` routes to `R::project_value_output(...)`
   (`parsed.rs:348-353`).
5. **Critical gap**: dispatcher at `grammar.rs:1107-1112` +
   `generated.rs:32937-32942` passes `&mut builder` only — not
   `&mut value_builder`. Per-shape emitters never call
   `value_builder.begin_compound` / `push_leaf` / `end_compound`
   (grep `generated.rs` → 0 matches). The slab is empty at
   `finish()`.
6. **Projector panics on empty slab**: `value_materialize.rs:281-285`
   and `generated.rs:25660-25664` — `panic!("AY-II.W0.c:
   Parsed::to_value() called on an empty value substrate; fused
   parse entry was not invoked")`. Every grammar's `to_value()` at
   HEAD PANICS.

### Current attribution (inferred)

`bbnf_value_twitter` cannot be benched post-W0 — `to_value()` panics.
Parse-only `bbnf_twitter` projection: W0.a removed the dominant W5
regression class (per-child `set_sib_skip_at` + `or_extra_at`); W1-fix
ceiling was 688 MB/s under that cost. Removing it recovers toward AU
1967 MB/s / 1.626 cyc/byte — projected **1,600-1,900 MB/s** at HEAD
(inferred from: AUDIT-D §1 AU→W6 regression signature + W0.a deletion).

**Fused-pipeline best-case** (IF W0.b threading completes — it has
NOT): converges to `bbnf_visitor_twitter` = 1,917 MB/s / 1.12× sonic
(AUDIT-D §2 measured). Geomean 0.99× — inside ≤ 1.20× gate by
construction. AY-I AUDIT-D §7's "CONDITIONALLY attainable in current
substrate" verdict carries forward; W0 did not discharge the condition.

### Samply attribution (inferred)

Post-W0 `bbnf_twitter` hot path: `<JsonParser>::parse` inlined body
returns toward W1-fix ~50% (reversing +17 pp object migration);
`parse_object_JsonParser_object` drops 41.21% → ~24%; finaliser rises
(restored sole stamp). `ValueBuilder::push_*` absent. `bbnf_value_*`
is not benchable — hot path is `std::panicking` unwind.

## 4. Q3 — CSS + Sheets fat-LTO panic transitive fix

AY-I AUDIT-D traced `columns.rs:409` panic at
`google_sheets_monolithic/parse_nested` + `css_l4/bootstrap` to W6.c
retry-IIFE truncations under an open frame, then `note_push` reading
the stale `last_child`.

Source verification at HEAD `b5bbda6c`:

- `note_push`, `SIB_SKIP_STAMPED_BIT`, `open_stack`, `OpenFrame` →
  **all deleted** (grep `crates/tape/src` → 0 matches each).
- Emitter retry sites: zero raw `columns_mut().truncate(...)`
  across `keyword.rs`, `wrap.rs`, `flat.rs`, `alt_dispatch.rs`,
  `array.rs`, `inline.rs`, `arglist.rs` — all 15 sites call
  `.rollback_to(...)`. (`inline.rs:368,705` mention the old API in
  doc comments only.)
- `Columns::rollback_to` truncates all parallel columns atomically
  (`columns.rs:206+`).

**Conclusion**: panic class is **transitively extinguished at
source**. The stamping site is deleted; every retry uses column-
coherent rollback. Bench confirmation would land at W0 close
ceremony, not performed in this read-only pass.

## 5. Q4 — B0 runway utilisation in AY-II

B0 (`7b223cf6`) shipped 10 `ay-*` Makefile targets, `profiling-prep`
profile, idempotent `scripts/prebuild-benches.sh` +
`prepare-profile-wave.sh`. AY-II/W0.md + W1.md reference them
extensively.

Utilisation at pre-close pause:
- W0 sub-agents used `cargo iter-check` + `make iter-test-*`
  (standard surface). W0.a/b/c/d/e sub-gates cite `ay-*` targets in
  specs.
- Orchestrator close ceremony (W0.md §"Orchestrator-owned close
  steps") **not executed** — per PROGRESS.md §Pre-close pause,
  `cargo check --workspace` did not complete; fat-LTO 5-bench
  matrix + samply never ran.
- `.profiles/samply/` absent at HEAD; no `post-AY-II-*` artefacts
  in `docs/benchmarks/`.

**Risk**: W1-W5 specs assume W0-close measurements. If W1 opens
without executing W0 close, W1's gates start from an unmeasured
baseline. Orchestrator-owned sequencing gap, not a runway defect.

## 6. Q5 — BA/BB/BC validity

### BA

BA.md §Thesis §1: *"BA starts from a correct substrate … visitor-lane
default `to_value()`, unified compound emission API, single-pass
finaliser stamping, first-class `Columns::rollback_to`."*

Landed at `b5bbda6c`: unified compound API **landed**; single-pass
finaliser **landed**; `rollback_to` **landed**; visitor-lane default
`to_value()` **NOT landed** (panics on empty slab).

BA.md invariant §7 is load-bearing: *"BA does not inherit unfinished
AY parity debt. If AY-II still needs a runtime change to hit its own
close gates, AY-II is not closed. Pass III or a new letter per SPEC
§Multi-pass tranche split opens before BA."* Applies as written.

BA.md does NOT explicitly state a `bbnf_value_twitter ≤ 1.15× sonic`
precondition at open; phrasing is "declared near-parity gates"
(§Invariants §7). AY-II.md §Defensible floor lists ≤ 1.15× at W1
close; BA inherits iff AY-II closes. W0's landing does not deliver.

BA/waves/W3.md still names `crates/jit/src/lib.rs` — crate does not
exist in workspace (AUDIT-A §3 flagged at AY-I; unchanged).

**Verdict**: BA thesis aligned to AY-II **declared** outcome; BA
invariant §7 forbids open on AY-II **landed** outcome. Split-decision
trigger per §9.

### BB

BB.md §Thesis §1: downstream of BA close. BB invariants §6 + ops §6:
runtime work excluded. No file-bound collision with AY-II scope.
**Verdict**: correctly gated; unaffected by W0 pause.

### BC

BC.md §Thesis §1: downstream of BA close. Chain AY-II → BA → BB → BC
is sequential; AY-II residual cascades. **Verdict**: structurally
downstream; no immediate action.

## 7. Q6 — Cross-tranche debt ledger

Single ledger aggregating every open debt across the tranche stack at
HEAD `b5bbda6c`:

| Debt | Source | Destination tranche | Artefact path |
|---|---|---|---|
| Fused-pipeline lockstep not threaded through shape emitters | AY-II/W0.b deferred | AY-II W1 or AY-III | `crates/core/src/backend/rust/emitter/shapes/*.rs` (value_builder arg absent) |
| `to_value()` panics on every grammar (empty slab) | AY-II/W0.b deferred | AY-II W1 or AY-III | `crates/core/src/backend/rust/emitter/shapes/value_materialize.rs:274-286` |
| `STRUCTURAL_SCAN_POLICY` emitted without `__path_walk` consumer | AY-II/W0.e deferred | AY-II W1 or AY-III | `PROGRESS.md:107-110` concern 4 |
| `projection_totality.rs` + `value_api_apples_to_apples.rs` untested against composed substrate | AY-II/W0.c+d deferred | AY-II W1 | `crates/core/tests/projection_totality.rs`, `value_api_apples_to_apples.rs` |
| Bootstrap double-regen idempotency unverified since W0-fix | AY-II/W0 close ceremony | AY-II W0 close or W1 | `scripts/bootstrap-bbnf.sh` + PROGRESS.md §Pre-close pause item 2 |
| `f372e7ef` hand-patched generated.rs in history | AY-II/W0 compose | AY-II W0 close (regen) | `git show f372e7ef:crates/core/src/grammar/generated.rs` (22,880-22,892 stub lines) |
| W0.c + W0.d test coverage has not run against composed+regen substrate | AY-II/W0 | AY-II W0 close or W1 | PROGRESS.md §Pre-close pause item 5 |
| Pratt reducer inner compounds still on `push_compound` | AY-I/W6.c | AY-II (not landed in W0) or BA | `crates/core/src/backend/rust/emitter/shapes/pratt.rs` |
| `push_compound` still a public API on `TapeBuilder` | AY-II/W0.a scope | AY-II close | `crates/tape/src/builder.rs:247` + `visitor.rs:365,389,515` + `dedup.rs:354` usages |
| Typed CSS semantic parity (lightningcss typed surfaces) not measured | AY-II/W0.d + W2 | AY-II W2 | `crates/core/tests/lightningcss_parity.rs` (pre-existing) + `typed_accessor_surface.rs` |
| parse-that SaturationCache retirement stashed | B0 environmental | future regex-analysis wave (BA?) | `/Users/mkbabb/Programming/parse-that` stash |
| CSS + Sheets fat-LTO bench not re-run post-W0.a retire | AY-I AUDIT-D §5 | AY-II W0 close ceremony | `post-AY-W5/W6-bench.txt` (JSON only); no CSS/Sheets artefact |
| AY-I.W8 outstanding twitter ≤ 1.15× + canada/citm ≤ 1.20× gates | AY-I FINAL §Hard gates | AY-II W1 | `docs/benchmarks/post-AY-II-W1-eager.json` (planned, not written) |
| BA W3 references non-existent `crates/jit/src/lib.rs` | BA.md pre-AY-II | BA.W3 replan | `docs/tranches/BA/waves/W3.md:22` |
| W0.e structural-scan policy has zero samply evidence | AY-II/W0.e | AY-II W0 close ceremony | `.profiles/samply/AY-II-W0/` (absent) |

**Aggregate count**: 15 debts. 7 route to AY-II internally (W0 close
or W1); 4 route to AY-II close; 2 remain BA-era; 1 is a B0 environmental
stash; 1 is a pre-AY-II BA file-bound error.

## 8. Q7 — `f372e7ef` history assessment

Line counts: `git show f372e7ef:crates/core/src/grammar/generated.rs`
= 30,430 lines; master HEAD `b5bbda6c` = 33,293 lines.

At `f372e7ef`, lines 22,880-22,892 carry a hand-patched stub for
BbnfBootstrap's `project_value_output` body =
`unreachable!("AY-II.W0 bootstrap stub: ...")`. Commit message
explicitly names it the "README self-host circular-dependency escape
recipe" one-shot.

At master HEAD `b5bbda6c`: line 24711 routes
`project_value_output` to `project_value_BbnfBootstrap`; line 25656
is a real grammar-emitted projector (2,863 lines added via W0.d
commits `db979564` + `58271da1` regen). Line 25660-25664 carries
the empty-slab panic guard (not `unreachable!`). Grep for
`unreachable!(` returns zero stub matches in generated.rs at HEAD.

**Verdict**: stub existed only at `f372e7ef`; post-regen commits
overwrote. History carries one transient hand-patched commit;
master HEAD is clean regen.

Not a fatal precedent — the README §Self-host escape recipe names
the pattern. PROGRESS.md §Pre-close pause item 1 still flags it
because the commit persists on history even though content is
dead. Corrective: interactive-rebase squash `f372e7ef` into
`db979564` before AY-II close. Discipline point; no runtime effect.

## 9. Top findings informing forward path

**Finding 1 — W0's landed state violates its own invariants §§1, 5.**
AY-II.md §1 asserts `Parsed::to_value()` has no parse invocation and
no tape-reconstruction fallback. Satisfied in letter (grep clean);
violated in spirit — the fused pipeline writes nothing, so
`to_value()` panics instead of projecting. §5 asserts fused parse
builds tape + value in one walk; only the tape is built.

**Finding 2 — `columns.rs:409` panic class extinguished at source.**
Every reader (`note_push` + stamp path) deleted; every retry uses
column-coherent `rollback_to`. CSS + Sheets fat-LTO benches that
panicked at AY-I close will compile clean on current substrate.
Bench confirmation pending (W0 close ceremony). Unqualified W0 win.

**Finding 3 — Gate is one deferred item away.** `bbnf_visitor_twitter`
already sits at 0.99× sonic geomean / 1.12× on twitter (AY-I AUDIT-D
measured). The fused-pipeline convergence to visitor-lane cost
requires only per-shape `value_builder` threading through the
dispatcher signature. `ValueBuilder` API, allocation, and projector
are complete; only the emission calls in each shape are missing.
Mechanical, not architectural.

### Split decision

Per SPEC §Multi-pass tranche split: AY-II requires at minimum one
more wave.

(a) **Absorb** — W1's first sub-phase lands per-shape threading,
then proceeds with JSON gate proof. Requires W0 close ceremony
first (regen + 5-bench + samply). Scope = planned W1 + 1 sub-phase.

(b) **New letter (AY-III)** — if per-shape threading reveals
architectural work (leaf payload decoding requires new payload-
column routing, etc.), close AY-II on current + open AY-III.

**Recommendation**: path (a). Evidence (`ValueBuilder` API complete,
allocation+finish wired, projector body complete, panic correctly
diagnosed) shows the residual is emission plumbing, not design.
SPEC's split threshold is "architectural work reveals a new
substrate"; not applicable. BA invariant §7 carries the real guard —
BA cannot open until the fused path delivers without panic.

