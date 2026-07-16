# PT-1 — THE REGEX ENGINE SOTA UPLIFT (parse-that)

The regex home earns its title. `bbnf-regex` (`parse-that/rust/regex`, the one regex home
under the union architecture) is lifted from a competent HIR→NFA→DFA compiler to a
prefilter-tiered, lazily-determinized, cross-repo-consumed match engine whose acceleration is
measured, not asserted. This is a FULL IMPLEMENTATION tranche (SPEC §2b supersession, owner
order 2026-07-16): it executes in the parse-that repo, by the owner or a parse-that-side
session, after the W0 handshake (letter BBNF-PT-G + owner acknowledgment + rebase over active
WIP + consumer baselines). bbnf-side agents never write into ~/Programming/parse-that — the
handshake, not a fence, is how full implementation and the non-clobber mechanics coexist. Its
one seam into bbnf-lang (W5) lands in the greenfield generator (bbnf-side source), interlocked
with GN-2.

---

## §1 CHARTER

### Mission

Productionize the parse-that regex engine to SOTA for the grammar-generator's use: a start-state
memchr prefilter (certified 8.8–29.5× match throughput), a shufti/Teddy multi-byte tier for
entry sets wider than `memchr3`, a lazy/hybrid DFA with a state-cap bail for compile-time on
large patterns, the pre-existing regex-bridge compile break repaired, the bbnf consumption seam
(the generator emits `dfa.find` for unanchored regex-leaf scans), and the parse-that x86 purge
with the profiling standing law ported. Every acceleration lands with its consumer and its cold
paired bench in the same wave.

### Supersessions (encoded here, binding)

- PT-1 executes SPEC §2b **PT-N1** under the owner's 2026-07-16 supersession: the PT series is
  elevated from REVIEW-NOT-EXECUTE to **FULL IMPLEMENTATION** ("fully-upgrade parse-that (and
  the regex engine thereof)... a full implementation, not just a skinny pass"). The PROVE-10
  245-line certified patch is the *seed*; this tranche executes its productionization in the
  parse-that repo. The SPEC §2b clause "generator inherits by emitting `dfa.find`" is the
  substrate for W5 — the greenfield generator inheritance, bbnf-side source, landable
  in-campaign.
- **Coordination mechanics (P23 lineage, re-scoped by the supersession).** Execution runs in
  the parse-that repo by the owner or a parse-that-side session, ONLY after the W0 handshake:
  letter **BBNF-PT-G-2026-07-16** (superseding the never-delivered BBNF-PT-F) delivered with
  the PROVE-10 `patch.diff` + `VERIFY.md` evidence, owner acknowledgment, a rebase point over
  any active WIP in the working tree (value.js/keyframes sessions carry untracked state), and
  recorded consumer baselines. bbnf-side agents never write into `~/Programming/parse-that`;
  pre-handshake work happens on scratch copies (R3 §8 recipe). The consumer suites are the
  close gates. The one cross-repo exception is W5's generator seam, which is bbnf-lang source.
- SPEC §3 checkbox 2 (parse-that `release.yml` aarch64-only) is an owner gate authored SOLELY by
  **PT-2.W5** (The Guard Rails, PT-N3 = de-nightly, per the SPEC's "when PT-N3 lands"); the owner
  ratifies (default proceeds, mirrors the x86 abrogation), and it rides the letter — not an executed
  in-tree wave. W6 CONSUMES it — the x86 purge satisfies the gate; it does not re-author the layer.

### Precept bindings (sk-v23 research/07, by number)

- **2** (no workarounds/stubs/disabled gates), **6** (no legacy — delete, never flag-and-keep):
  W4 restores the canonical re-export, W6 deletes the x86 siblings outright.
- **8 / 37 / 40** (Y.13 — substrate lands with its consumer in the same wave): every wave names
  its in-wave consumer below; a prefilter without a caller does not open.
- **14 / 15** (no god modules; directory-modules over flat siblings): W2 breaks the growing flat
  `prefilter.rs` into an `automata/prefilter/` directory-module owning its `memchr`/`shufti`/
  `teddy` kernels; W3 lands `automata/lazy/` as a directory-module.
- **16 / 54** (tests in `tests/`, never inline): every wave adds a `tests/` oracle; the PROVE-10
  in-bench `assert_eq!` stays as a bench-time gate but is promoted to a proper `tests/` subtest
  (curing the named bench-only-wiring residue).
- **19** (a hard gate is valid only with a build/test/bench/generated-diff artefact; grep-only is
  supplementary): every gate below carries an exact command and a numeric threshold.
- **43–49** (the profiling standing law): cold per-parse, N≥200, paired same-binary interleaving,
  divan median ratio certified, absolutes directional; PMU cyc/B via `proc_pid_rusage` recorded
  where the macOS host permits (userspace PMU is restricted — wall-clock paired ratios are the
  certified metric, per the PROVE-10 disclosure).
- **50–52** (engineering laws; one cargo per target dir; long runs backgrounded + Monitor),
  **22** (agents never commit; orchestrator integrates by cherry-pick).

### REFUTATION GUARDS (SPEC §0 laws that constrain this tranche)

- **No digit-scan SIMD (02, NO_GO).** W6 edits `number_simd.rs` but only PURGES the x86 sibling;
  it neither adds nor resurrects a Mula/Lemire digit-scan lever. The aarch64 NEON digit path is
  preserved verbatim. The canada float redress (Eisel–Lemire) is GN-2.W5's concern, not PT-1's.
- **No block-scheduling pursuit (04, refuted).** No wave pursues multi-block software pipelining;
  the prefilter/lazy work is orthogonal to the movemask-stitch floor.
- **No shim perf claims (09, refuted).** `bbnf-regex` is pure scalar + `SmallVec` (R3 §3); it has
  no `portable_simd`. W1's speed comes from the `memchr` crate's own aarch64 NEON backend; W2's
  shufti uses real `vqtbl1q_u8` intrinsics. No wave claims performance from a `portable_simd`→NEON
  shim; every number is measured paired, never asserted from a shim asymmetry.
- **Fold-over-cursor dead; tape-navigation is not the get lane.** The engine is DFA-based; no wave
  relitigates the fold cursor or routes a get through tape navigation.
- **Warm benches are disinformation.** Every PT-1 bench is cold per-parse, paired, N≥200. W7 hunts
  warm/cached contamination as a first-class attack.

---

## §2 INTERLOCKS

| Direction | Id | Contract |
|---|---|---|
| depends-on | **W0 handshake** → all | No PT-1 code lands before the letter is placed, owner-acknowledged, and rebased over the active value.js/keyframes WIP in the parse-that working tree (non-clobber law, HANDOFF §6.9). |
| feeds | **PT-1.W1 → GN-1** | GN-1 vendors `parse-that-regex` into the greenfield (SPEC §1, §GN-1). W1 must land the prefilter in `parse-that/rust/regex` BEFORE GN-1's vendor step (or GN-1 re-vendors post-W1) so the greenfield inherits `Dfa::find` born-accelerated. |
| depends-on | **GN-1 → PT-1.W5** | W5 lowers to `dfa.find` in the greenfield generator, which consumes the vendored crate; it cannot open until GN-1's port-in exists. |
| interlock | **PT-1.W5 ⇄ GN-2.W1** | Both land codegen prefilter-lowering. GN-2.W1 lowers self-loop scan sites to `find_member` (bbnf-simd shufti, SimdMode::Prefilter); PT-1.W5 lowers unanchored regex-leaf scans to `dfa.find` (bbnf-regex prefilter). The recognizer dispatch decides which lowering fires per site — they must NOT double-emit. |
| ordering advisory | **W4 alongside W1** | W4's numbering reflects dependency depth, not execution order. Landing it with W1 restores the `parse_that --test regex` DFA subtest oracle that W1–W3 correctness leans on (PROVE-10 §3 substituted an in-bench assert for the uncompilable subtests). |

**Owner gates:** the letter delivery + placement (SPEC §3 checkbox 1; placement law W0); the
parse-that `release.yml` aarch64-only cut (SPEC §3 checkbox 2) is authored by **PT-2.W5** (The
Guard Rails, PT-N3 = de-nightly) and consumed — not re-authored — by PT-1.W6's purge. Defaults
proceed absent veto.

---

## §3 WAVE MANIFEST

| Id | Name | Scope | LOC (+new/−del) | Gate | Cap |
|---|---|---|---|---|---|
| W0 | The Handshake | letter placed, owner ack, rebase over WIP, baselines recorded | +80 / −0 | born-verified | 20m |
| W1 | The Prefilter Tier | land PROVE-10 (Prefilter + `Dfa::find`), productionized | +320 / −0 | born-verified (non-regression) | 30m |
| W2 | The Wide-Entry Tier | shufti/Teddy prefilter for entry sets > `memchr3` | +180 / −97 | born-RED | 45m |
| W3 | The Lazy DFA | hybrid DFA + state-cap bail (the unchosen adjudication half) | +450 / −0 | born-RED | 45m |
| W4 | The Bridge Repair | fix pre-existing `parse_that::regex::parser` E0432 | +10 / −2 | born-RED | 20m |
| W5 | The Consumption Seam | generator emits `dfa.find` for unanchored regex-leaf scans | +120 / −40 | born-RED | 30m |
| W6 | The x86 Purge | delete the two x86 scan files' x86 paths; port the bench law | +10 / −150 | born-verified + structural | 30m |
| W7 | The Adversary | re-measure/audit every W1–W6 claim on a fresh scratch copy | +60 / −0 | born-RED (can send waves back) | 45m |

All parse-that benches run from `parse-that/rust/` (nightly auto via `rust-toolchain.toml` pin
`nightly-2026-04-11`, R3 §1). All parse-that work is against a scratch copy per the R3 §8 recipe
(rsync working tree, repoint the 7 external patch-deps, isolated `CARGO_TARGET_DIR`) — the
original tree is never mutated (non-clobber law).

---

## §4 PER-WAVE SPEC

### W0 — The Handshake (coordination is a wave, not a fence)

**Objective.** Establish the sanctioned execution footing: the coordination letter placed, owner
acknowledgment recorded, the parse-that working tree rebased so PT-1's landings do not clobber
the active value.js/keyframes WIP, and the consumer baselines captured against which every
downstream gate regresses.

**File-level plan.**
- The letter **`BBNF-PT-G-2026-07-16`** — placed ONLY by the owner or a parse-that-side session
  (placement law). It carries: the PT-1 execution schedule (W1–W6), the PROVE-10 `patch.diff` +
  `VERIFY.md` evidence, the PT-R.5 repoint re-timed to GN-0/GN-1, the non-clobber acknowledgment,
  and the rebase point. A bbnf-side agent NEVER writes into `~/Programming/parse-that`; it prepares
  the letter body as an artifact for the owner/parse-that session to place.
- A coordination note under `docs/tranches/sk-v24/PT-1/` records the baseline table.

**Baselines recorded (the anchor for W1–W6 non-regression):**
- `cargo test -p bbnf-regex` = 44/44 on the rebased tree (PROVE-10, VERIFY §3).
- `cargo bench -p bbnf-regex --bench regex_dfa_compile` pristine scan medians per pattern.
- Consumer close-gate suites (SPEC §2b; HANDOFF §5): `value.js`, `latex-paper` (^0.7.1 SKEW — flag
  the 0.9→1.0 jump), `keyframes.js` — recorded green on the rebased tree.

**Consumer in this wave.** W1's landing consumes the rebased tree + the baseline table; the
handshake's product is the footing W1 stands on.

**Gate (born-verified).** The letter is present at the sanctioned path by the sanctioned author
AND owner ack is recorded AND the working-tree WIP is inventoried (`git -C ~/Programming/parse-that
status` + stash list) and a rebase point named AND `cargo test -p bbnf-regex` = 44/44 on the
rebased tree. **Failure semantics:** if uncommitted value.js/keyframes WIP would be clobbered by
the landing sequence, or bbnf-regex is not 44/44 on the rebased tree, the handshake HALTS and
re-coordinates — W1 does not open. (This gate can fail: a red baseline or an unrebasable conflict
blocks the tranche.)

---

### W1 — The Prefilter Tier (land the certified patch, productionized)

**Objective.** Land PROVE-10's certified 245-line additive patch (`Prefilter` + `Dfa::find`,
8.8–29.5× on prefilterable patterns, controls 0.97–1.01×, compile-neutral, 44/44), then
productionize it beyond bench grade.

**File-level plan.** Apply `prototypes/PROVE-10/patch.diff` to `parse-that/rust/regex`:
- `src/automata/prefilter.rs` (**new, +97**) — `Prefilter { None, Byte1, Byte2, Byte3 }` +
  `from_dfa` (reads the start-state transition row, O(256)) + `next_candidate`
  (`memchr`/`memchr2`/`memchr3`). Sibling of `dfa.rs`/`accel.rs`/`nfa.rs` under `automata/`
  (the `accel.rs` precedent — colocation, precept 15).
- `src/automata/dfa.rs` (**+46**) — `prefilter` field on `Dfa`; Step-6 derivation in `from_nfa`
  after flattening (dfa.rs:101); `Dfa::find` (unanchored leftmost, `#[inline]`, prefilter branch
  hoisted out of the loop so the inactive path is byte-identical to a bare `find_at` scan).
- `src/automata/mod.rs` (**+1**), `src/lib.rs` (**+1** — `pub use automata::prefilter::Prefilter`).
- `Cargo.toml` (**+4** — `memchr = "2.5"`, aarch64 NEON backend).
- `benches/regex_dfa_compile.rs` (**+96** — the skip-heavy corpus, paired `scan_scalar`/
  `scan_prefilter` lanes, the `count_scalar == count_prefilter` correctness assert).

**Adaptation notes.** The patch was certified against a scratch copy at parse-that HEAD `ef10d5b`
(R3 §1); the `automata/` module is stable, so the hunks apply clean (`git apply --check -p1`
REPELLED attack H in VERIFY §1). Rebase over the W0 point if HEAD advanced.

**Productionization (curing the bench-only-wiring residue, precept 16/54).** Add
`tests/dfa_find_prefilter_test.rs` (**+~60**) — a proper `tests/` oracle asserting `Dfa::find`
returns byte-identical match sets vs the byte-by-byte `find_at` scan across the four prefilterable
patterns AND the two `None` controls; the in-bench `assert_eq!` remains as the bench-time gate.
Confirm the rustdoc on `Prefilter`/`Dfa::find` (present in the patch) documents the public API.

**Consumer in this wave.** The `scan_prefilter` bench lane calls `Dfa::find`; the new `tests/`
oracle exercises it on prose. Both live in the wave.

**Oracle & bench (cold paired, PMU where allowed).**
`cargo bench -p bbnf-regex --bench regex_dfa_compile` — divan `BytesCount`, N=200, paired
same-binary lanes; `cargo test -p bbnf-regex --test dfa_find_prefilter_test`.

**Gate (born-verified, non-regression).** The bench reproduces **≥8×** on `0x[0-9a-fA-F]+`,
`#[0-9a-fA-F]{3,6}`, `https?://…`, `(?:foo|bar)[a-z]*` AND **0.97–1.01×** on `[a-z]+`/`\w+`
controls (paired, cold, N≥200) AND `cargo test -p bbnf-regex` = 44/44 + the new oracle green AND
the compile axis is neutral (mod/pristine `dfa_compile` within noise). **Failure semantics:** any
control below **0.95×** (a real regression), any prefilterable below **8×**, a red test, or the
correctness assert firing → RED.

---

### W2 — The Wide-Entry Tier (shufti/Teddy beyond memchr3)

**Objective.** Entry sets of 4+ distinct start bytes fall to `Prefilter::None` today (parity) —
the acknowledged scope cap (PROVE-10 §5). Add the SIMD multi-byte tier so wide entry sets skip
too, with zero regression on the memchr tier or the controls.

**File-level plan (colocation — precept 14/15).** `prefilter.rs` now owns >1 kernel, so it becomes
a directory-module:
- `src/automata/prefilter/mod.rs` — the `Prefilter` enum (grows `Shufti(ByteSet)` and, where the
  DFA exposes a multi-byte literal prefix, `Teddy(...)`), `from_dfa` dispatch, `next_candidate`.
- `src/automata/prefilter/memchr.rs` — the Byte1/2/3 memchr tier, MOVED from the flat file (DRY).
- `src/automata/prefilter/shufti.rs` (**new**) — nibble-LUT byte-set membership via `vqtbl1q_u8`
  (the shufti idiom, R7 §3): for a 4..N-byte entry set, build the low/high-nibble LUT once at DFA
  build, scan 16-byte lanes for the next byte whose class is in the set, then anchored `find_at`
  confirm. Reuses the accel `ByteSet` vocabulary (`sets/byteset.rs`, R3 §3).
- `src/automata/prefilter/teddy.rs` (**new**) — the bucketed multi-literal tier (R7 §3, Teddy
  ICPP21), fed by `info/literal_prefix.rs` (R3 §3) when the DFA carries multi-byte literal
  prefixes; candidate front-end + anchored confirm, order-preserving for leftmost.
- `from_dfa` cost model (pluggable, not a hardcoded branch): entry set ≤3 → memchr; 4..N single
  bytes → shufti; multi-byte literal prefixes present → Teddy; else `None`.

**Seams.** `sets/byteset.rs:251` (`ByteSet`), `info/literal_prefix.rs:85`
(`RegexInfo` literal extraction), R7 §3 (shufti = local `classify_chunk_nibble` idiom, two
`vqtbl1q_u8` + AND). Extends the bench corpus in `benches/regex_dfa_compile.rs` with
`SCAN_PATTERNS_WIDE` (e.g. `[aeiou][a-z]+`, `[bcdfg]awn`, `(alpha|gamma|delta|kappa)…` for the
literal tier).

**Consumer in this wave.** A `scan_wide` bench lane calls `Dfa::find` on wide-entry patterns +
`tests/dfa_find_wide_test.rs` asserts match-set parity vs the scalar scan.

**Gate (born-RED).** Opens RED — the tier does not exist, so wide patterns sit at ~1.0× parity.
Closes when `cargo bench -p bbnf-regex --bench regex_dfa_compile` shows **≥2×** on the wide-entry
corpus AND the four W1 prefilterable patterns still fire the memchr tier at their W1 ratios
(**no tier regression**) AND `[a-z]+`/`\w+` controls hold at **0.97–1.03×** AND
`cargo test -p bbnf-regex` green including the new wide oracle. **Failure semantics:** the shufti/
Teddy tier changing the match set (correctness assert fires) → HARD RED; wide throughput < 2×, a
memchr-tier pattern regressing, or a control below 0.95× → RED.

---

### W3 — The Lazy DFA (the unchosen adjudication half, now specced)

**Objective.** PROVE-10 chose the prefilter over lazy determinization for its one-win parsimony
(REPORT §1). The lazy/hybrid DFA is the complementary axis — compile-time on large patterns whose
eager subset construction blows up. Land it with a state-cap bail so a pathological pattern falls
back rather than exhausting memory (R7 §4: "never a wrong answer").

**File-level plan (directory-module — precept 15).** `src/automata/lazy/`:
- `mod.rs` — `LazyDfa` (transition cache + on-demand determinization), the search loop, the
  cost-model entry that picks lazy when the estimated eager DFA exceeds a size threshold
  (`info/dfa_size.rs:38` is the estimator — a pluggable cost input, not a hardcoded branch).
- `state_cache.rs` — `LazyStateId` with reserved tag bits (unknown/dead/quit/start/match, the
  regex-automata encoding, R7 §4) + a bounded cache with a state cap.
- `builder.rs` — on-demand subset construction that REUSES the eager `subset_construction` /
  byte-class machinery from `dfa.rs` (DRY — precept 1/5), stepping one input byte's states at a
  time rather than the full powerset up front.
- The **state-cap bail**: on cache exhaustion or state-count > cap, return a `Bail`; the caller
  falls back to the eager `Dfa` (or NFA) for that pattern — a fallback, never a wrong answer.

**Consumer in this wave.** The compile path routes large patterns through `LazyDfa`; a
`benches/regex_lazy_compile.rs` (or an extension of `regex_dfa_compile`) measures lazy-vs-eager
compile on a large-pattern corpus (bounded-repetition blowups, large unicode classes);
`tests/lazy_dfa_test.rs` asserts (a) `LazyDfa` match results == eager `Dfa` on the standard corpus
and (b) a cap-triggering pattern bails and the fallback yields the eager answer.

**Gate (born-RED).** Opens RED — no lazy engine exists. Closes when the bench shows **≥5×**
compile-time reduction (lazy vs eager) on the large-pattern corpus AND match throughput on those
patterns is within **0.98–1.02×** of eager (**zero match-throughput regression**) AND the bail is
exercised: a pathological pattern hits the cap and the fallback result equals the eager result
(`tests/` oracle). **Failure semantics:** < 5× compile win → RED; any match-throughput regression
> 2% → RED; a bail returning a non-eager answer → HARD RED.

---

### W4 — The Bridge Repair (the pre-existing E0432)

**Objective.** Repair the verified pre-existing compile break that blocks the entire
`parse_that --test regex` bridge (PROVE-10 §3; VERIFY §4), restoring the DFA/parser subtest oracle
that W1–W3 correctness properly leans on.

**The break (verified both ways, VERIFY §4).**
```
error[E0432]: unresolved import `parse_that::regex::parser`
 --> parse_that/tests/regex/parser_test.rs:2:24
2 | use parse_that::regex::parser::{parse, parse_with};
```
Reproduces identically with and without the PROVE-10 patch (bbnf-regex-only, 0 parse_that lines) —
a parse_that-side re-export drift.

**Root cause (located).** `parse-that/rust/parse_that/src/regex/mod.rs` re-exports the bbnf-regex
surface sibling-by-sibling — `bbnf_regex::automata::{accel,dfa,nfa}` (mod.rs:15-17),
`::classify`, `::first`, `::hir`, `::sets`, `::unicode`, `::utf8` (mod.rs:18-26) — but NOT
`parser`. bbnf-regex exposes the parser at `bbnf_regex::hir::parser` (R3 §3, `hir/parser/mod.rs:153`
`pub fn parse` / `:158` `parse_with`), re-exported at its crate root (`regex/src/lib.rs:27`).

**File-level plan (no legacy, no workaround — precept 2/6).** Add ONE canonical re-export to
`parse_that/rust/parse_that/src/regex/mod.rs` matching the sibling style (lines 15-26):
`pub use bbnf_regex::hir::parser;`. This restores `parse_that::regex::parser::{parse, parse_with}`
as a genuine re-export of the canonical bbnf-regex parser — not an alias, not a shim.

**Consumer in this wave.** The `parse_that --test regex` bridge itself: `parser_test.rs`
(22 subtests) + the co-housed `dfa`(49)/`byteset`(17)/`nfa`(13)/`utf8`(9)/`accel`(4)/`equiv`(6)/
`hir`(4) subtests (R3 §7) compile and run — and thereby validate W1's `Dfa::find`, W2's tiers, and
W3's `LazyDfa` against the real DFA subtests rather than a bench stand-in.

**Gate (born-RED).** Opens RED (E0432). Closes when `cargo test -p parse_that --test regex`
COMPILES and PASSES the full bridge (22 parser + the dfa/byteset/nfa/utf8/accel/equiv/hir
subtests). **Failure semantics:** E0432 persists, or any bridge subtest fails → RED.

---

### W5 — The Consumption Seam (the generator inherits the prefilter)

**Objective.** The bbnf-lang generator emits `dfa.find` for unanchored regex-leaf scans, inheriting
the W1 prefilter (and W2/W3 tiers) for free. This is the SPEC §2b PT-N1 "generator inherits by
emitting `dfa.find`" clause, now a landed wave. It lands in the greenfield bbnf-lang, consuming the
GN-1-vendored `bbnf-regex`.

**File-level plan.** In the greenfield codegen (the port of skinny's `lower/` + emit spine,
SPEC §GN-1): the regex-leaf lowering pass. Where a grammar rule contains an unanchored regex leaf
(a `RegexClass` scan that is not start-anchored), the emitter emits `dfa.find(bytes, pos)` instead
of a hand-rolled `find_at`-across-every-offset loop. Seams: the recognizer/`RegexClass` lowering
(`classify.rs:55` `RegexClass`, R3 §3; R7 local reuse map — `crates/core/src/lower/`), the emit
spine that residualizes (SPEC §GN-1, PROVE-12). The `dfa` is the compiled `bbnf-regex` `Dfa`
already available to the generated parser (the one regex home is consumed, not duplicated).

**Interlock (named).** PT-1.W5 ⇄ GN-2.W1. GN-2.W1 lowers self-loop scan sites to `find_member`
(bbnf-simd shufti, `SimdMode::Prefilter`); PT-1.W5 lowers unanchored regex-leaf scans to `dfa.find`
(bbnf-regex prefilter). The recognizer dispatch selects one lowering per site — no double-emit.

**Consumer in this wave.** A real grammar with an unanchored regex leaf (e.g. a token grammar
scanning for a hex-color `#[0-9a-fA-F]{3,6}` or a URL) generates a parser whose emitted `.rs` calls
`dfa.find`; a cold paired bench measures that scan site vs the pre-seam `find_at` loop.

**Oracle & bench.** Inspect the generated output (`cargo expand` / read the generated `.rs` —
precept: inspect generated output) to confirm `dfa.find` emission and zero residual `find_at`
loops at the regex-leaf site; regen bit-for-bit idempotent (precept 53); a cold paired bench on
the greenfield harness (paired mode, N≥200).

**Gate (born-RED).** Opens RED — the generator emits `find_at`. Closes when the generated parser
for a regex-leaf grammar emits `dfa.find` (grep the generated file: `dfa.find` present, the
hand-rolled loop absent) AND a cold paired bench shows the prefilterable regex-leaf scan site
beating the pre-seam `find_at` baseline by **≥5×** AND regen is bit-for-bit idempotent AND the
generated-LOC budget holds (precept: `generated_loc_budget`). **Failure semantics:** the generator
still emits `find_at` (grep hit) → RED; no measured win on the prefilterable leaf → RED; regen not
idempotent → RED.

---

### W6 — The x86 Purge + the bench law ported

**Objective.** parse-that carries exactly two files with real x86 intrinsic call-sites
(R3 §4; `bbnf-regex` has none, R3 §3). Purge them under the x86 abrogation (SPEC §4/§5, 4-layer),
and port the profiling standing law to the parse-that benches.

**File-level plan (delete, don't flag — precept 6).**
- `parse-that/rust/parse_that/src/parsers/scan/quote_parity.rs` (210 LOC, R3 §4) — DELETE the x86
  pclmulqdq path (`:42-49`, `#[cfg(all(target_arch="x86_64", target_feature="pclmulqdq"))]`:
  `_mm_set_epi64x`/`_mm_clmulepi64_si128`/`_mm_cvtsi128_si64`). KEEP the aarch64 `vmull_p64` path
  (`:51-56`) and the scalar fallback (`:58-68`).
- `parse-that/rust/parse_that/src/parsers/scan/number_simd.rs` (267 LOC, R3 §4) — DELETE the x86
  SSE4.2 impl (`:145-235`) and the runtime `is_x86_feature_detected!("sse4.2")` dispatch (`:222`).
  KEEP the aarch64 NEON impl (`:29-136`) and the scalar fallback (`scan_digits_simd`, `:239`).
  **REFUTATION GUARD:** this is a delete-the-x86-sibling purge, not a digit-scan SIMD addition —
  law 02 (NO_GO) is respected; the NEON digit path is unchanged.
- The x86 abrogation *layers* for parse-that — the structural token-gate CI step +
  `tests/no_x86.rs`, the workspace lints, and `release.yml` aarch64-only (owner §3.2) — are
  authored SOLELY by **PT-2.W5** (The Guard Rails, PT-N3 = de-nightly; it rides the letter as an
  owner gate the owner ratifies, not an executed in-tree wave). W6 does not re-cut them; its own
  gate greps `scan/` for zero x86 tokens to prove THIS purge landed, consuming PT-2.W5's
  enforcement layer.
- The bench law ported: ensure `benches/regex_dfa_compile.rs` (and the parse_that scan benches)
  run cold per-parse, N≥200, paired same-binary lanes (precepts 43-49); PMU cyc/B via
  `proc_pid_rusage` recorded where the macOS host permits (userspace-restricted — wall-clock paired
  ratios certified, absolutes directional). No warm bench survives.

**Consumer in this wave.** The structural token gate (CI) + the aarch64 build + the scan test
suites: `quote_parity_test.rs` (7), `number_simd_test.rs` (12), `number_fastpath_test.rs` (21),
R3 §7 — they stay green with the x86 siblings gone.

**Gate (born-verified + structural).**
`rg 'target_arch\s*=\s*"x86' parse-that/rust/parse_that/src/parsers/scan/` returns **ZERO**
(structural gate) AND `cargo build -p parse_that` green on aarch64 AND
`cargo test -p parse_that --test quote_parity_test --test number_simd_test --test
number_fastpath_test` green AND the paired divan discipline is wired (a bench asserting cold N≥200
paired lanes). **Failure semantics:** any x86 token remaining in `scan/` → RED; the aarch64 build
failing, or any scan test regressing → RED. (The structural grep is paired with a build+test
artefact, per precept 19 — not grep-only.)

---

## §5 CHALLENGE WAVE

### W7 — The Adversary (re-measure and audit this tranche's own claims)

**Objective.** Independently reconstruct the world and re-measure every W1–W6 claim on a FRESH
parse-that scratch copy (R3 §8 recipe, baseline built pristine FIRST, patch applied via
`git apply -p1`, interleaved) — the PROVE-10 verification discipline turned on PT-1 itself. The
challenge wave asserts nothing on open; it closes only when every claim reproduces, else it files a
defect that sends the offending wave back RED.

**The re-measure list (numeric envelopes to reproduce).**
- **W1:** 8.8–29.5× on the four prefilterable patterns, 0.97–1.01× controls, compile-neutral,
  44/44 tests, the `count_scalar == count_prefilter` assert green in ≥2 independent windows.
- **W2:** ≥2× on the wide-entry corpus, memchr-tier patterns un-regressed, controls flat, the
  wide correctness assert green.
- **W3:** ≥5× lazy-vs-eager compile on large patterns, match throughput 0.98–1.02× of eager, the
  state-cap bail exercised and correct.
- **W4:** `parse_that --test regex` compiles and passes (22 parser + dfa/byteset/nfa/utf8 subtests).
- **W5:** the generated parser emits `dfa.find` (inspect generated output — no `find_at` residue),
  regen idempotent, the ≥5× leaf win real and paired (not warm).
- **W6:** zero x86 tokens in `scan/`, scan tests green, the bench law genuinely cold-paired.

**Attack list (the failure-mode checklist, HANDOFF §3 CRITIQUE; VERIFY §1 lineage).** Opt-profile
asymmetry between arms; sabotaged baseline lane; warm/cached contamination; cherry-picked window;
hidden x86 / name-gates / stubs / dormant kernels; parity gap (a prefilter/shufti/Teddy/lazy tier
silently changing the match set); patch-vs-report mismatch; **prototype residue** (bench-only wiring
left un-promoted to `tests/`, feature-flag limbo, hand-schema crutches) — the framing's named
defect class, hunted explicitly.

**Consumer in this wave.** The re-measurement harness + a defect ledger under
`docs/tranches/sk-v24/PT-1/` recording each claim's fresh-window reproduction (or its refutation).

**Gate (born-RED — can send waves back).** Opens RED. Closes GREEN only when every W1–W6 claim
reproduces within its stated envelope on the fresh scratch copy under cold paired benches. **Failure
semantics:** any claim that fails to reproduce → the offending wave re-opens RED with the defect
ledgered; the challenge wave CANNOT close green over an unreproduced claim. (The gate can fail: an
unreproduced ratio is a defect, not a footnote.)

---

## §6 LOC LEDGER

| Wave | +new | −del | Net |
|---|---:|---:|---:|
| W0 | 80 | 0 | +80 |
| W1 | 320 | 0 | +320 |
| W2 | 180 | 97 | +83 |
| W3 | 450 | 0 | +450 |
| W4 | 10 | 2 | +8 |
| W5 | 120 | 40 | +80 |
| W6 | 10 | 150 | −140 |
| W7 | 60 | 0 | +60 |
| **Total** | **1230** | **289** | **+941** |

Directional. W3 (the new lazy engine) dominates the budget; W6 is the tranche's only net-negative
wave (the x86 purge). Generated LOC (W5) is governed by the greenfield `generated_loc_budget`,
separate from this hand-authored ledger.

---

## §7 EXIT CRITERIA

- W0 handshake complete: letter placed by sanctioned author, owner-acknowledged, tree rebased,
  baselines recorded, bbnf-regex 44/44 on the rebased tree.
- W1 born-verified: PROVE-10 ratios reproduced, promoted to a `tests/` oracle, 44/44 green.
- W2 GREEN: shufti/Teddy wide-entry tier ≥2×, no tier or control regression, match-set parity.
- W3 GREEN: lazy DFA ≥5× compile on large patterns, zero match-throughput regression, bail correct.
- W4 GREEN: the regex bridge compiles and passes (E0432 cured).
- W5 GREEN: generator emits `dfa.find`, ≥5× on the prefilterable leaf, regen idempotent, GN-2.W1
  interlock honored (no double-emit).
- W6 GREEN: zero x86 in `scan/`, aarch64 build + scan tests green, bench law cold-paired.
- W7 GREEN: every claim reproduced on a fresh scratch copy; the defect ledger is empty.

---

## §8 RISKS (with named counterexamples)

- **The shufti wide tier is slower than memchr's own multi-byte handling.** `memchr` internally
  vectorizes; a naive shufti loop could lose to `memchr2`/`memchr3` on 4-byte sets that are nearly
  in-tier. *Counterexample the challenge must build:* a 4-byte entry set where `memchr3`-of-3 +
  one-byte-fallback beats the shufti LUT. Mitigation: the W2 cost model keeps the memchr tier for
  ≤3 and gates shufti on a measured crossover, not a fixed width.
- **The lazy DFA's cache thrash erases the compile win.** A pattern whose match walk revisits many
  distinct states repeatedly pays determinization cost per sample, not once. *Counterexample:* a
  large unicode-class pattern over a high-entropy corpus where lazy compile is ≥5× but per-parse
  throughput regresses >2%. Mitigation: W3's gate binds BOTH axes — the compile win AND
  0.98–1.02× throughput; a throughput regression fails the wave.
- **The state-cap bail returns a wrong answer under a race between cap and match.** A pattern that
  reaches an accept state on the same step it exceeds the cap. *Counterexample:* a bounded-repetition
  pattern matching exactly at the cap boundary. Mitigation: the bail returns `Bail` (never a
  partial match); the W3 oracle asserts fallback-result == eager-result on the cap pattern.
- **W5 double-emits with GN-2.W1.** A self-loop scan site that is also an unanchored regex leaf gets
  both `find_member` and `dfa.find`. *Counterexample:* a grammar rule that is a starred regex class
  at a self-loop. Mitigation: the recognizer dispatch is the single decision point; the W5 gate
  greps the generated file for exactly one lowering per site.
- **The x86 purge breaks a cross-arch consumer.** A sibling repo builds parse_that for x86.
  *Counterexample:* none known — the x86 abrogation is normative both repos (SPEC §4). Mitigation:
  W6's structural grep (proving the purge) + PT-2.W5's aarch64 CI assertion is the enforcement;
  x86 is out of scope by law.
- **Warm-bench self-deception (the standing pathology, 09's lesson).** A two-arm window asymmetry
  inflates a ratio. *Counterexample:* the PROVE-09 `portable_simd`→NEON shim that did not reproduce
  under paired interleaving. Mitigation: every PT-1 bench is paired same-binary; W7 re-runs on a
  fresh copy in ≥2 windows and rejects any direction that inverts.
