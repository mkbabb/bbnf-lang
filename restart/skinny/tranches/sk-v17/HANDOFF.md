# Handoff SK-V17

Date: 2026-05-29.
HEAD at bracket: `1c5bd7a25`.
Status: Pass Alpha cycle V4 (folds V1 + V2 + V3 CHALLENGE dispositions CH1-CH7 +
CONSOLIDATED). The V3 substantive folds are carried (the V2 CH2-V2-F1 (b′)
sheets_witness repair, the 6→24 broadcast-count reconciliation, the
`css_l4.toml`-is-totality SK-V18 demotion). V4 folds the two residual V3
count-correction REVISEs (V3-CH1-a stale meta-note, V3-CH1-b grep-substring mislabel)
and the F1 orphan (alphaD:154 O5 grammar-derivation relabel). CHALLENGE V3 = 59/61
ACCEPT (96.7%), above the §3Z ≥95% bar; zero orphan REVISE into V4.

## Benched-substrate disclosure (load-bearing)

This handoff gates the **benched skinny tree** (`skinny/crates/`). The totality tree
(`crates/core/`) symbols `StructLayout`/`OpenFrame`/`CssArena`/`TapeStructBuilder`
are grep-clean-absent from `skinny/crates/` (verified) and are the SK-V18 fold
target, NOT SK-V17 owner paths. The benched flat-tape substrate is
`skinny/crates/runtime/src/tape/` (`Tape`/`ValueRef`/`TapeBuilder`/`PayloadArena`,
`mod.rs:94,175,38` + `assembler.rs:42,71`). The benched CSS Track 1 is today a
fact-stream String (`track1_facts -> Result<String,String>`, `nonjson_css_l4.rs:596`,
calling `track1::parser::parse`), emitted by `emit_fact_stream` (`generated.rs:5`),
routed by the hand-coded `W5C_REQUEST_FACT_PROFILES` array (`codegen/src/lib.rs:336`).
See `SYNTHESIS.md` benched-surface note for the full path translation.

## Current State

SK-V16 closes at `1c5bd7a25` with banked wins: the CSS L4 grammar-derived provider;
EXACT 8-field structural equality with cssparser (`rules=10136, style=9561,
sel=9561, decls=20043`, `track1_errors=0`, `cssparser_errors=0`, 4/4 corpora); the
cross-grammar PEG codegen branch-order fix; the O(1) generic speculative checkpoint
(`8153236e8`, 20x sound, grammar-neutral); and a shared flat-tape SUBSTRATE
(`skinny/crates/runtime/src/tape/`: `Tape`, `ValueRef`, `PayloadArena`, single
non-generic `TapeBuilder`, green correctness tests). JSON rides this tape; CSS does
not.

The W6 close is honest (`restart/audit/skinny-impl-overfit/sk-v16-w6tape-report.md`):
the substrate is **LANDED BUT UNWIRED for CSS** — zero CSS parse-path callers; the
benched CSS typed path is still the fact-stream String emitter
(`emit_fact_stream`/`W5C_REQUEST_FACT_PROFILES`), there is no eager `OpenFrame` tree
in skinny (that is the totality tree). CSS typed Track 1 is ~3.09 Mbps cold on the
typed-retime plane (`w6-speed-report.md:164`) / ~64-70 Mbps on the fact-stream
profile plane against lightningcss full-CSSOM at a run-dependent ~793/833 scrutineer
or ~61 build — roughly an order of magnitude (build plane) below the >SOTA bar. The
>SOTA bar is NOT met and nothing on the CSS path moved. (The lightningcss figure is
run-dependent: 833/809 in `w6-speed-report.md:59`, 793/61 in `w6tape-report.md:42-47`,
929.281 in the RESULTS.md W8R broadcast row, ~974 in the contract canonical; no
single committed number is 974 — the SK-V17 gate is the same-run re-baseline, §0.2/§0.5.)

`skinny/RESULTS.md` holds 51 admitted JSON rows (all A/GO, strict, same-plane, riding
the lazy-offset tape) — the proof the unified model is >SOTA and the regression
tripwire for tape activation. It also holds 24 `css_l4/*/direct_to_struct/main` rows
(lines 112-135, grep-verified `grep -c '^| css_l4/.*/direct_to_struct/main '` = 24),
all `not_admitted:SK-V15-W0-broadcast-diagnostic` / `AUDIT-FALSIFIED`, carrying the
single broadcast tuple `2319.041/2362.037/929.281` projected across all 24 rows. There
are ZERO admitted typed CSS rows; these 24 falsified broadcast diagnostics are the W8R
regression (one timing tuple → 24 conceptual rows) the N>=50 telemetry retires.

## What SK-V17 Opens

The subject: **CSS L4 typed parsing must BEAT lightningcss full-CSSOM on regular
corpora, with honest tailwind handling, via the UNIFIED TAPE / LAYOUT / PROJECTION
model generalized across ALL grammars + dav1d-style aarch64 NEON hot leaves.**

The gating artefact is **the lazy-view accessor generator** — it does not exist yet,
and lives in the skinny codegen tree (`skinny/crates/codegen/`). Everything
downstream (tape activation, CSS-on-tape, the >SOTA bench) is blocked on it. The
four-lever route (architecture doc §5, translated to the benched skinny tree):

1. Kill fact-stream String serialization (`emit_fact_stream`, `generated.rs:5`) →
   skinny `TapeBuilder` append (`assembler.rs:42,71`); RETIRE the hand-coded
   `W5C_REQUEST_FACT_PROFILES` CSS routing array (`codegen/src/lib.rs:336`), deriving
   routing from the grammar/`BackendRule` shape (the dominant benched-track1 cost).
2. Alloc removal: O(1) tape checkpoint (`offsets.len()` marker + truncate), no
   `split_off`, no `Vec<Vec>` arena, no eager per-leaf payload (removes the measured
   syslib floor).
3. NEON structural pre-scan: `to_bitmask64` movemask cascade + `byte_class_index_64`
   via `skinny/crates/bbnf-simd/src/dispatch.rs` `select_classifier`, replacing the
   scalar delimiter/balance scan. **S-P1 must re-confirm the hot leaf on the benched
   tape path**: the architecture profile's `find_component_delim ~56%` /
   `consume_balanced_at ~10%` figures are inherited from the core-tree profile and
   are NOT assumed; they are re-profiled on the benched skinny path before any kernel
   lands (actual-profiling).
4. Commit-by-construction spine: remove speculative rollback on the structural
   backbone.

Honest ceiling: 300-600 Mbps band; first cross of the lightningcss bar plausible on
animate/bootstrap; tailwindcss hardest and may land short on the first pass.

Generality scope (SYNTHESIS §0.4): the projection generator's exercised riders are
**JSON + CSS only** — JSON is the existing generated `value_from_ref` witness, CSS is
the new rich rider. `sheets_witness` CANNOT serve as a projection-generator exercise
(it is a 24-line `EventGrammar` byte-classification trait impl with no `.bbnf` /
parser / `BackendRule` shape to walk; codegen treats sheets/bbnf as fail-closed
negative controls). The projection generator's non-CSS-non-JSON generality is
asserted-by-construction with proof deferred to SK-V18; SK-V17 does NOT claim the
Lock 14 CSS+Sheets minimum is met (witness is JSON+CSS, not Sheets — Lock 14
phrase #2, `LOCKS.md:386-387`). The NEON SIMD leaf's non-JSON exercise IS `css_l4`
(a real rider sharing the `select_classifier(alphabet)` kernel) — sound and distinct
from the projection-generality scope.

Also opened: retire the `W6_SAMPLE_COUNT=1` single-sample harness (N>=50 cold +
median); rewire lightningcss as a same-run full-CSSOM-materializing comparator (not a
fact-stream); clean-regen the 8 dirty generated CSS / real-typed files.

## Authority

- `restart/skinny/tranches/sk-v17/SYNTHESIS.md` (this contract's goalset).
- `restart/skinny/tranches/sk-v17/research/alpha/{alphaA..alphaE}.md` +
  `research/alpha-hardening/V1/{CH1..CH7,CONSOLIDATED}.md`.
- `restart/audit/skinny-impl-overfit/sk-v16-css-sota-tape-architecture.md` (core-tree
  paths translated to the skinny benched tree per the benched-surface note).
- `restart/audit/skinny-impl-overfit/sk-v16-w6tape-report.md` +
  `sk-v16-w6tape-conversion-report.md` + `sk-v16-w6-speed-report.md` +
  `sk-v16-w6p1-dimension-dispatch-report.md` + `sk-v16-w6p2-o1-checkpoint-report.md`.
- `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md` +
  `restart/audit/skinny-impl-overfit/V2/CONSOLIDATED-AUDIT.md`.
- `restart/skinny/tranches/sk-v16/SYNTHESIS.md` + `HANDOFF.md` + `SPEC.md`.
- `skinny/RESULTS.md`, `skinny/REDRESS.md`, `restart/HANDOFF.md`,
  `restart/locks/LOCKS.md` (Lock 1 substrate-union, Lock 6/14 generated-output,
  Lock 14 grammar-neutrality, Lock 16 SIMD parity).
- `restart/prompts/pass-contracts/PASS-ALPHA.md`, `restart/prompts/ORCHESTRATOR.md`.

## Gate Posture

`PASS-ALPHA.md` and `ORCHESTRATOR.md` describe G-Alpha as mandatory, but the active
user pin says only G-Omega is mandatory and every other gate auto-passes. SK-V17
follows the active user pin: do NOT stop for G-Alpha. Stop only at G-Omega, an
unrepaired invariant violation, or completed SK-V17 close.

Alpha hardening runs the six-lens CHALLENGE pass per `ORCHESTRATOR.md` §3W (CH1
Correctness / CH2 Generality / CH3 Regression / CH4 Cost / CH5 Hidden Coupling /
CH6 Next-Tranche-Impact), writing
`restart/skinny/tranches/sk-v17/research/alpha-hardening/V{N}/CH{1..6}.md` +
`CONSOLIDATED.md`. CH7 overfit-prune is a **pass-added monotonic extension lens**
(the six-lens set CH1-CH6 is the orchestrator-citable canon; CH7 is added by this
pass beyond it, not elevated to the mandatory set by §3W). Its scan scope: every
SK-V17 candidate is checked for relocated overfit on the skinny-greppable surface —
specifically: (a) `W5C_REQUEST_FACT_PROFILES` (`codegen/src/lib.rs:336`) is RETIRED
and not relocated into projection DATA; (b) no per-rule-id match arms or hand-curated
packing/color constants enter the skinny generic crates that JSON does not need;
(c) every residual CSS routing entry names its `.bbnf` rule; (d) the CSS regen profile
array (`regen_css.rs:45-153`) trends toward the JSON emitter shape. The `css_l4.toml`
LOC convergence is NOT a CH7 scan gate — `css_l4.toml` is a TOTALITY artefact
grep-clean-absent from `skinny/`, so its `json.toml`-parity trend is an SK-V18
totality-fold metric only, noted not gated. CH7 runs alongside CH1-CH6 before S-P0.

## Pre-Blocked Routes (binding on S-P0 through S-P3)

SK-V17 must NOT reopen (full semantics in `SYNTHESIS.md` §0.4):

- AZ-IV eager-value-tree materialization (118x regression); eager per-leaf payload
  / f64-alloc-per-number / per-color `Box<CssColor>`. Materialization stays
  lazy-by-default via `ValueRef`.
- StructRegistry / Arena<G> / Builder<G> hot-path indirection (28-65x; 983x css
  bootstrap; 10583x tailwind WATCHDOG). No registry in the per-leaf hot path.
- CSS fact-stream String serialization as a live admission plane
  (`emit_fact_stream`/`CSS_GENERATED_RS`/`CssFullParseSummary`, `generated.rs:5`):
  diagnostic-only.
- The hand-coded `W5C_REQUEST_FACT_PROFILES` CSS profile array (`codegen/src/lib.rs:336`):
  RETIRE, do not extend or relocate into projection data (the overfit re-entry seam).
- The 24-row broadcast measurement (one timing tuple → 24 conceptual admits; the
  source of the 24 falsified `css_l4/*/direct_to_struct/main` RESULTS rows, lines
  112-135).
- Fixture / FNV contrivances; FNV production migration; FNV stays bench-only.
- x86 / AVX-512 / SVE (Apple cores have no SVE). aarch64 NEON + optional
  dotprod/i8mm only.
- brace-counter CSS admission; lightningcss CSSOM comparison before Track 1 emits
  comparable CSSOM; deleting legacy CSS generated/runtime shims before replacement
  proof; full-codegen close claims while dirty generated files remain.
- No second substrate: an introduced skinny `StructLayout`/`TapeStructBuilder`/
  `TapeCursor` alongside the landed `Tape`/`ValueRef` is a Lock 1 type-ambivalence
  violation (REJECT). The projection generator emits accessors over the EXISTING
  `Tape`/`ValueRef`; no new cursor/builder type.

Inherited REDRESS families (semantics carried): `28+33, 50-55, 60-72, 80, 82-84,
88, 89, 96-98, 183/184/209-213, 215, 242-247, FNV closed-enum production migration`.

Hidden-coupling escapes (Lock 1 substrate-union, forbidden unless G-Omega amends):
retained sidecars / sidecar tables / sidecar event vectors, retained cursor/list,
cursor streams, aux density/projection tables, parser-owned structural
projections/streams, parallel source passes, second tapes, public `UnionTape`, new
substrate APIs, sixth `BackendShape`, production FNV arbiters, production
hash-correctness proof, Track 1 == Track 2 sidecars, wrong-plane comparator
admission, cross-call classifier-state retention.

## Next Move

**next-move = ready-for-T-P1 (totality fold) then W0 dispatch.** (S-P3 CONVERGED
2026-05-29: CHALLENGE V2 95.8% → V3 97.9%, zero REJECT, zero orphan disposition-flipping
REVISE, V≤5, waves 6≤12; per `HARDENING-S-P3-V3-CONSOLIDATED.md`.) The SK-V17 SPEC is the
contract: W0 (baseline + telemetry lock + lightningcss CSSOM re-baseline) is dispatchable
now; W1–W5 conditionally gated; W4/L9 doubly-conditional on the post-W1 re-profile. One
non-gating residual carried to the W0 first-touch: the R1 three-line P3-A/P3-C SPEC-line
citation re-key (`:447`→`:475`; `:616,637`→`:670-672,695`; `:388,391`→`:390,396,446-448`).

The narrative below is the Pass-Alpha-time plan that produced this SPEC; it is superseded
by the converged SPEC + the next-move line above.

1. Run the CHALLENGE pass (CH1-CH6 canon + CH7 pass-added overfit-prune extension)
   over alphaA-F; fold dispositions into Pass Alpha V{N+1} if any REVISE/REJECT.
   Converge per ORCHESTRATOR §3Z (≥95% ACCEPT x2 consecutive, zero orphan REVISE,
   V<=5).
2. After CHALLENGE convergence, dispatch skinny pass S-P0 (overfit audit) → S-P1
   (profile: **re-confirm the fresh CSS hot leaf on the benched skinny tape path** —
   the architecture profile's `find_component_delim` ~56% / `consume_balanced_at`
   ~10% / `emit_*` ~34% figures are from the core-tree profile and are tagged
   S-P1-re-confirm-on-benched-path, NOT assumed) → S-P2 (research surviving
   grammar-neutral candidate classes) → S-P3 (author `sk-v17/SPEC.md` with the §4.4
   wave plan + `DISPATCH-PROMPT.md`).
3. S-P3 sequences the four-lever waves over the benched skinny tree preserving
   dependency order: (W1) lazy-view accessor generator in `skinny/crates/codegen/`
   (`grammar_provider.rs` + `lower/{tape_plan,offset_tape,event_tape}.rs`) + codegen
   unification → (W2) tape activation + builder seam flip (CSS off
   `RuntimeEmitterKind::RequestFacts`, retire `W5C_REQUEST_FACT_PROFILES`,
   `emit_fact_stream` → skinny `TapeBuilder`; emit into `runtime/src/tape/`,
   read via `ValueRef`) → (W3) CSS typed equality re-proof + N>=50 full-CSSOM bench
   → (W4) NEON structural index union via `bbnf-simd/src/dispatch.rs`
   (profile-first re-confirmed, gated behind tape) → (W5) commit-by-construction
   spine + adversarial tailwind tuning + C4a udot orphan wiring; C4b i8mm kernel
   ONLY if the W4 re-profile proves the digit leaf is a top-N tailwind self-time
   leaf. Each primitive lands WITH its hot-path consumer in the same commit (no
   orphan kernels).
4. S-P3 binds the `--skv17-css-sota-report` gate consumer (per-corpus median,
   N>=50, full-CSSOM same-run comparator, EXACT equality before speed,
   preserve-rich-ast, `tape_activated`, `w5c_profile_array_retired`, per-corpus
   delta-vs-lightningcss, hot leaf) and re-uses the SK-V16 dirty-generated +
   native-simd report consumers. `tape_activated` is satisfied ONLY when the benched
   `track1::parser::parse` emits into the skinny runtime `Tape`, read via `ValueRef`,
   proven by `PayloadArena` write/alloc counters — NOT by a grep returning non-zero
   in `crates/core/` (wrong-tree dishonesty is REJECTed).
5. Clean-regen the 8 dirty generated CSS / real-typed files; `cargo xtask regen
   --check` 9/9 exit 0 before any close claim.
6. Run totality passes + Pass Omega only where the loop requires them or a
   spec-class amendment surfaces (the tape/layout/projection model folds into
   TOTALITY at the `crates/core/` tree in SK-V18; if it amends the Lock 1 substrate
   manifest or a BackendShape, route through Pass Omega + G-Omega).
7. Close criterion: at least one regular corpus (**animate OR bootstrap** — the two
   regular corpora in the benched set `css_l4_corpus.rs:22-54`; `normalize` is NOT
   benched) crosses the lightningcss full-CSSOM bar at N>=50 median, with EXACT
   cssparser typed equality re-proven, preserve-rich-ast intact, JSON 51/51 held.
   tailwindcss crossing is a stretch; its honest residual gap is acceptable and
   recorded in REDRESS. If no regular corpus crosses, record the honest residual and
   escalate per PASS-ALPHA §8.

Revert protocol, hard caps, and per-wave triumvirate discipline are sanctioned-deferred
to S-P3 (PASS-ALPHA §4.4 authority), not paper-closed here.

## Close status (W5, 2026-05-30, HEAD `6bb4b2a6c`)

**SK-V17 is CLOSED** — R10 met. The five waves executed:

- **W0** — baseline/telemetry; lightningcss@W0 >SOTA bar LOCKED per corpus.
- **W1** — fact-stream String PRUNED; CSS Track-1 routed into the existing skinny
  offset tape; `W5C_REQUEST_FACT_PROFILES` retired; EXACT 4-field equality.
- **W2** — rich lazy 9-field typed CSSOM projection (`BackendRule`-walking, zero
  payload writes); EXACT 9-field cssparser equality.
- **W3** — shared grammar-neutral aarch64 NEON eq-set classifier + 2 net-new mask
  primitives (checkasm PASS, same-wave consumed); >SOTA MET.
- **W4** — commit-by-construction Alt-mode (CONDITIONAL): **NOT-MET, L9 not-needed**.
  The post-W1 re-profile (samply, bootstrap + material) measures 0% speculative
  checkpoint/rollback self-time; W1's PRUNE already rebuilt the recognizer as a
  commit-as-you-scan delimiter parser. The sound subset is empty. Honest
  conditional-not-met close per `SPEC.md:679-680`.
- **W5** — close: `regen --check` 9/9 exit 0; Lock-14 grammar-neutrality CLEAN;
  RESULTS reconciled; invariants held; R10 CLOSED.

**Final >SOTA (N=200 cold median, rich-typed vs lightningcss full-CSSOM):** bootstrap
2.210× · animate 2.355× (both regular corpora cross) · tailwind 3.348× · material
1.996×. EXACT 9-field cssparser equality; JSON 51/51 held; preserve-rich-ast intact.

Residuals routed to SK-V18 (close ledger §5): the literal single-emitter
codegen-unification (REDRESS-W2-1), the crates/core totality-tree adoption, the
Sheets/BBNF-self projection generality. The ~7 pre-existing bbnf-bench audit-overlay
census failures are verified pre-existing and carried forward (they do not block the
close).

Close ledgers: `research/w5/skv17-W5-close-ledger.md` (verdict + RESULTS
reconciliation + residual ledger), `research/w4/skv17-W4-conditional-ledger.md`
(W4 re-profile note). On close, Pass Alpha dispatches the SK-V17→SK-V18 synthesis.
