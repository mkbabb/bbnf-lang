# SK-V14 S-P1 Profile — V3 CHALLENGE Consolidated — G-S-P1-CONVERGED

Aggregator: SK-V14 S-P1 V3 hardening aggregator (write-only).
Date (UTC): 2026-05-23.
Scope: seven-lens CHALLENGE V3 confirming pass over the V2 P1 axis
artefact set, byte-identical to the V2 micro-fold landed at commit
`069ba203c413d46e7a5d465a128a983254e53841`
(`docs(sk-v14-p1-profile): V2 light micro-redispatch — five orphan
REVISEs landed`). HEAD at V3 dispatch is `4ad8f1949` (V2 hardening
aggregator commit; `docs(sk-v14-p1-hardening-V2): challenge V2 +
consolidated`); no further P1-axis edits occurred between V2
substrate commit and V3 dispatch — the seven V3 lens agents
re-evaluated the same six P1 artefacts (`p1a-samply-mode-1.md` …
`p1f-results-delta.md`) under V2 + V3 byte-equality discipline.
Authority: `restart/prompts/ORCHESTRATOR.md §3W` (lens registry) +
`§3Z` (≥95 % × 2 cycles, zero orphan REVISEs convergence rule; V max=5
ceiling); `restart/prompts/skinny/PASS-1-PROFILE.md §3` (CH1-CH6
specialisations + §6 dispatch gate); `restart/prompts/skinny/PASS-0-
OVERFIT-AUDIT.md §CH7` (Overfit-Prune lens binding from S-P0); V1
baseline `restart/skinny/tranches/sk-v14/research/p1/hardening/
HARDENING-S-P1-V1-CONSOLIDATED.md` (NOT-CONVERGED-V2-REQUIRED; 91.4 %
sub-axis / 95.16 % per-lens; three orphan REVISEs); V2 baseline
`restart/skinny/tranches/sk-v14/research/p1/hardening/
HARDENING-S-P1-V2-CONSOLIDATED.md` (CONVERGED-EXPECTING-V3-CONFIRM;
99.43 % sub-axis / 99.88 % per-lens; zero orphan REVISEs); V3 dispatch
context inherited from `restart/skinny/tranches/sk-v14/research/p1/
hardening/V1/CHALLENGE-CONTEXT.md §0-§5` (no V3-specific dispatch
context file; V3 lens agents read the V1 dispatch verbatim + V2 lens
disposition as the prior canonical statement, per `ORCHESTRATOR.md
§3Z` confirming-cycle protocol).
Input ledger: seven V3 lens dispositions under
`restart/skinny/tranches/sk-v14/research/p1/hardening/V3/`
(`CH1.md` 287 lines, `CH2.md` 283, `CH3.md` 332, `CH4.md` 207,
`CH5.md` 278, `CH6.md` 269, `CH7.md` 478 — 2134 lens lines).

## §0 — V3 cycle verdict + G-S-P1-CONVERGED

### §0.1 Per-lens dispositions (verbatim from each V3 CH file)

| Lens | V1 rate | V2 rate | V3 sub-axes | V3 ACCEPT | V3 REVISE | V3 REJECT | V3 rate | V3 verdict |
|---|---:|---:|---:|---:|---:|---:|---:|---|
| CH1 CORRECTNESS | 89.6 % | 99.13 % | 115 (25+19+25+14+18+14 per artefact) | 114 | 0 (1 ACCEPT-WITH-NOTE on P1-D §1.1 self-referential line drift carried forward unchanged from V2 — non-REVISE; one cosmetic Finding 0 on V2 CH1 §6 Sources line-count, NOT in sub-axis count) | 0 | **99.13 %** | ACCEPT (V2 ACCEPT-rate preserved exactly; zero new REVISE; one cosmetic V3-introduced documentary finding on V2 hardening file itself, not on P1 artefact) — **LOCKED** (V2+V3 both ≥95 %) |
| CH2 GENERALITY | 100 % | 100 % | 4 in-scope artefacts (P1-A/B/C/E) | 4 | 0 | 0 | **100 %** | ACCEPT (V2 R1 closure intact; R2/F1/F2 + F-V2-CH2-1/2/3 carry-forward to S-P2; one new V3 cite-hygiene observation F-V3-CH2-1 on V2 §1.3 vestigial `xctrace_probe` Cargo.toml path — substantive F1 verification unaffected; xctrace_probe is now `bbnf-bench` bin) — **CLOSED at V3** (3-cycle 100 %) |
| CH3 REGRESSION (REDRESS) | 100 % | 100 % | 6 artefacts × 43 §4 anomalies | 6 | 0 | 0 | **100 %** | ACCEPT (V2 F-1 closure intact; ANOM-1/2/3 + REDRESS-126 pre-block guard intact at unchanged line geometry; 43/43 §4 anomalies intact; V3 introduces zero new findings — F-V3-CH3-A invariance proof + F-V3-CH3-B inheritance chain are documentary-only) — **CLOSED at V3** (V2+V3 both 100 %) |
| CH4 COST | 93.5 % | 100 % | 49 (31 V1-base + 18 V2-disclosure) | 49 | 0 | 0 | **100 %** | ACCEPT (CF-1 closure intact; build_flags_regime row across P1-A/B/C/D verbatim at V2 line anchors; 4-point refusal lattice intact; cohort regime intact; Cargo.toml `:78-86` cross-check re-verified `target-cpu` NOT pinned; CF-V3-1 surfaces zero new findings — confirming pass is clean) — **LOCKED at V3** (V2+V3 both 100 %) |
| CH5 HIDDEN COUPLING | 83 % | 100 % | 6 artefacts × 5 sub-axes | 6 | 0 | 0 | **100 %** | ACCEPT (CH5-A closure intact via F-V2-P1E-1; CH5-B persists CLOSED-VIA-CONTRACTED-DEFERRAL; CH5-C + CH5-D carry to S-P2 by structural design; substrate-union framing intact across three canonical paragraphs `p1e:246`, `p1c:450,455`, `p1a:318-321`; Track 1 vs Track 2 entry-point grep witnesses re-verified at HEAD) — **CLOSED at V3** (V2+V3 both 100 %) |
| CH6 ANTI-PAPER-CLOSE | 100 % | 100 % | 6 artefacts (7 §-rate cells) | 6 | 0 | 0 | **100 %** | ACCEPT (F-V2-P1ABC-RERECORD CONTRACTED-DEFERRAL re-verified via all three discriminators — named cause + named route + named decision precedent; parse-attribution feature gate plumbing intact at **14 functions** in `runtime/src/grammars/json/generated.rs` — V1 saw 8, V2 saw 11+, V3 confirms 14; P1-C ANOM-4 paper-close attestation at `:489-491` byte-identical; P1-E `github_events` 8-sample CH6 risk at `:111` byte-identical) — **3-cycle 100 %**; gate OPEN |
| CH7 OVERFIT-PRUNE | 100 % | 100 % | 7 subclauses (4 explicit + 3 supporting) | 7 | 0 | 0 | **100 %** | ACCEPT (audit-overlay 261-cell census preserved byte-for-byte; PRUNE-1 W14.* 47-hit consistency preserved; zero fake-`@generated` recurrence in P1 artefact bodies — 13 hits all confined to `hardening/V{1,2}/CH7.md` documentary citations; 9-grammar cargo-metadata census preserved; F-V2-P1F-1 contracted-deferral framing block at `p1f:179-187` preserved; one new V3 documentary-hygiene observation §3.5 on cumulative `@generated` literal growth — non-blocking) — **3-cycle 100 %**; gate OPEN |

### §0.2 Aggregate ACCEPT-rate

Two aggregation methods (per `ORCHESTRATOR.md §3Z`):

- **Sub-axis-weighted (load-bearing for §3Z convergence):**
  (114 + 4 + 6 + 49 + 6 + 6 + 7) / (115 + 4 + 6 + 49 + 6 + 6 + 7)
  = **192 / 193 = 99.48 %**.
- **Per-lens mean (informational; equal weight per lens):**
  (99.13 + 100 + 100 + 100 + 100 + 100 + 100) / 7 = **99.88 %**.

Both aggregation methods comfortably clear the §3Z ≥95 % floor for
the **second consecutive cycle**. V2 → V3 sub-axis trajectory:
99.43 % → 99.48 % (+0.05 pp net; CH4 sub-axis denominator grew from
31 to 49 between V2 and V3 as the V2 18-row disclosure matrix was
incorporated into the V3 base, reducing the impact of the single CH1
non-ACCEPT row from 1/175 to 1/193). Per-lens mean **identical** to
V2 (99.88 % → 99.88 %), reflecting bit-identical lens dispositions
on six of seven lenses and unchanged CH1 99.13 % rate.

### §0.3 REJECT roster

**Zero REJECT findings** across all 7 V3 lenses. V3 introduces no
falsification of any P1 axis claim; the V1 + V2 zero-REJECT posture
is preserved across the full chain.

### §0.4 REVISE roster (orphan accounting)

**Zero orphan REVISEs at V3.** No V2 carry-forward REVISE existed
(V2 closed all three V1 orphans via five light packets per
`HARDENING-S-P1-V2-CONSOLIDATED §0.4`); V3 introduces no new REVISE
across any of the 7 lenses. Per `ORCHESTRATOR.md §3Z`, the orphan-
REVISE clock requires **zero** open REVISEs at each cycle counted
toward the "× 2 consecutive cycles" sub-clause — V2 and V3 both
satisfy this in full. **§3Z orphan-REVISE sub-clause: SATISFIED at
V2+V3 chain.**

### §0.5 V3-introduced non-REVISE findings (S-P2 first-wave routing)

Three non-blocking V3-fresh findings surfaced during V3 confirming;
all are documentary / cite-hygiene / process-class and none warrants
a V4 micro-fold or §3Z reopen:

1. **CH1 Finding 0 (V3 cosmetic; non-blocking) — V2 CH1.md §6 Sources
   cites P1-A line count as 345 vs HEAD 343.** A 2-line cosmetic
   drift on the V2 hardening lens document's source-cite block; does
   NOT affect any in-artefact P1-A cite. All 25 of P1-A's in-artefact
   file:line anchors verified at HEAD per CH1 V3 §0.3. Aggregator
   awareness only; not P1 artefact correction.

2. **CH2 F-V3-CH2-1 (V3 cite-hygiene; non-blocking) — V2 §1.3 cites
   `skinny/crates/xctrace_probe/Cargo.toml` as a sibling crate, but
   at HEAD `xctrace_probe` is a `bbnf-bench` bin** (`skinny/crates/
   bbnf-bench/src/bin/xctrace_probe.rs`); no standalone Cargo.toml
   exists. V2's substantive F1 verification still holds (bench-harness
   `--features runtime/parse-attribution` transitive form is the
   correct unmask invocation); only the V2 cite path is vestigial.
   Routed to S-P2 first-wave grooming.

3. **CH7 §3.5 (V3 documentary-hygiene; non-blocking) — cumulative
   `@generated by skinny bbnf-codegen` literal occurrences in
   `restart/skinny/tranches/sk-v14/research/p1/` grew V1=4 → V2=13
   under documentary-citation accumulation in hardening CH7 cycles.**
   V3 CH7 was authored to avoid code-fence quoting of the literal so
   that V3 HEAD does not bump the count further; CH7 §4.2.1 CI-lint
   recommendation gains a precise firing rule (code-fence content
   equals literal, NOT a grep/git grep command line, NOT in
   `hardening/V{N}/CH{M}.md`). S-P3 / LOCKS.md follow-up.

The two V2 §0.5 process-class observations carry forward unchanged at
V3 and remain routed to S-P2 first-wave grooming or process-class
LOCKS.md adoption (not S-P1 work):

4. **CH6 §3.6 volatile /tmp filesystem state.** V1 path-existence
   verifications cannot be re-executed at V3 confirming because macOS
   wipes `/tmp` between agent sessions; V1 CH6 §1.1-§1.4 evidence
   tables remain the binding path-existence record. Process-class
   carry; consider relocating sidecar `.syms.json` + `identity.txt`
   artefacts to `restart/skinny/tranches/sk-v14/research/p1/artefacts/`
   for future hardening-cycle re-verification.

5. **CH7 §3.3 cargo-metadata command-portability hygiene + CH1
   P1-D §1.1 self-referential line drift carry.** The cargo metadata
   command should annotate `cd /Users/mkbabb/Programming/bbnf-lang`
   prefix or `--manifest-path` form in V1/V2/V3 CH7 §2.1 documentary
   form; the V3 dispatch operationalized the workaround via
   instruction. P1-D §1.1 self-ref drift (`s/41/53/` + `s/62/74/`)
   remains a 30-second mechanical fix that would lift CH1 from
   99.13 % to 100 % × 6/6 if pursued — V2 §0.5 observation 1 carried
   forward verbatim at V3 (CH1 V3 §3 Finding 1). Aggregator option;
   non-blocking on §3Z LOCK.

### §0.6 Convergence vote + G-S-P1-CONVERGED

Per `ORCHESTRATOR.md §3Z` (≥95 % × 2 consecutive cycles, zero orphan
REVISEs across both counted cycles):

| Cycle | Sub-axis rate | Per-lens mean | Orphan REVISEs | §3Z status |
|---|---:|---:|---:|---|
| V1 | 91.4 % | 95.16 % | 3 (CH1 P1-E typed-plane; CH4 CF-1 RUSTFLAGS; CH5 P1-F schema gap) | Did NOT meet on either sub-clause |
| V2 | 99.43 % | 99.88 % | **0** | First ≥95 % cycle; orphan-REVISE sub-clause satisfied |
| **V3** | **99.48 %** | **99.88 %** | **0** | **Second consecutive ≥95 % cycle; orphan-REVISE sub-clause satisfied** |

**§3Z LOCK SATISFIED at V2+V3 chain.** Both ≥95 % on both
aggregation methods; zero orphan REVISEs across both counted cycles.
The CH6 + CH7 lenses **stand-alone-discharge §3Z** at V2 already
(CH6: V1=100 % + V2=100 %; CH7: V1=100 % + V2=100 %; both with zero
REVISE at V1 and V2); V3 provides supererogatory third-cycle
confirmation for both (3-cycle 100 % across CH6 and CH7). The
remaining five lenses (CH1, CH2, CH3, CH4, CH5) discharge §3Z at the
**V2+V3 two-consecutive-cycle chain**: V2 was the first ≥95 % cycle
on these five; V3 is the second. **All seven lenses now discharge
§3Z** at or before V3 with margin.

### **Cycle verdict: G-S-P1-CONVERGED.**

V3 closes the SK-V14 S-P1 Profile Pass §3Z chain on all seven
lenses. No V4 / V5 required; V max=5 ceiling not approached. The
heavy F-V2-P1ABC-RERECORD remains correctly **CONTRACTED-DEFERRED to
S-P2 entry artefact** per V1 aggregator Option X (carried unchanged
through V2 and re-verified intact at V3 — parse-attribution feature
plumbing intact at 14 gates; named cause + named route + named
decision precedent all preserved). S-P1 PROFILE PASS converges; the
S-P2 dispatch gate opens immediately upon V3 aggregator commit per
`PASS-1-PROFILE.md §6` + the SK-V14 ORCHESTRATOR-PROMPT THE SK LOOP.

Per SK-V14 ORCHESTRATOR-PROMPT discipline ("do not relinquish except
at G-Omega"), **G-Alpha is auto-signed** at G-S-P1-CONVERGED and the
orchestrator proceeds directly to S-P2 dispatch per
`restart/prompts/skinny/PASS-2-RESEARCH.md`, with concurrent Pass
Omega T-P1 dispatch per the SK LOOP.

## §1 — Full S-P1 chain summary (axis pass + V1 + V2 + V3)

### §1.1 — Axis pass (commit `3510c1de5`)

Six P1 axis files atomic-committed 2026-05-22 → 2026-05-23 early
under `restart/skinny/tranches/sk-v14/research/p1/S-P1-DISPATCH-CONTEXT.md`:

- `p1a-samply-mode-1.md` (340 lines initial; parse_only × 17 corpora;
  atos -inlineFrames pipeline; LTO-fused `dispatch_value` envelope).
- `p1b-samply-mode-2.md` (320 lines; direct × 17 + typed × 11 = 56
  profiles; `DirectParser::skip_value` typed-plane substrate-walk
  finding).
- `p1c-samply-mode-3.md` (607 lines; mode-III × 17 × 4 probes;
  8 ANOMs including `alternate_scalar_plan` misnaming).
- `p1d-pmu-cycles.md` (648 lines; 231 PMU rows; cycles+inst
  REACHABLE; PMC counters UNREACHABLE).
- `p1e-hot-leaf-attribution.md` (306 lines; CH2 Lock-14
  mis-attribution census; –40 admit AUDIT-FALSIFIED).
- `p1f-results-delta.md` (260 lines; 75 rows; 8 schema escalations;
  45/45 stale criterion-slope hot-leaf).

Total: 2481 lines at axis-pass commit `3510c1de5`.

### §1.2 — V1 CHALLENGE (commit `a3dfcaf38`)

Seven-lens write-only V1 challenge (no fold packets; lens-only
disposition). V1 aggregate sub-axis 91.4 % / per-lens mean 95.16 %;
three orphan REVISEs surfaced:

- CH1 BINDING REVISE on P1-E typed-plane file:line drift (5 stale
  `generated_real_typed.rs` cites — `:1739` → `:2949` +1210 on
  `DirectParser::skip_value`, etc.).
- CH4 CF-1 RUSTFLAGS regime drift (P1-A asserted "native per
  Cargo.toml" but no shell carries it; P1-B explicit unset; P1-C/D
  explicit native — silent cross-row regime drift).
- CH5 P1-F `track2_entry_point` schema-column gap (CH5 hidden-coupling
  guard absent from RESULTS.md; 0 matches across 186 lines).

V1 aggregator decision: NOT-CONVERGED-V2-REQUIRED; five light fold
packets prescribed (F-V2-P1E-1 BINDING; F-V2-METHODOLOGY-1;
F-V2-P1F-1 reclassification; F-V2-P1A-MOVEMASK + F-V2-P1C-LINEDRIFT
ACCEPT-WITH-NOTE closures); heavy F-V2-P1ABC-RERECORD packet
deferred to S-P2 entry artefact per Option X (V1 §3.2:
"parse-attribution rebuild is primitive-design ground-truth, not
lens-correctness fix").

### §1.3 — V2 light micro-redispatch (commit `069ba203c`)

Five V2 light packets landed atomically across the six P1 axis
artefacts; +86 / −26 line delta; no symbol re-record:

- **F-V2-P1E-1** (BINDING) — typed-plane file:line refresh in P1-E
  §1.2 + §2.3 + §5.4; 7 typed-plane rows refreshed against HEAD
  `generated_real_typed.rs` (3056 lines; 8 grep hits at
  `:516/527/592/1150/1219/1330/2197/2949`). Mechanically closes CH1
  V1 BINDING REVISE; carry-through-closes CH2 R1+R2 + CH5 CH5-A.
- **F-V2-METHODOLOGY-1** — `build_flags_regime` row landed in all
  four P1-A/B/C/D frontmatters; cohort lattice canonical at V2
  ({P1-A, P1-B} unset; {P1-C, P1-D} native); 4-point cross-regime
  refusal rule encoded; P1-A V1 misstatement explicitly corrected
  with `skinny/Cargo.toml:78-86` evidence. **V2 cohort discovery:**
  P1-A was also RUSTFLAGS-unset, not native (V1 erroneously implied
  native cohort by "per Cargo.toml" — V2 corrects via Cargo.toml
  cross-check).
- **F-V2-P1F-1** — 5-paragraph contracted-deferral framing block at
  `p1f-results-delta.md:179-186` citing SYNTHESIS §2:232-258 + §3
  C-2:272 + PASS-ALPHA §4.4:112-122 verbatim. Reclassifies CH5 V1
  REVISE to ACCEPT-WITH-CONTRACTED-DEFERRAL.
- **F-V2-P1C-LINEDRIFT** — 3 NEON cites refreshed at
  `p1c-samply-mode-3.md` (`bulk_emit_positions_64_neon :2`,
  `bitmap_prefix_xor_64_neon :2`, `eob_pad_clamp_neon :4`); REDRESS
  path normalised `restart/skinny/REDRESS.md` → `skinny/REDRESS.md`
  at `:509 + :599`.
- **F-V2-P1A-MOVEMASK** — "Line-anchor convention" paragraph at
  `p1a-samply-mode-1.md:137` + 12 table cells annotated with
  `(fn @ N)` form. Codifies samply RVA-to-line attribution policy.

### §1.4 — V2 CHALLENGE (commit `4ad8f1949`)

Seven-lens write-only V2 challenge; V2 aggregator commits all 8
hardening files atomically. V2 aggregate sub-axis **99.43 %** /
per-lens mean **99.88 %**; **zero orphan REVISEs**. All three V1
orphan REVISEs verified closed in lens reports; two additional V2
ACCEPT-WITH-NOTE items closed via F-V2-P1C-LINEDRIFT +
F-V2-P1A-MOVEMASK. V2 verdict: CONVERGED-EXPECTING-V3-CONFIRM (first
≥95 % cycle on both methods; CH6 + CH7 standalone-discharge §3Z at
V2; remaining five lenses require V3 second-consecutive-cycle
confirmation).

### §1.5 — V3 CHALLENGE (this commit)

Seven-lens write-only V3 pure-confirming pass over byte-identical V2
P1 artefacts (V2→V3 P1 diff: zero bytes; only V2 hardening files
landed between V2 substrate commit `069ba203c` and V3 dispatch).
V3 aggregate sub-axis **99.48 %** / per-lens mean **99.88 %**; zero
orphan REVISEs; **G-S-P1-CONVERGED**. Three non-blocking V3-fresh
documentary observations (CH1 Finding 0 on V2 CH1 §6 cosmetic;
CH2 F-V3-CH2-1 vestigial xctrace_probe cite-hygiene; CH7 §3.5
cumulative `@generated` literal growth) all routed to S-P2 first-wave
grooming or LOCKS.md adoption — none alter §3Z LOCK.

## §2 — Handoff to S-P2

### §2.1 S-P2 entry artefacts

S-P2 inherits the following from S-P1:

**(a) Six P1 profile artefacts (HEAD `4ad8f1949`; substrate at
`069ba203c`):**
- `p1a-samply-mode-1.md` (343 lines; parse_only × 17 corpora; atos
  -inlineFrames pipeline; LTO-fused `dispatch_value` envelope;
  movemask annotation + RUSTFLAGS-unset cohort disclosure).
- `p1b-samply-mode-2.md` (323 lines; direct × 17 + typed × 11; 56
  profiles; `DirectParser::skip_value` typed-plane substrate-walk
  finding; RUSTFLAGS-unset cohort + cross-regime refusal guard).
- `p1c-samply-mode-3.md` (616 lines; mode-III × 17 × 4 probes;
  8 ANOMs; RUSTFLAGS-native cohort + 3 NEON line-anchor refresh +
  REDRESS path normalisation).
- `p1d-pmu-cycles.md` (669 lines; 231 PMU rows; cycles+inst
  REACHABLE; PMC counters UNREACHABLE; RUSTFLAGS-native cohort +
  two-target-dir regime re-confirmation).
- `p1e-hot-leaf-attribution.md` (321 lines; CH2 Lock-14
  mis-attribution census; typed-plane file:line refresh on
  `generated_real_typed.rs`; substrate-union §4.4 paragraph intact).
- `p1f-results-delta.md` (269 lines; 75 rows; 8 schema escalations;
  contracted-deferral framing for 4 NEW SK-V14 schema columns).

These artefacts form the **empirical floor for S-P2 primitive
design.** S-P2's CH2 generality argument rests on the four-witness
CH2 redundancy (P1-A + P1-B + P1-C + P1-E) sustained across V1+V2+V3.

**(b) Heavy F-V2-P1ABC-RERECORD packet (first wave deliverable per
V1 aggregator Option X — re-verified intact at V3):**

1. Rebuild `xtask`, `bbnf-bench` with
   `cargo build --release -p bbnf-bench --features runtime/parse-
   attribution` (**transitive form** per CH2 F1; feature lives at
   `runtime/Cargo.toml:21`; bench-harness propagates through dep
   declaration). NB: `xctrace_probe` is a **bbnf-bench bin** (not a
   standalone crate; `skinny/crates/bbnf-bench/src/bin/
   xctrace_probe.rs`) — V3 CH2 §3.1 corrects V2 §1.3's vestigial
   path cite.
2. Re-record P1-A samply (17 corpora × 1 plane = 17 profiles) under
   `--features runtime/parse-attribution`. Re-extract top-N tables.
3. Re-record P1-B samply (17 direct + 11 typed = 28 corpora ×
   2 planes = 56 profiles). Re-extract.
4. Re-record P1-C samply (4 mode-III probes). Re-extract.
5. Re-record `github_events parse_only` Track 1 with longer iter
   count (target ≥4000 samples) to crack the 8-sample inlined-std
   `<u16 as From<u8>>::from` noise envelope.
6. Append the `parse-attribution=on` top-N decomposition tables to
   P1-A §2.1, P1-B §2.1+§2.2, P1-C §2.1+§2.2 — converting CH6 "named
   + routed" status to "named + routed + executed".

Plumbing intact at V3 HEAD (CH6 V3 §1.2 verified):
**14 functions** gated with `#[cfg_attr(feature = "parse-attribution",
inline(never))]` in `skinny/crates/runtime/src/grammars/json/
generated.rs` at lines 33-34, 43-44, 58-59, 79-80, 86-87, 117-118,
138-139, 157-158, 185-186, 201-202, 211-212, 217-218, 236-237. V1 saw
8 gates; V2 saw 11+; V3 confirms 14 (gate count grew across passes
as bench-counters work landed). Feature declaration at
`runtime/Cargo.toml:21` unchanged. Route executable on S-P2 demand.

Cost estimate (wall, single-host sequential per V1 §2.6):
≈135 min sequential; ≈60 min parallel (3 captures concurrent on
independent target dirs).

**(c) Non-blocking process observations (S-P2 first-wave grooming
items):**

1. **CH7 §3.3 cargo-metadata cd-prefix** (working-directory
   annotation on the verbatim grammar-census command;
   `cd /Users/mkbabb/Programming/bbnf-lang` or `--manifest-path` form
   in S-P3 / LOCKS.md adoption).
2. **CH7 §4.2.1 fake-`@generated` CI lint extension** (distinguish
   documentary citations in `hardening/V{N}/CH{M}.md` from
   load-bearing claims elsewhere; V3 §3.5 reinforces with precise
   firing rule — code-fence content NOT inside grep/git grep command
   AND NOT in hardening lens path).
3. **CH6 §3.6 /tmp non-volatile relocation** (relocate `.syms.json`
   + `identity.txt` sidecars from volatile `/tmp/skv14-p1*/` to
   `restart/skinny/tranches/sk-v14/research/p1/artefacts/` for future
   hardening-cycle re-verifiability).
4. **CH2 F-V3-CH2-1 xctrace_probe path-cite refresh** (V2 §1.3
   vestigial `skinny/crates/xctrace_probe/Cargo.toml` reference;
   xctrace_probe is a `bbnf-bench` bin, not a standalone crate).

**(d) CH2 carry-forward findings (deferred from V1 + V2 — none
S-P1 blocker):**

- **F1 parse-attribution transitive-feature plumbing** — bench-harness
  must invoke `--features runtime/parse-attribution` (transitive
  form). V3 verified plumbing intact at 14 functions in
  `runtime/src/grammars/json/generated.rs`; feature declaration at
  `runtime/Cargo.toml:21`; bench-harness `runtime` dep at
  `bbnf-bench/Cargo.toml:18` does NOT add `parse-attribution` to
  default feature set, so the transitive form is required.
- **F2 CSS L4 zero-evidence asymmetry** — S-P2 must promote CSS L4
  zero-profile-evidence from CH3/CSS-substrate to a CH2 sub-finding
  in S-P2 fold; the CH2 cross-grammar generalization argument must
  be answered from JSON + CSS L4 spec **jointly**, without CSS L4
  profile corroboration (CSS L4 profile corpus does not exist at
  SK-V14 P1 dispatch).
- **R2 P1-E §2.2 `distinct_values` cite collapse** —
  `generated.rs:542` → `:506` (or `:506 (fn @ 506)` per F-V2-CH2-2
  convention); definition opens at line 506; line 542 is the closing
  `}`. Non-blocking — CH2 primitive class `dispatch` is correct
  independent of cite drift.
- **F-V2-CH2-1 `apache_builds` `parse_option_scalar_string` row** —
  the worked example of the CH2 cross-grammar generalization
  argument (the `string` primitive maps directly to CSS L4 / Sheets
  / BBNF-self); only non-`dispatch` typed-plane row in P1-E §2.3.
- **F-V2-CH2-2 `(fn @ N)` cite-hygiene convention** — currently only
  P1-A adopts it; standardize across P1-B, P1-C, P1-E in S-P2
  first-wave grooming.
- **CH5-C view-tree consumer source-touch budget** + **CH5-D
  parse-attribution unmasking** carry forward as S-P2 design-class
  items (CH5 V3 §3 ledger).

### §2.2 No CH-driven blocker on S-P2 dispatch

V3 confirms no lens surfaces a primary-capture falsification,
primitive mis-classification, REDRESS-family silent re-open,
methodology-irreproducibility, hidden-coupling (parallel substrate /
sidecar / Track 1 ≡ Track 2 collapse), paper-close pattern, or
overfit-pattern recurrence. The S-P1 P1 axis artefacts hold as a
sound input substrate for S-P2 primitive design across the V1 → V2 →
V3 chain.

CH6 explicitly records (V3 §5): "CH6 gate is OPEN for S-P2 dispatch;
the lens converges at V3 with no V4 MUST items." CH7 explicitly
records (V3 §4.4): "No CH7-driven blocker on S-P2 dispatch." CH2 V3
§4.3 records: "CH2 GENERALITY lens CONVERGES at V3; no CH2 V4/V5
work expected; S-P2 dispatch gate opens for this lens." CH3 V3 §8
records: "CH3 standalone CLOSES at V3; no CH3-grounded blocker to
either V4 or to S-P2 dispatch." CH4 V3 §6 records: "CH4 lens
convergence LOCKED at V3; zero blocker into the V3 aggregator." CH5
V3 §5 records: "CH5 gate is OPEN for S-P2 dispatch." CH1 V3 §5
records: "§3Z lock condition: SATISFIED. CH1 LOCKED. S-P1 → S-P2
dispatch may proceed from the CH1 side once all 7 lenses confirm
V2 + V3 ≥95 % with zero orphan REVISEs." All seven conditions met
at this commit.

## §3 — Methodological notes institutionalized for SK-V{N+1}

The S-P1 V1 → V2 → V3 chain crystallizes several methodology
practices for the next SK pass:

### §3.1 Belt-and-braces over deferral

V1 surfaced three orphan REVISEs; rather than defer them to S-P2,
the V1 aggregator routed all three through a **V2 light micro-fold**
(five light packets totaling +86/−26 line delta; no symbol
re-record). V2 closed all three orphans mechanically + closed two
additional V1 ACCEPT-WITH-NOTE items; V3 confirmed bit-identical
substrate stability with zero new REVISE. V2 sub-threshold
documentary observations (CH1 P1-D self-ref line drift; CH6 /tmp
volatility; CH7 cargo-metadata cd prefix) routed to S-P2 first-wave
grooming or process-class LOCKS.md adoption — **not deferred
indefinitely**, but routed to the next pass with explicit hand-off
artefact (this §2 + the F-V2-P1ABC-RERECORD specification carried at
each cycle's consolidated §3.2).

### §3.2 Atomic-commit-by-aggregator pattern

Four aggregator commits across the S-P1 chain (`3510c1de5` axis
atomic write; `a3dfcaf38` V1 hardening atomic; `4ad8f1949` V2
hardening atomic; this commit V3 hardening atomic). Zero
staging-race contamination across any of the four; each cycle's
seven CH files committed together with the consolidated. Per
`[agent-orchestration]` discipline ("never let sub-agents race on
shared files; commit before parallelizing"), the lens dispatches
were write-only (lens agents wrote `hardening/V{N}/CH{M}.md` only;
aggregator committed all 8 atomically). This pattern carries to all
future SK passes' Pass Alpha + Pass Profile + Pass Research sub-pass
hardening cycles.

### §3.3 Executable-verification mandate

The CHALLENGE-CONTEXT §3 executable-verification mandate ("if you
cite a path/file/symbol, verify it exists; if you cite a numerical
claim, recompute it") surfaced concrete cite-drift bugs at V1
(P1-E typed-plane file:line drift; P1-C NEON line-anchors; P1-C
REDRESS path) that would otherwise have entered S-P2 as
silently-wrong evidence. P1-A/B's atos -inlineFrames pipeline was
established as the headless equivalent of interactive `samply record`
per `[samply-symbol-resolution]`; CH1 + CH2 + CH4 + CH7 V1 all
surfaced cite-drift via grep verification. **V2 + V3 inherited this
mandate** and continued to verify every cited path:line against
HEAD source via `grep` / `wc -l` / `git diff` — V3 specifically
re-ran every V2 grep against HEAD `4ad8f1949` to confirm bit-identical
substrate stability.

### §3.4 V1 cohort discovery

The CH4 CF-1 orphan REVISE was not a routine cite-drift bug but a
**factual cohort mis-classification**: V1 P1-A asserted "native
target CPU per `skinny/Cargo.toml`" but no shell invocation carried
`-C target-cpu=native` and the `[profile.release]` block does not
declare `target-cpu`. V2 corrected the assertion via Cargo.toml
cross-check evidence; the cohort lattice is now schema-explicit
({P1-A, P1-B}-unset + {P1-C, P1-D}-target-cpu=native). The 4-point
cross-regime refusal rule encoded across P1-B `:10 + :185`, P1-C
`:23-25`, P1-D `:21-31` discharges the consumer-side enforcement —
any S-P2 / S-P3 aggregator computing cross-row Mbps/c/B deltas must
match `build_flags_regime` before comparing. This pattern is binding
for all future SK passes' P1 axis dispatches.

### §3.5 Contracted-deferral vs paper-close discrimination

CH5 V1 P1-F orphan REVISE was reclassified via F-V2-P1F-1 to
ACCEPT-WITH-CONTRACTED-DEFERRAL by anchoring on PASS-ALPHA §4.4:112-122
precedent verbatim ("This layer is authored downstream by skinny
pass S-P3 in `sk-v{N+1}/SPEC.md`"). The 5-paragraph framing block at
`p1f-results-delta.md:179-186` established the corpus-level textual
vocabulary for distinguishing contracted-deferral from paper-close,
parallel to the same precedent the V1 aggregator Option X invokes
for the heavy F-V2-P1ABC-RERECORD deferral. CH6 V2 read F-V2-P1F-1
as **CH6-positive** for this reason. The three CH6 discriminators
(named cause; named route; named decision precedent) are now
formalized for all future SK passes' deferral classifications.

## §4 — Sources

V3 lens dispositions (all verified existing at write-time):

- `restart/skinny/tranches/sk-v14/research/p1/hardening/V3/CH1.md`
  (287 lines; 99.13 %; one ACCEPT-WITH-NOTE on P1-D §1.1 carried
  forward unchanged from V2; one cosmetic V3 Finding 0 on V2 CH1 §6
  Sources line-count drift — not in sub-axis count).
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V3/CH2.md`
  (283 lines; 100 %; V2 R1 closure intact; F-V3-CH2-1 vestigial
  xctrace_probe cite-hygiene non-blocking; 3-cycle 100 % CLOSED).
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V3/CH3.md`
  (332 lines; 100 %; 43/43 §4 anomalies intact; F-1 closure
  persistent; F-V3-CH3-A invariance proof + F-V3-CH3-B inheritance
  chain documentary; CLOSED at V3).
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V3/CH4.md`
  (207 lines; 100 %; 49/49 sub-axes; CF-1 closure intact; CF-V3-1
  zero new findings; LOCKED at V3).
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V3/CH5.md`
  (278 lines; 100 %; CH5-A closure intact; CH5-B persists
  CLOSED-VIA-CONTRACTED-DEFERRAL; CH5-C + CH5-D carry to S-P2;
  CLOSED at V3).
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V3/CH6.md`
  (269 lines; 100 %; F-V2-P1ABC-RERECORD CONTRACTED-DEFERRAL
  re-verified; parse-attribution plumbing intact at 14 functions;
  3-cycle 100 %; gate OPEN).
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V3/CH7.md`
  (478 lines; 100 %; audit-overlay 261-cell census preserved;
  PRUNE-1 W14.* 47-hit consistency; zero fake-`@generated`
  recurrence in P1 artefact bodies; 3-cycle 100 %; gate OPEN).

V3 substrate artefacts under review (HEAD =
`4ad8f1949099829b7ad723ddfd7eeb2a40cf61cd`; substrate commit =
`069ba203c413d46e7a5d465a128a983254e53841`; V2→V3 P1-artefact diff
**zero bytes** verified by CH3 V3 §4 + CH5 V3 §1 + CH6 V3 §3.4 + CH7
V3 §2.6):

- `restart/skinny/tranches/sk-v14/research/p1/p1a-samply-mode-1.md`
  (V3 HEAD: 343 lines).
- `restart/skinny/tranches/sk-v14/research/p1/p1b-samply-mode-2.md`
  (V3 HEAD: 323 lines).
- `restart/skinny/tranches/sk-v14/research/p1/p1c-samply-mode-3.md`
  (V3 HEAD: 616 lines).
- `restart/skinny/tranches/sk-v14/research/p1/p1d-pmu-cycles.md`
  (V3 HEAD: 669 lines).
- `restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md`
  (V3 HEAD: 321 lines).
- `restart/skinny/tranches/sk-v14/research/p1/p1f-results-delta.md`
  (V3 HEAD: 269 lines).

V1 + V2 baseline + dispatch context:

- `restart/skinny/tranches/sk-v14/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md`
  (546 lines; V1 verdict NOT-CONVERGED-V2-REQUIRED; sub-axis 91.4 %;
  three orphan REVISEs; five V2 fold packet specs; Option X heavy
  deferral).
- `restart/skinny/tranches/sk-v14/research/p1/hardening/HARDENING-S-P1-V2-CONSOLIDATED.md`
  (295 lines; V2 verdict CONVERGED-EXPECTING-V3-CONFIRM; sub-axis
  99.43 %; per-lens 99.88 %; zero orphan REVISEs; V3 Option A
  pure-confirming dispatch recommendation).
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CHALLENGE-CONTEXT.md`
  (53 lines; V1 dispatch context — V2 + V3 both inherit this binding
  per CHALLENGE-CONTEXT §3 executable-verification mandate).
- `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/`,
  `V2/` lens dispositions (V1: 7 lens files + consolidated; V2: 7
  lens files + consolidated; all preserved at HEAD).

Binding authorities:

- `restart/prompts/skinny/PASS-1-PROFILE.md §3` (CH1-CH6
  specialisations) + `§6` (S-P2 dispatch gate).
- `restart/prompts/ORCHESTRATOR.md §3W` (universal CH1-CH6 lens
  registry) + `§3Z` (≥95 % × 2 cycles, zero orphan REVISEs
  convergence rule; V max=5 ceiling).
- `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md §CH7` (Overfit-Prune
  lens definition).
- `restart/prompts/pass-contracts/PASS-ALPHA.md §4.4` (contracted-
  deferral pattern verified verbatim in F-V2-P1F-1 framing block at
  `p1f-results-delta.md:183`).
- `restart/skinny/tranches/sk-v14/SYNTHESIS.md §2` Telemetry Binding
  (lines 232-258; 4 NEW column declarations: `track2_entry_point` at
  `:240`, `comparator_plane` at `:241`, `per_iter_equality` at `:242`,
  `audit_overlay_verdict` at `:255`) + `§3` C-2 row (line 272;
  R1+R2 wave deliverable).
- `restart/skinny/tranches/sk-v14/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md`
  (S-P0 prune list; 74 findings; PRUNE-1..PRUNE-7).
- `skinny/REDRESS.md` (5041 lines; REDRESS-126 anchors verified at
  `:3768`, `:3864`, `:3869` across V1 + V2 + V3; canonical path
  verified by `find` — `restart/skinny/REDRESS.md` does not and never
  has existed).

Source-code verification (HEAD-grep-validated by V3 lens reports per
CHALLENGE-CONTEXT §3 executable-verification mandate):

- `skinny/crates/bbnf-bench/src/generated_real_typed.rs` (3056 lines
  at V3 HEAD; 8 typed-plane grep hits at
  `:516/527/592/1150/1219/1330/2197/2949` — confirms F-V2-P1E-1
  refresh survives one full cycle).
- `skinny/crates/bbnf-simd/src/aarch64/{movemask, bulk_emit_positions_64,
  bitmap_prefix_xor_64, eob_pad_clamp}.rs` (fn at lines 4/2/2/4 at
  V3 HEAD — confirms F-V2-P1A-MOVEMASK + F-V2-P1C-LINEDRIFT
  refreshes survive).
- `skinny/crates/runtime/src/grammars/json/generated.rs` lines
  33-34, 43-44, 58-59, 79-80, 86-87, 117-118, 138-139, 157-158,
  185-186, 201-202, 211-212, 217-218, 236-237 (**14
  parse-attribution feature gates** at V3 HEAD; V1 saw 8, V2 saw
  11+, V3 confirms 14 — gate count growing across passes as
  bench-counters work landed).
- `skinny/crates/runtime/Cargo.toml:21` (`parse-attribution = []`
  feature declaration unchanged).
- `skinny/crates/bbnf-bench/Cargo.toml:18` (`runtime = { workspace =
  true, features = ["bench-counters"] }`; no `parse-attribution`
  propagation row — confirms F1 transitive form requirement).
- `skinny/crates/bbnf-bench/src/bin/xctrace_probe.rs` (xctrace_probe
  is a bbnf-bench bin, not a standalone crate; confirms V3 §3.1
  cite-hygiene observation).
- `skinny/Cargo.toml:78-86` (`[profile.release]` block; no
  `target-cpu` directive — confirms F-V2-METHODOLOGY-1 P1-A
  RUSTFLAGS-unset cohort correction holds at V3).
- `skinny/RESULTS.md` (185 lines at V3 HEAD;
  `grep -c "track2_entry_point|comparator_plane|per_iter_equality|
  audit_overlay_verdict" skinny/RESULTS.md` → 0; CH5-B
  contracted-deferral framing persists).
- `Cargo.toml:18` (`[workspace.metadata.bbnf]` `grammars` 9-element
  array at repo root; CH7 cargo-metadata census preserved).
- `skinny/REDRESS.md` (canonical path verified by `find` and `ls`;
  REDRESS-126 anchors at `:3768`, `:3864`, `:3869`).

Filesystem state observations:

- `/tmp/skv14-p1*/` wiped between V1 commit (`a3dfcaf38`,
  ~2026-05-23 02:00 UTC) and V3 confirming (~2026-05-23 evening
  UTC); already wiped at V2 confirming time per V2 §0.5 observation
  2. V1 CH6 §1.1-§1.4 path-existence evidence tables stand as
  binding record per CH6 V3 §1.1. Process-class observation;
  routed to LOCKS.md adoption (relocate sidecars to
  `restart/skinny/tranches/sk-v14/research/p1/artefacts/`).

Commit verification:

- Axis pass commit: `3510c1de5` (`docs(sk-v14-p1-profile): six-axis
  S-P1 V1 — atomic write-only commit`).
- V1 hardening atomic commit: `a3dfcaf38`
  (`docs(sk-v14-p1-hardening-V1): challenge V1 + consolidated`).
- V2 light micro-redispatch commit:
  `069ba203c413d46e7a5d465a128a983254e53841`
  (`docs(sk-v14-p1-profile): V2 light micro-redispatch — five orphan
  REVISEs landed`); 6 files changed, 86 insertions, 26 deletions.
- V2 hardening atomic commit: `4ad8f1949`
  (`docs(sk-v14-p1-hardening-V2): challenge V2 + consolidated`);
  8 files added (V2/CH1..CH7 + V2 consolidated); 2415 insertions;
  zero P1-axis file modifications.
- V3 hardening atomic commit: **this commit**
  (`docs(sk-v14-p1-hardening-V3): challenge V3 + consolidated —
  G-S-P1-CONVERGED`); 8 files added (V3/CH1..CH7 + V3 consolidated);
  zero P1-axis file modifications.

V2 → V3 substrate diff verification (across CH3 + CH5 + CH6 + CH7 V3
lens reports): `git diff 069ba203c 4ad8f1949 -- restart/skinny/
tranches/sk-v14/research/p1/p1{a,b,c,d,e,f}*.md` → **zero bytes**
across all six artefacts. The V3 confirming pass mathematically
guaranteed to reproduce V2 conclusions on every executable
verification, because the underlying files are byte-for-byte the same
as the V2 substrate.
