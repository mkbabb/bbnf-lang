---
title: "SC-5 — Is parse_only a permanent non-gate? K-classification adjudication"
tranche: sk-v8
phase: p2-substrate-ceiling
lens: substrate-ceiling
scope: SC-5
date: 2026-05-17
status: research (read-only adjudication; no code edits, no commit)
authority_inputs:
  - skinny/RESULTS.md
  - skinny/REDRESS.md (items 75, 77, 78; SK-V7 W0/W0b sections)
  - restart/skinny/tranches/sk-v7/SYNTHESIS.md
  - restart/skinny/tranches/sk-v8/SPEC.md (Section 0)
  - skinny/crates/bbnf-bench/src/gate.rs
  - skinny/crates/bbnf-bench/src/bin/gate.rs
  - skinny/crates/bbnf-bench/src/report.rs
  - skinny/crates/bbnf-bench/src/metadata.rs
verdict: "Option (c) — retire parse_only as a strict-vs-strict SOTA gate, replace with a plane-matched tape-vs-tape workload; K is partly an excuse"
---

# SC-5 — Is `parse_only` a permanent non-gate?

## §1 The K-classification catalogued (per-row, why K)

`skinny/RESULTS.md` carries 17 `parse_only` rows. Every single one is
`Outcome=K`, `Verdict=NO-GO`. The supporting columns are byte-identical
across all 17:

| Column | Value (all 17 rows) |
|---|---|
| `Strictness` | `deferred` |
| `parse_utf8` | `view-boundary` |
| `escape_complete` | `yes` |
| `flaw_probe` | `invalid UTF-8 rejected outside hot scan; lossy/permissive competitors are flaw probes` |
| `Output plane` | `borrowed view over offset tape vs DOM` |
| `Hot leaf` | `unprofiled in W0b; no kernel prescription from this row` |
| `Signal` | `NO-GO parse gate classified K` |

The throughput is *not* uniform — and that is the load-bearing fact. The
authoritative delta order in `skinny/RESULTS.md` is `Δ vs SK-V6`, `Δ vs
sonic-strict`, `Δ vs simdjson DOM`, then `Δ vs yyjson`. Read in that order, the
same-run sonic-strict comparator column spans both positive and negative
non-admission signals:

- **Positive same-run sonic comparator deltas, but not strict-admission
  evidence:** citm_catalog (+24.6%), canada (+27.9%), mesh (+21.4%),
  marine_ik (+37.0%), instruments (+10.6%), and numbers (+51.2%). These are
  guard telemetry only because the measured bbnf row is still
  `Strictness=deferred`, `parse_utf8=view-boundary`, and `Output plane=borrowed
  view over offset tape vs DOM`.
- **Negative same-run sonic comparator deltas:** twitter (-25.1%),
  apache_builds (-28.2%), github_events (-34.0%), update_center (-43.1%),
  random (-36.4%), gsoc-2018 (-53.3%), unicode_mixed (-50.3%),
  unicode_escapes (-34.6%), unicode_basic (-26.8%), distinct_values (-61.2%),
  and y_string_unicode (-54.1%).
- **Sidecar/historical planning signals, not same-run sonic-strict evidence:**
  C++ simdjson/yyjson/RapidJSON/asmjson deltas and SK-V6 deltas stay printed
  but cannot be called strict-admission evidence. For example, unicode_escapes
  is a sonic loss (-34.6%) despite the simdjson DOM sidecar delta (+113.6%);
  canada's +54.6% is simdjson DOM sidecar, not sonic; citm_catalog's -11.3% is
  simdjson DOM sidecar while +24.6% is the sonic column.

So K is applied **uniformly to rows with positive and negative throughput
signals alike**. K is not a throughput verdict. It is, per the row prose, a
declared *kind-mismatch*: bbnf `parse_only` produces a borrowed view over an
offset tape; sonic-rs / simdjson DOM rows produce a materialized DOM. The
RESULTS notes (§Notes, "lazy tape materialization" lines) reinforce this —
bbnf emits an offset/sparse-flag tape (0.05x-0.75x of input bytes, **0
payload bytes** on every corpus). It never builds value nodes.

### The mechanical truth of the K letter

The enum letter K is **not** native to "DOM-vs-view non-comparison". In
`skinny/crates/bbnf-bench/src/gate.rs:15` the variant is named
`KSimdParityHashFail`. `gate::classify` (gate.rs:118-126) returns K *only*
at:

```
if !input.simd_parity_ok { return Outcome::KSimdParityHashFail; }
```

and `simd_parity_ok` is computed in `src/bin/gate.rs:60-63` as
`scalar_hash == simd_hash` **AND** the SIMD-metadata sidecar hash matching
the scalar hash. The report builder (`report.rs:504`) proves the
`parse_only` workload *can* legitimately render `A / GO` — K is not wired
into the workload string. K is therefore a **repurposed enum letter**: an
outcome that originally meant "the scalar vs SIMD differential offset-hash
disagreed (or the SIMD sidecar is absent/stale)" is now carrying a
hand-authored `Signal="parse gate classified K"` narrative about output
planes. The W0b telemetry redress (REDRESS item 78, `0d2fab3f`) is what
authored the `Output plane` / `flaw_probe` prose; the letter itself comes
from the pre-existing gate path.

## §2 Is K correct, or an excuse? (evidence)

**It is both — and the parts must be separated.**

### What is genuinely correct about K

1. **The output-plane mismatch is real.** RESULTS' own materialization
   notes show every parse corpus emits `0 payload bytes`. bbnf `parse_only`
   produces offsets + sparse structural flags only; it never decodes
   numbers, never unescapes strings, never builds nodes. sonic-rs strict,
   simdjson DOM, yyjson, RapidJSON all return a navigable value tree with
   decoded scalars. Comparing 12285 Mbps of "emit offsets" against 21020
   Mbps of "emit a DOM" *is* a category error. A like-for-like strict gate
   cannot be built on unequal output contracts.
2. **The strictness plane is honestly deferred.** `parse_utf8=view-boundary`
   means bbnf validates UTF-8 at the borrow boundary, outside the hot scan;
   `Strictness=deferred` is disclosed, not hidden. SK-V7 W0 (REDRESS 77)
   genuinely repaired the sonic-rs comparator by removing `utf8_lossy`, and
   W0b marks the lossy column as a flaw probe. The strictness discipline is
   sound.

### What is an excuse hiding inside K

1. **K is a single letter doing two jobs.** The enum says "SIMD parity hash
   fail"; the row prose says "output-plane non-comparison". A reader cannot
   tell from the outcome whether the SIMD differential failed, the sidecar
   is missing, or the row is a deliberate non-gate. This conflation lets a
   *measurement-pipeline failure* (absent/stale SIMD sidecar hash → K) and a
   *philosophical non-comparison* (DOM vs view → also rendered K) wear the
   same badge. That is the excuse: the uniform K masks whichever of the two
   is actually firing per row.
2. **K hides at least one real loss as a "non-comparison".** twitter
   `parse_only` is -25.1% vs same-run sonic strict, -35.8% vs simdjson DOM
   sidecar, and -49.1% vs yyjson sidecar; the SK-V7 synthesis §2.6/§7 already
   names twitter parse as the "hard
   residual" with a yyjson 1.98x gap requiring a fusion-quality refactor.
   That is a substantive throughput loss. Filing it under K alongside
   canada (+27.9% vs same-run sonic, with separate +54.6% simdjson DOM and
   +36.6% yyjson sidecar deltas) tells the reader "incomparable" when the honest
   statement is "bbnf's offset-emit plane is slower than sonic's DOM-build
   plane on this corpus *even though bbnf does strictly less work*". The
   non-comparison framing is technically true but rhetorically convenient —
   it removes a loss from the scoreboard without resolving it.
3. **The "DOM vs view" framing is not symmetric.** A view-over-offset-tape
   is *strictly less* output than a DOM. If bbnf is slower while producing
   less, that is a worse result than a like-for-like loss, not an
   incomparable one. K presents an asymmetry that disfavors bbnf as if it
   were neutral incomparability.

**Adjudication of §2:** K is *correct* that `parse_only` cannot be a
strict-vs-strict SOTA gate against DOM builders. K is an *excuse* in that it
(a) overloads a SIMD-parity enum letter, and (b) launders real throughput
losses (twitter, the unicode/distinct family) into "non-comparison" status.
The classification's *conclusion* (not a valid SOTA gate) is right; its
*encoding* (uniform K, no loss disclosure) is misleading.

## §3 The adjudication (a / b / c)

Three options were weighed.

### Option (a) — bbnf builds a comparable DOM and competes plane-matched
**Reject.** This contradicts the entire skinny thesis. bbnf-skinny's value
proposition is the lazy offset tape with `0 payload bytes` — the GO rows
(`direct_to_struct`, `real_typed_struct`) win precisely *because* they
project structure directly into a typed digest/struct and never build an
intermediate DOM. Forcing a DOM build for parity would (i) introduce a
parser shape bbnf does not otherwise want, (ii) re-introduce the very
allocation/payload cost the substrate is designed to avoid, and (iii)
violate the SK-V8 Section 1 non-negotiables ("No new substrate without a
same-wave consumer"). Building a DOM only to lose to simdjson on simdjson's
own terms is strategically empty.

### Option (b) — retire `parse_only` entirely; GO target moves to `direct_to_struct` + `real_typed_struct`
**Partially accept, but insufficient alone.** It is correct that the SOTA
*product* claim surfaces are `direct_to_struct` (digest) and
`real_typed_struct` (typed) — those are the plane-honest candidates (Track 1
generated vs Track 2 independent hand parser / oracle), subject to the strict
measured-path admission predicates in §4. Retiring `parse_only` as a *SOTA
gate* is right. But fully *deleting* `parse_only` discards a genuinely useful
signal: it is the cleanest measurement of the **structural substrate itself**
— the offset-tape emit rate — independent of payload decode and typed
projection. SK-V8 SPEC Section 0.5 already keeps several parse rows as named
*guard rows* ("Guard row for any parser change", canada/mesh/numbers/
marine_ik/instruments). Deleting the workload would forfeit that guard.

### Option (c) — replace `parse_only` with a plane-matched workload; retain a renamed substrate-guard row — **RECOMMENDED**
The adjudication: **`parse_only` is not a permanent non-gate, but it is a
permanent non-*SOTA*-gate in its current form.** Recommendation:

1. **Demote `parse_only` from the SOTA scoreboard.** It stops being a
   GO/NO-GO contributor to the overall verdict. Its 17 rows become
   explicitly-labelled **substrate-guard rows** with their own outcome
   class (see §4) — not K, not A/G. Demotion does not permit deletion or
   down-sampling of residuals: every substrate-guard row must retain all
   same-run sonic comparator deltas, sidecar/historical deltas, and named
   residuals already visible in RESULTS, including twitter, unicode_basic,
   unicode_mixed, y_string_unicode, unicode_escapes, and distinct_values
   losses. Positive rows such as citm_catalog/canada/mesh/marine_ik/
   instruments/numbers remain visible as substrate-guard deltas, not strict
   SOTA-admission evidence.
2. **Introduce a new plane-matched telemetry row: `tape_vs_tape`** (working
   name). This is W0/W1-plan telemetry or gate-binding work, not a W3
   production consumer and not SOTA-admission evidence yet. The comparator
   is *not* a DOM builder; it is a structural-index producer on the same
   output plane: simdjson's *structural index stage* (the `stage 1`
   find-marks pass), or sonic-rs's lazy/`get` skeleton, or yyjson's
   `read-insitu` minimal mode. These produce an offset/index structure, not
   a full DOM. That is the only like-for-like comparator class for bbnf's
   offset tape, but `tape_vs_tape` cannot support SOTA admission until
   same-run structural-index competitor rows exist and publish strict
   deltas on the same corpus/profile run. The RESULTS note `canada
   structural scan: 69075 Mbps; floor is 40000 Mbps` is bbnf-only telemetry;
   it is encouraging, but not a gate.
3. **Keep `direct_to_struct` + `real_typed_struct` as the SOTA-claim
   surfaces.** They are the product-shaped rows to adjudicate once the §4
   `gate-json` strict-admission predicates hold. The overall verdict is driven
   by those admitted product rows, not by `parse_only`.

Net: option (c) = (b)'s demotion + a new plane-matched telemetry/gate-binding
row + preservation of the substrate-guard signal. This satisfies the
substrate-ceiling lens's actual question — *interrogate the substrate* —
because `tape_vs_tape` can measure the substrate ceiling directly against a
substrate-class competitor once same-run competitor rows exist.

## §4 Telemetry + goalset implications

The SK-V8 SPEC (Section 0.3) currently freezes the outcome enum to
`{A, C, G, K, L, N-direct}` and Section 0.4 requires the schema-v3 surface
plus the new W0 telemetry block. If option (c) is adopted, the following
schema changes are required and must be routed through a REDRESS amendment
(SPEC Section 0.3 explicitly permits enum amendment "in REDRESS and the
SPEC"):

1. **New outcome letter for substrate-guard rows.** Add an explicit
   non-SOTA-gate outcome — e.g. `S` ("substrate guard: maintain-only, not a
   SOTA gate") — so the 17 demoted `parse_only` rows stop wearing the
   overloaded `K`. `K` reverts to its honest single meaning
   (`KSimdParityHashFail`) and is reserved for an actual scalar-vs-SIMD
   differential failure. This removes the §2 conflation at the root.
2. **`Output plane` becomes a gate-eligibility key, not just prose.** The
   gate must read `comparator_plane` (already a required SK-V8 field per
   Section 0.4) and *refuse to compute a strict GO/NO-GO when the bbnf row
   plane and the comparator plane differ*. Today the plane mismatch is
   narrated in prose; it must become an executable gate predicate. This is
   a natural W1 CostFacts-gate-binding extension: the gate already consumes
   `comparator_plane`; it should now branch on it.
3. **Executable `gate-json` strict-admission refusal rules.** A row can enter
   strict admission only when all four predicates hold in the measured row:
   matching `comparator_plane`, `comparator_strictness` in the accepted strict
   set, same-run freshness for the comparator evidence, and strict validation
   performed inside the measured path. `gate-json` must refuse strict
   admission, not merely warn, when any predicate fails. `Strictness=deferred`,
   `parse_utf8=view-boundary`, stale sidecars, sidecar-only C++ evidence,
   historical SK-V6 evidence, and output-plane mismatch may remain printed as
   guard telemetry only. The refusal must apply before an A/G/GO SOTA verdict
   is computed, so a deferred parse row cannot pass by outsourcing validation,
   freshness, or comparator work outside the row being measured.
4. **New workload value `tape_vs_tape` as gate-binding telemetry.**
   `metadata.rs:418` maps workloads; a later plan may add `tape_vs_tape`
   alongside `parse_only`, with comparator columns restricted to
   structural-index-plane competitors (simdjson stage-1, sonic lazy
   skeleton). Its `comparator_plane` is `offset/index` on both sides. That
   makes the row eligible for future strict-vs-strict adjudication, but only
   after same-run structural-index competitor rows exist. Until then, it is
   W0/W1 telemetry/gate-binding and cannot support SOTA admission. It must not
   be counted as W3's production same-wave consumer.
5. **Close condition / goalset changes (SPEC Section 0.1 + 0.5):**
   - Section 0.1 item 7 ("Any parse/direct behavior wave meets its named
     row threshold...") is rewritten so `parse_only` rows are
     *maintain-only* (the existing ±1.0% W0 budget), never a *close*
     contributor. The overall verdict no longer counts `parse_only`.
   - Section 0.5's 17 `parse_only` rows keep their "Profile-bound;
     ±1.0 percent" W0 target but their "Later posture" column changes from
     "Candidate parse residual" to "Substrate-guard row; not a SOTA gate".
     The four already marked "Guard row" are unchanged in spirit. All
     comparator-delta columns remain printed with their anchors separated, and
     twitter/unicode/distinct losses stay named as residuals rather than
     disappearing behind `S`.
   - The SOTA close remains driven by `direct_to_struct` +
     `real_typed_struct` until `tape_vs_tape` has same-run
     structural-index competitor rows with strict deltas. `tape_vs_tape` is
     a candidate future plane-matched gate, not a current close
     contributor; `parse_only` is retired from SOTA-admission counting.
6. **W0 unaffected as a dispatch.** W0's job (Section 3) is telemetry lock,
   not behavior. The enum amendment and the `tape_vs_tape` row addition are
   *plan augmentations* for W1 gate-binding or an explicit W0 plan
   augmentation, not behavior smuggled into W0 and not a W3 production
   consumer. W0 itself should still capture all 17 `parse_only` rows as
   `SK-V8-open` baseline — they are not deleted, only reclassified later.

## §5 Interaction with the union substrate

The substrate-ceiling hypothesis posits a **union substrate**: the offset
tape ⊕ structural-projection. This is exactly the construct that makes
`parse_only` plane-comparable — and it reframes the adjudication.

- **The offset tape alone** is what `parse_only` measures today. Against a
  DOM it is incomparable (§1–§2). Against a *structural-index* competitor
  (the `tape_vs_tape` row of §3) it is comparable — because the offset tape
  *is* the structural-index half of the union.
- **The structural-projection half** is what `direct_to_struct` and
  `real_typed_struct` measure: structure projected straight into a digest
  or typed struct. Those are already plane-matched against sonic-rs strict
  (digest vs struct) and are the live product-claim surfaces, subject to the
  §4 strict-admission predicates.
- **The union** therefore does *not* make the present `parse_only` row
  plane-comparable to a DOM — a DOM is a *third* plane (materialized value
  tree) that neither half of the union produces. What the union does is
  explain *why* `parse_only` should be split, not patched: `parse_only`
  isolates the tape half; `direct_to_struct`/`real_typed_struct` isolate
  the projection half. The correct gate topology mirrors the substrate
  topology:
  - tape half → `tape_vs_tape` vs structural-index competitors;
  - projection half → `direct_to_struct` / `real_typed_struct` vs
    sonic-rs strict.
- This is the strongest argument for option (c) over (b): a *union*
  substrate has two measurable faces, and a healthy gate suite measures
  *both* faces against plane-matched competitors. Deleting `parse_only`
  (pure option b) blinds the suite to the tape face. Renaming it to a
  plane-matched `tape_vs_tape` telemetry row keeps the union fully observed
  and may later turn the substrate-ceiling question into a winnable one.
  The `canada structural scan: 69075 Mbps` figure already in RESULTS
  suggests the tape face is fast and the ceiling is high, but it cannot
  admit a SOTA claim without same-run structural-index competitor rows.

So: a union substrate does **not** make today's `parse_only` (tape vs DOM)
comparable. It makes a *correctly-planed* `tape_vs_tape` row the right
future comparator shape; until same-run structural-index competitor rows
exist, that row is telemetry/gate-binding work only.

## §6 Risks

1. **No same-run structural-index competitor exists yet.** simdjson stage-1
   and sonic lazy skeleton are not currently benched. Standing up
   `tape_vs_tape` is a possible W0/W1 gate-binding augmentation, now routed as
   residual rather than default SK-V8 scope: about 120-180 LOC across the
   comparator harness source
   `skinny/crates/bbnf-bench/benches/json_parity.rs`, workload metadata in
   `skinny/crates/bbnf-bench/src/metadata.rs`, and report/gate plumbing in
   `skinny/crates/bbnf-bench/src/report.rs`,
   `skinny/crates/bbnf-bench/src/gate.rs`, and
   `skinny/crates/bbnf-bench/src/bin/gate.rs` if the schema prints the new
   plane. Focused tests must include a workload-map test, a
   comparator-plane refusal test for DOM rows, a deferred/view-boundary
   strict-admission refusal test, a stale/sidecar-only comparator refusal
   test, and a same-run row test that rejects bbnf-only structural-scan
   numbers as SOTA evidence. The rerun budget is one allowed gate refresh
   after the harness lands. SK-V8 Section 1 forbids new substrate or behavior
   smuggled into W0; this work is telemetry/gate-binding only and cannot be
   counted as a W3 production consumer or W3 same-wave consumer. Because SPEC
   V4 does not assign this work to W0 or W1, a later plan must explicitly add
   owner files, LOC, focused tests, and rerun budget before it can consume wave
   scope.
2. **Enum amendment touches a frozen surface.** SPEC Section 0.3 freezes
   `{A,C,G,K,L,N-direct}`. Adding `S` (substrate-guard) requires a REDRESS
   entry + SPEC edit + `gate-json` reject-list update. If done sloppily it
   re-introduces exactly the kind of comparator drift REDRESS 75/77 warned
   about. It must be a deliberate, redress-recorded amendment.
3. **Demotion could be read as concealing real losses.** Moving
   `parse_only` off the SOTA scoreboard must *not* erase the twitter
   -35.8%/-49.1% residual, the unicode-family losses, or the
   distinct_values loss. The substrate-guard rows must still publish their
   strict Δ vs every competitor and carry named residuals for
   twitter/unicode/distinct rows; the `tape_vs_tape` row must surface the
   same corpora honestly once same-run structural-index competitors exist.
   If the demotion is used to quietly drop those losses, that repeats the
   §2 excuse at a larger scale. Mitigation: the substrate-guard outcome `S`
   must still carry full delta telemetry, and each named structural-scan
   residual must be routed rather than hidden.
4. **Risk of plane proliferation.** Three workloads (`tape_vs_tape`,
   `direct_to_struct`, `real_typed_struct`) each need a distinct comparator
   set. The schema is already 24+ columns; adding a fourth workload class
   risks an unreadable table. Mitigation: `tape_vs_tape` *replaces*
   `parse_only` in the scoreboard role rather than adding a fourth class —
   net workload count is unchanged.
5. **`canada structural scan 69075 Mbps` is a single number, not a gate.**
   It is encouraging but it is bbnf-only; it is not yet measured against a
   competitor's stage-1. The optimism in §3/§5 about a "winnable" gate is a
   hypothesis until the plane-matched competitor is benched. The W0
   profile-lock should be the moment this is confirmed or falsified.

## §7 Sources

- `skinny/RESULTS.md` — all 17 `parse_only` rows, columns `Outcome`,
  `Strictness`, `parse_utf8`, `escape_complete`, `flaw_probe`,
  `Output plane`, `Hot leaf`, `Signal`, the `Δ vs sonic-strict` /
  `Δ vs simdjson DOM` columns, and the §Notes `lazy tape materialization` /
  `canada structural scan: 69075 Mbps` lines.
- `skinny/REDRESS.md` — item 75 (comparator-plane correction; strict-anchor
  ineligibility of lossy rows), item 77 (SK-V7 W0 strict comparator repair),
  item 78 (SK-V7 W0b schema-v3 telemetry; authored the `Output plane` /
  `flaw_probe` prose; `parse_only` workload string).
- `restart/skinny/tranches/sk-v7/SYNTHESIS.md` — §1 strict-vs-strict
  comparator gate discipline, §2 post-V6 open gates, §3.1 W0 comparator
  repair, §3.6 twitter real_typed skip-work, §7 twitter parse hard
  residual, §8 SOTA-beat posture, §9 proposed comparator-plane disclosure
  lock.
- `restart/skinny/tranches/sk-v8/SPEC.md` — Section 0.1 global close
  condition, 0.2 comparator classes, 0.3 outcome enum freeze, 0.4 required
  telemetry, 0.5 opening row goalset (17 `parse_only` targets).
- `skinny/crates/bbnf-bench/src/gate.rs` — `Outcome::KSimdParityHashFail`
  (line 15), `classify` returning K on `!simd_parity_ok` (lines 118-126),
  verdict mapping K → NoGo (lines 68-72).
- `skinny/crates/bbnf-bench/src/bin/gate.rs` — `simd_parity_ok` computation
  (lines 60-63), `gate::classify` call site for `parse_only` rows
  (lines 68-83).
- `skinny/crates/bbnf-bench/src/report.rs` — `output_plane` field,
  `parse_only` workload string, `borrowed view over offset tape vs DOM`
  literal (lines 41, 78, 85), `parse_only | A | GO` test (line 504) proving
  K is not wired into the workload.
- `skinny/crates/bbnf-bench/src/metadata.rs` — `output_plane` /
  `borrowed view over offset tape` (lines 46, 148, 196-199), workload map
  default `parse_only` (line 418).
