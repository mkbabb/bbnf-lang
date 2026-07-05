# Dig Shard 09 — The AY Tranche Arc (AY-I, AY-II-I, AY-III)

The parity tranche of the pre-restart campaign, in three passes. All three
ultimately failed or were deferred without executing their core waves; the
declared target — near-sonic-rs JSON parity on a single grammar-derived
substrate — was never met and was absorbed forward into the B-series and the
AZ tranches. This shard excavates what was attempted, what shipped versus what
died, the root causes of divergence, the work worth resurrecting, and the
meta-process pathologies that recurred here.

The single most important fact this shard establishes: **the historical
performance peak was reached at tranche AU (twitter 1967 MB/s, 0.615 cyc/byte,
0.91× sonic-rs) and every tranche since — AV, AW-I..V, AX, and all three AY
passes — was a regression from it.** AY was an eighteen-month attempt to climb
back to a summit the project had already stood on, and it never got there.

---

## A. What each pass attempted

### AY-I — "Canonical Packed Substrate" (parity tranche, pass I)

Charter (`docs/tranches/AY-I/AY-I.md:20-35`): close the post-AU architectural
drift by making the grammar-derived parser write **one canonical packed
substrate** directly, then drive the default eager JSON path to near-sonic-rs
parity. Explicit non-goals: no replay/recovery/incremental, no build-iteration
reform, no second runtime path. Thirteen invariants (`AY-I.md:60-79`) all bind
"one parser, one substrate, no orthogonal paths, no grammar-name
specialization, every substrate addition ships a same-wave consumer."

Nine waves W0–W8 (`AY-I.md:103-113`). Hard close gates (`AY-I.md:127-134`):
`twitter <= 1.15× sonic`, `canada <= 1.20`, `citm <= 1.20`, 5-fixture geomean
`<= 1.20`. The defensible floor (`AY-I.md:138-153`) was itself the full gate
set — "anything less is a tranche miss, not an acceptable floor."

A narrow prelude annex `B0` was sanctioned to remove command/build/bench drag
blocking W5 (`AY-I.md:28-31`).

### AY-II-I — "Fused Substrate + Full Grammar-Derived Semantic Parity" (pass II)

After AY-I stalled at W7, a four-agent audit triumvirate diagnosed the
remainder and a successor pass was authored
(`docs/tranches/AY-II-I/AY-II-I.md`). Its nine-point architectural thesis
(`AY-II-I.md:53-101`) sharpened the target: **ONE parse pass** constructing
tape AND `<Grammar>Value` in a single fused walk (`Parsed::to_value()` a thin
projector that does not reparse); **ONE compound emission API** (`push_compound`
retires, unified `begin/end_compound` over `Columns::rollback_to`); **ONE
stamping path** (`note_push` + `SIB_SKIP_STAMPED_BIT` retire, finaliser is sole
stamp source); **peer-first close gates** keyed to sonic-rs / simd-json /
lightningcss / cssparser rather than internal ratios; and CSS-L4 typed parity
vs lightningcss promoted to a hard gate. Waves W0 → W0' → W1(JSON) → W2(CSS) →
W3(Sheets) → W4(BBNF) → W5(close).

### AY-III — "Gestalt Continuation on the Post-B5 Substrate" (pass III)

After the B-series (B1→B3→B4→B2→B5→B6) rebuilt the substrate underneath AY-II-I,
a third pass was authored (`docs/tranches/AY-III/AY-III.md`) to close the AY arc
on the post-B5 `Tape<R>`-over-`Columns` substrate. It consolidated the AY-II-I
audit debt into 9 DROPS / 7 MERGES / 1 PROMOTE / 6 REFINES
(`AY-III.md:248-344`), compressed five waves into three, and set a **defensible
floor of twitter >= 1500 MB/s (<= 1.50× sonic)** with an escape clause routing
the remaining architectural lever to AZ-I.W2 (`AY-III.md:376-403`). A full
`AGENT_DISPATCH.md` with 13 templates (1258 lines) and three wave specs were
authored.

---

## B. What shipped vs what died on the vine

### AY-I — landed real infrastructure, missed every parity gate

Cross-checked against git (`git log --all -- docs/tranches/AY-I`: 45 commits
across the AY paths). Per the honest close ledger (`AY-I/FINAL.md`):

**Shipped (28 commits + 4 audit artefacts):**
- W0: seven stale test files retired; `dta.rs` carved 550→80 LOC; `shape_dict.rs`
  deleted; 1491 workspace tests green (`FINAL.md:22-28`).
- W1: reverted SoA columns back to flat `Vec<TapeRec>` + parallel `sib_skip`;
  Pratt Option C op-stack hoist; twitter recovered to 688 MB/s (`FINAL.md:30-36`).
- W6: **direct-to-struct admission broadened 2 → 71 grammar-derived projections**
  across JSON/CSS-L4/Sheets/BBNF via `ir.payload_layouts` + `RustNamedTypes`,
  with 69 `materialize_projection_*` helpers and a `PROJECTION_DIRECT_TO_STRUCT`
  associated const (`FINAL.md:71-84`). This is the tranche's durable asset.
- B0 annex closed cleanly (14 commits, `66a0f2cd`→`317cb394`): three-tier
  profile discipline, 10 `ay-*` Makefile gate targets, idempotent
  prepared-binary profiling workflow (`FINAL.md:187-190`). AUDIT-A scored B0
  4/5 — "a textbook prelude annex" (`AUDIT-A:57-69`).

**Died / failed:**
- **Every parity gate missed** by ~3.5–4.3× (`AY-I/FINAL.md:143-146`): twitter
  3.995×, canada ~3.07×, citm ~3.8×, geomean ~3.7× — against `<= 1.15/1.20`.
- The W5 write-time substrate experiment (`open_compound`/`close_compound`/
  `note_push`/`SIB_SKIP_STAMPED_BIT`/`open_stack`) **caused a cumulative
  W4→W6 regression of −27%** (746 → 616 → 548 MB/s) and was retired wholesale
  at AY-II/W0 (`FINAL.md:59-84, 166-172`).
- `runtime/path.rs::navigate_tape` shipped DEAD — zero production consumers
  (`FINAL.md:120, 142`; AUDIT-B verdict).
- W7 stalled outright on a rollback-invariant violation in `note_push`
  (`new_idx - prev` underflow across retry-IIFE `truncate` sites); the draft
  `prev < new_idx` band-aid was discarded as preserving the broken invariant
  (`FINAL.md:88-99`). W7 and W8 were superseded by AY-II.
- Bootstrap regen at W6 produced compile failures in gorgeous + bbnf-bootstrap
  from `@pretty` emission drift; `generated.rs` was frozen at W5-era state
  (`FINAL.md:82-84`).

### AY-II-I — one wave landed, then superseded and deferred

Status banner (`AY-II-I.md:8-11`): **"SUPERSEDED-BY-AY-III + DEFERRED —
2026-04-27. AY-II-I.W1-W5 was never executed."**

**Shipped:** W0 dispatched five parallel sub-agents (`PROGRESS.md:147-213`);
W0' collapsed into a `FusedBuilder` and its close ceremony was ultimately
**folded into B4.W1** (2026-04-25), where a 327-failure runtime-parser
regression was resolved (`PROGRESS.md:15-71`: pre-W1 1163/327 → post-W1
1480/10). The post-B5 substrate then dissolved the welded `FusedBuilder`
entirely into `Tape<R>` over `Columns` (`AY-II-I.md:182`).

**Died:** W1(JSON), W2(CSS), W3(Sheets), W4(BBNF), W5(close) never ran. The
pending-debt ledger (`AY-II-I.md:14-30`) names destinations for all eleven
un-landed items — every one routed to B4/B5, AZ-I, or AZ-II.

### AY-III — authored in full, never dispatched

Status banner (`AY-III.md:3-11`): **"DEFERRED — 2026-04-27 … no dispatch will
fire."** PROGRESS confirms (`AY-III/PROGRESS.md:250-266`): dispatch skipped
because AY-III verifies a *deprecating* substrate — AZ-I direct-to-struct
supersedes the tape projection lane for the three data grammars and AZ-II
deletes `crates/tape/` entirely. A complete plan, 13-template dispatch doc, and
three wave specs were written and committed but zero implementation commits
landed. This is the purest instance of the meta-pathology in §E: exhaustive
planning that outran, and never became, execution.

---

## C. Root causes of divergence — with evidence

### C1. AU was the peak; the substrate churn since was net-negative

`AUDIT-D` reconstructs the twitter trajectory from committed benchmark
artefacts (`AY-I/audit/AUDIT-D-perf-truth.md:17-28`):

| Checkpoint | MB/s | cyc/byte | vs sonic |
|---|---:|---:|---:|
| AU (`3b8b757d`) | **1967** | 1.626 | **0.91×** |
| post-AX-W1 (`6516086f`) | 448 | 7.126 | 0.21× |
| AY W1-fix | 688 | 4.645 | 0.32× |
| AY W4 | 746 | 4.288 | 0.35× |
| AY W5 | 616 | 5.191 | 0.29× |
| AY W6 | 548 | 5.838 | 0.25× |
| sonic | 2154 | 1.486 | 1.00× |

"AU's 1.626 cyc/byte was local max … CURRENT twitter is 3.62× worse than AU"
(`AUDIT-D:37-38`). Every JSON fixture regressed 27–32% from W4 and sits at
0.21–0.30× AU (`AUDIT-D:49-51`). The archaeology arc quantifies the AU→master
fall as a 4.5× regression on the most-tested fixture
(`AYW-archaeology-optimization-arc.md:644-648`).

The three AU invariants that eroded (`AYW-archaeology…:679-717`): (1) flat AoS
`Vec<TapeRec>` write path pivoted to a SoA 7-column layout at AV/AW-I — 7 Vec
pushes per record LLVM cannot fuse, "the single largest regression driver";
(2) unified `push_leaf_with(kind, PayloadData)` fragmented into a multi-column
`push_structural` cross-crate call that failed to inline (23–43% of self-time);
(3) `.map(|_| ())` elimination reversed into compound-wrap emission on scalar
leaves (100K scalars → 200K records where sonic-rs has none).

### C2. The gap was never a parse-speed floor — it was the tape-reconstruction default

The most consequential — and most under-exploited — finding of the whole arc:
**the emitted visitor lane already beats sonic-rs.** `AUDIT-D:70-85`:

| Fixture | bbnf_value / sonic | bbnf_visitor / sonic |
|---|---:|---:|
| data | 3.95× | **0.98× (beats)** |
| twitter | 4.00× | 1.12× |
| citm | 5.10× | **0.93× (beats)** |
| canada | 5.17× | 1.03× |
| data_xl | 3.56× | **0.91× (beats)** |

Visitor-lane geomean = **0.99× sonic — already inside the W8 gate**
(`AUDIT-D:84-85`). "The parser emits at near-sonic byte cost when the
tape-then-walk reconstruction is skipped. The gap is the default
`Parsed::to_value()` consumer still reconstructs from the generic tape"
(`AUDIT-D:143-151`). The internal critique reaches the same conclusion
independently: the default hot path is still parse-into-tape → finalize →
walk/materialize, "fundamentally different from … sonic-rs and simdjson, where
the hot representation is primary rather than reconstructed"
(`AY-critique-path-forward-2026-04-20.md:87-107`). This is why AY-II-I re-
centered on the fused single-pass thesis (§D2).

### C3. Substrate-without-consumer — the endemic anti-pattern, reproduced at 5/7 waves

The archaeology arc catalogs **14 substrate-without-consumer instances across
18 tranches** as the #1 chronic pattern (`AYW-archaeology…:413-457`), with AO
structural dispatch as the exemplar (built across 6 sub-phases, landed with the
activation flag hardcoded `false`, zero `scan_structural` calls at close). The
six-agent AY synthesis crystallizes it: "AY ships architectural substrate
faster than consumers can activate it … substrate-without-consumer is endemic"
(`AYW-SYNTHESIS.md:19-23`).

AUDIT-A scores AY's reproduction of this pattern **1/5**: five of seven closed
waves closed "with recorded misses," each a same-wave consumer gap
(`AUDIT-A:182-216`). "This is exactly the chronic pattern … *Emission lands;
consumer doesn't; hard gate closes on 'code exists'; runtime never fires.* AY
is reproducing it at 5/7" (`AUDIT-A:213-216`). The "complete with recorded
misses" wave-status value is a reviewer-unfalsifiable escape valve that the
SPEC does not recognize as a closed state (`AUDIT-A:228-245`).

Dead surfaces AY itself shipped (`AYW-SYNTHESIS.md:31-40`): `handle.rs` (142
LOC, zero emitted consumers), `phf.rs` (zero workspace callers), eager
`structural_scan` (regressed −64% twitter, retired same wave), `navigate_tape`
(0 consumers).

### C4. Spec-driven vision outran profile attribution

The approach-ratio analysis is the clearest structural lesson
(`AYW-archaeology…:599-638`): **profile-driven tranches (AP, AU) = 2/2 BEAT or
historical-peak, zero substrate-without-consumer landings; spec-driven tranches
accumulated 12 of 14 substrate-without-consumer instances.** "Spec-driven
tranches are where architectural vision outran profile attribution"
(`:615-616`). The AV→AW-V descent was "a 5-tranche spec-driven cascade where
each tranche's substrate assumed the next would activate it" (`:634-638`). AY
inherited three separate substrate regressions and asked nine waves to unwind
them while holding every invariant simultaneously — AUDIT-A: "internally
elegant and historically unsound … under-budgeted scope at least 2×"
(`AUDIT-A:8-18, 247-269`).

### C5. Overfitting to JSON — the CSS/Sheets panic that JSON-only closes masked

`AUDIT-D:153-201`: **two of four close-matrix benches PANIC at master HEAD** —
`css_l4/bootstrap` and `google_sheets/parse_nested` both index-out-of-bounds at
`crates/tape/src/columns.rs:409` inside `set_sib_skip_at`, reached from
`note_push`. Root cause: the W6 Pratt outer-compound retarget reopened a class
of retry-rollbacks under an open frame that Shape-1 JSON never exercises.
Critically: "**W5/W6 close artefacts never benched CSS or Sheets under `bench`**
… The W6 close ledger rationale ('not re-bench'd') masked this panic"
(`AUDIT-D:186-189`). Benching only JSON while the tranche claimed grammar
generality is the overfitting failure in concrete form.

### C6. God modules and quadruplicated structural facts

The structural-analysis-gap audit found an **8,114-LOC classifier with ~1,450
LOC retirable** (`AYW-SYNTHESIS.md:12-14`). The duplication ledger
(`AYW-SYNTHESIS.md:109-131`): FIRST-byte sets in **four** implementations;
`EClassFacts` in two (classify.rs's own comment acknowledges the
re-implementation); `is_operator_chain` in three; wrap-pattern detection in
four; alt-of-literal in four. "Each tranche added a pass that needed a
structural fact, and nobody plumbed the canonical source out. Every
accumulation is a divergence opportunity" (`:127-131`).

### C7. The e-graph was dead on arrival

**13 of 14 e-graph rewrite rules register zero fires on production grammars**
because the normalizer converges first — its canonical form already subsumes
what G1–G4 + CommonSuffixFactor + HIR-tier rules would rewrite
(`AYW-SYNTHESIS.md:26-30`). This is a load-bearing negative result: the e-graph
layer as built was redundant against the normalizer that ran before it.

### C8. Build walls and algorithmic blowups gated the dev loop

- **The 80-minute cold rustc IR-pipeline wall.** The `bbnf_derive` proc-macro
  ran the full IR pipeline inside rustc; retired only at B2 when `cargo xtask
  regen` became the canonical entrypoint and `crates/derive/` was deleted
  (`AY-II-I/PATH-FORWARD.md:29-34`).
- **The AY-II-I regen O(N²) blowup.** `value_end_compound` invoked recursive
  `subtree_size` on every compound close, turning each close into Θ(N) and the
  whole parse into Θ(N²); introduced at W0'.a `bd563c1d`, it stalled regen 15+
  minutes (`AY-II-I/audit/W0p-regen-root-cause.md:5-16`; PROGRESS:337-355).
  Fixed by an O(1) in-stack `direct_child_count` — but a *second* hot path of
  equal cost survived (`PROGRESS.md:356-387`).
- **The gorgeous dev-dep wall.** Six serial `#[derive(Parser)]` sites inside one
  rustc = 9:16 cold on every `cargo check -p bbnf --tests`
  (`PROGRESS.md:391-436`); resolved by a first-principles `iter-check` exclude
  surface (45× speedup, `PROGRESS.md:427-436`). Per the standing
  `build-infra-first` precept, this drag should have been removed before, not
  during, the parity waves.

---

## D. Work worth resurrecting for the totality plan

### D1. The AU load-bearing primitives — the proven-fast substrate shape

`AYW-archaeology…:650-677` catalogs exactly what made AU fast, and it is a
short, concrete list the totality plan should treat as a floor, not an
aspiration: flat AoS `Vec<TapeRec>` write path (one 16-byte store per push);
unified `push_leaf_with(kind, PayloadData)` with a 5-variant `PayloadData` enum;
per-grammar `push_fingerprint` capacity divisor (canada +18%, BBNF +49%);
`.map(|_| ())` elimination at codegen (309 sites → 0); `RegexClass::Numeric {
allow_leading_dot }` dispatch. AY.W1 partially restored (1) and (3) but the arc
never reassembled the full set with AU's discipline. "Restoring the substrate
without restoring the discipline is a necessary but not sufficient condition"
(`:715-717`).

### D2. Fused parse+value single-pass emission — the actual BEAT-sonic lever

The visitor-lane 0.99× evidence (C2) plus the synthesis verdict: "The path to
BEAT-sonic runs through **fused parse+value emission** — a single AST walk that
writes Value variants directly instead of tape-then-walk-tape … modelled after
sonic-rs's `Value::Number(f)` IS the node" (`AYW-SYNTHESIS.md:104-107, 165-169`).
AY-II-I made this its thesis (`AY-II-I.md:56-62`), the B5 substrate restoration
delivered the `Tape<R>` shape it needs, and AZ-I direct-to-struct is where it
was ultimately routed — but it was **never proven at the JSON close gate**. This
is the highest-ROI unfinished lever in the entire arc.

### D3. Unified cost-guided e-graph extraction — the #1 forward recommendation

The archaeology arc's primary BA recommendation (`AYW-archaeology…:783-829`):
fuse the eight separate analyses (recognizers, regex_info, structural_alphabet,
dispatch, fuse, inline, shape_dispatch, payload_layout) into **one cost-guided
e-graph extraction pass** where each analysis becomes a rewrite rule, costs
arbitrate, and extraction produces the IR the shape emitter consumes. "You
cannot mine data an extraction pass doesn't read" — this retires the
substrate-without-consumer class *structurally*, not by per-tranche discipline
(which demonstrably failed 14 times). Note the tension with C7: the e-graph as
built was dead against the normalizer, so this recommendation is specifically
to make the e-graph *the* decision surface (subsuming the normalizer), not to
add rewrite rules alongside it.

### D4. The 71 grammar-derived direct-to-struct projections

AY-I's one durable technical asset (`AY-I/FINAL.md:71-84, 154-160`): admission
broadened 2 → 71 across four grammars via `ir.payload_layouts` +
`RustNamedTypes`, with 69 materializers and the `PROJECTION_DIRECT_TO_STRUCT`
const pattern. The projection-totality invariant it motivated
(`PROJECTION_DIRECT_TO_STRUCT.len() == materializer count == consumer count`,
`AY-II-I.md:125-128`) is the right mechanical guard against substrate-without-
consumer for typed surfaces and should carry into the totality plan.

### D5. Payload-layout CSP admitting all Named types into a 16B hot slot

Second forward candidate (`AYW-archaeology…:831-839`): `TypeDesc::Named`
collapses pre-emit on every Rust-target grammar (`prune_unreachable` is the
culprit); a 16B aggregate slot admits all scalar-struct Named types, with
arena-backed widening for larger cases (33B color-function). This is the
concrete mechanism behind CSS-L4's persistent typed-parity gaps.

### D6. Emitter-side 4-lane reordered-unrolling reducer

Third candidate (`AYW-archaeology…:841-851`): AU.7.1's naive SoA reducer failed
its gate at 1.94×; AW-IV.W5.1 closed it at **6.57× on canada** via 4-lane
reordered unrolling (4 accumulators breaking strict-IEEE left-fold) — but only
as a microbench with no emitter consumer (`AYW-archaeology…:435-436`). The
emitter-side codegen (`Tape::reduce_column<C,R>` actually consumed per-grammar)
is the missing lever; read-side cache only, write side stays AoS.

### D7. Negative results that must not be re-learned

- **E-graph rewrite rules duplicate the normalizer** (13/14 dead, C7). Do not
  build an e-graph rewrite layer *behind* a converging normalizer.
- **Eager whole-input `structural_scan` regressed −64% on twitter**
  (`AYW-SYNTHESIS.md:38-40`). A structural pre-scan must be a costed, per-grammar
  activation decision, never an unconditional tax (this is exactly what the
  charter now demands via `STRUCTURAL_SCAN_POLICY`, which itself then shipped
  consumerless — the pattern is self-similar).
- **The SoA 7-column pivot was catastrophic** (−60–76%, `AYW-archaeology…:591`).
- **`pay_f64` column-splitting was negative information density** — it separated
  a column already present in `pay_wide`, creating a routing distinction with no
  consumer benefit; "a specialization is only good if it shortens a real
  consumer path" (`AY-critique-path-forward:409`).
- **Cost-model gates run against a substrate the cost model doesn't see produce
  null or misleading results** (AU.7.1 SoA gate on naive f64-sum; AW-IV.W5.3
  648-measurement sweep where `CALIBRATED_WEIGHTS == default`,
  `AYW-archaeology…:511-524`).

---

## E. Meta-process pathologies

### E1. Multi-pass tranche letters as a symptom of unclosed work

AY became AY-I, AY-II-I, then AY-III — three full plan documents, three PROGRESS
logs, an audit triumvirate, and a 13-template dispatch doc — of which **only the
first landed any implementation, and even it missed every gate.** Each pass
inherited the previous pass's unmet target and re-scoped it forward. AY-II-I's
core five waves never executed; AY-III never dispatched a single commit. The
pattern the archaeology arc names as "label collisions (mid-tranche pivot under
same letter)" (`:495-509`) here escalated into serial re-founding, and the real
work migrated entirely into the B-series and AZ tranches. The lesson for the
totality plan: **a pass that cannot close should not spawn a same-family
successor pass; the target either lands or is honestly abandoned.**

### E2. Documentation-discipline correlates with cleanness

`AYW-archaeology…:770-772`: "Cleanness-of-handling correlates with documentation
discipline. AU/AW-I/AW-II/AX all produce FINAL.md + bench + routed-ledger at
close. AM/AO/AP/AR/AS/AT all lack at least one." AY-I did produce an honest
FINAL with a routed hard-gate table (`AY-I/FINAL.md:137-149`) — that honesty is
why this dig is possible — but the honesty documented failure rather than
preventing it.

### E3. Deferral chains compound into un-closeable waves

AUDIT-A's terminal finding (`AUDIT-A:238-245`): AY's five closed-with-misses
waves produced a W7 that had to *simultaneously* reclaim a −27% bench, restore a
broken regen, land a shared-fact optimizer, and add a `note_push` bounds check —
"four deferrals landing in a single wave, which is exactly what … §Scope-reveal
protocol names as a new-letter trigger." Each soft-pass/recorded-miss was a
promissory note; W7 was where all of them came due at once and the wave broke.

### E4. Planning-without-execution (doc/code inversion)

AY-III is the extreme case: a complete, committed, internally-coherent plan
(`AY-III.md` 454 lines, `AGENT_DISPATCH.md` 1258 lines, three wave specs) with
**zero implementation commits**, deferred before dispatch because the substrate
it was written to verify was already being deleted underneath it
(`AY-III/PROGRESS.md:250-266`). The plan was authored faster than reality could
be established under it — the doc/code inversion the restart was called to end.

### E5. The runway was built and not used

B0 delivered a textbook profiling runway (AUDIT-A 4/5), yet "AY.W5's regression
could have been diagnosed in 10 minutes with `make ay-samply-json-twitter`; W5
admits 'not yet diagnosed with samply' … B0 worked; AY did not pick up the
tools" (`AUDIT-A:70-78`). Samply was deferred continuously from W3 onward
(`AUDIT-A:218-227`). The single discipline that distinguished the two winning
tranches (AP, AU) from the losing cascade — profile-first, prove activation
before adding surface — was available and skipped.

---

## F. Bottom line for SK-V21

1. **The summit exists and its coordinates are recorded.** AU hit 0.91× sonic
   with a short, concrete primitive set (§D1). The totality plan's JSON floor
   should be AU-relative, and the AU primitives are the starting substrate, not
   a research question.
2. **The BEAT-sonic lever is proven and unfinished.** The emitted visitor lane
   is already 0.99× sonic (§C2); fused parse+value single-pass (§D2) is the way
   to make it the default. Everything else in the arc was churn around this one
   unshipped idea.
3. **The failure mode is structural, and there is a structural fix.** 14/14
   substrate-without-consumer instances were not prevented by discipline;
   unified cost-guided e-graph extraction (§D3) makes the failure mode
   inexpressible. Per-wave projection-totality assertions (§D4) are the
   mechanical guard in the interim.
4. **Profile-driven beats spec-driven, empirically 2/2 vs 12/14** (§C4). Bench
   every grammar every wave (the CSS/Sheets panic in §C5 is what JSON-only
   closes hide); prove runtime activation with samply before adding surface.
5. **Do not re-learn the negatives in §D7** — dead e-graph-behind-normalizer,
   eager structural scan, SoA 7-column, column-splitting with no consumer,
   cost-gates the cost model can't see.
6. **Multi-pass re-founding is the anti-pattern to avoid** (§E1, §E4). A plan
   authored faster than it can be executed, deferred before dispatch, onto a
   substrate being deleted underneath it, is the exact divergence the restart
   answers. The convergence loop's discipline — prototypes as analysis
   instruments that never land until convergence — is the correct inversion.
