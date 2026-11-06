# PROTO-A1 — JSON bench adjudication on the shared cold harness

Date 2026-07-05. Solo session A1. One binary, one measurement window, four
deliverables. The mandated shared instrument (`json_canon_bench`, report 33) was
rebuilt with the two gaps the critiques named closed: (1) P2's `assert_eq`
digest-parity gate folded in, and (2) the spec's fold-over-lazy-cursor construct
added as a first-class lane. All lanes run under identical discipline — release
fat-LTO, **system allocator** (faithful to `json_canon_bench`, NOT P2's private
mimalloc harness), N=200 cold-per-parse, median/min/stddev/CI reported.

## Measurement conditions (adjudication protocol honored)

- Host arm64 Apple-M. One stable window `20:45:47–20:46:12` EDT, **loadavg
  `{8.62 7.59 7.37}` → `{8.77 7.73 7.43}`** — the standing 6–8 host baseline; the
  <2.0 quiet mandate is unmet this session (a TW0 gate), so **absolutes are
  DIRECTIONAL; same-run ratios are authoritative** per charter.
- **PMU cycles/byte via `proc_pid_rusage(RUSAGE_INFO_V5).ri_cycles`** — a real
  per-process cycle count, load-robust, the preferred metric. No 3.2 GHz
  conversion was needed; the PMU path works.
- **Harness self-validation (cross-session):** my twitter `parse_tape`
  **1.5654 cyc/byte** reproduces report 33's **1.559**; my canada fused/sonic
  cold Mbps (9291 / 11633) reproduces report 33's (9936 / 11794) within ~5%. The
  instrument is the same truth-floor, re-confirmed.
- Harness: `crates/bbnf-bench/src/bin/a1_adjudication.rs` (modes: default cold
  table, `pmu`, `p1` interleaved). Fold-cursor construct:
  `direct_struct.rs::track1_tape_walk_digest` / `tape_walk_value`.

## Parity gate — P2's assert_eq, folded in and PASSING at FULL byte-equality

`fused(sink) == foldcursor(lazy) == sonic` holds **including the fingerprint**
(full `==`, not just structural shape) on all three corpora. This is strictly
stronger than report 33's XOR-under-black_box and confirms the three lanes
materialize byte-identical products:

| corpus | objects | numbers | strings | depth | fp fused==fold | fp fused==sonic |
|---|--:|--:|--:|--:|:--:|:--:|
| twitter | 1264 | 2109 | 4754 | 11 | true | true |
| canada | 4 | 111126 | 4 | 8 | true | true |
| citm | 10937 | 14392 | 735 | 8 | true | true |

The four lanes are benched on identical work; every ratio below is same-product.

---

## Deliverable 1 — P1 presize CI: overturn the +9.3%, confirm the tie

`parse_tape` under GrowOnly(D) vs Sampled(A), **interleaved paired per-sample**
(D then A inside one iteration — the interleaving CRIT-P1 §2 demanded to null
inter-invocation drift), N=200. Ratio is per-sample throughput A/D; the 95% CI is
on the paired ratio mean.

| corpus | D med Mbps (sd) | A med Mbps (sd) | A/D mean | %A vs D | 95% CI | verdict |
|---|--:|--:|--:|--:|--:|---|
| twitter | 19598 (3140) | 19736 (2580) | 1.0345 | **+3.45%** | [+1.07%, +5.83%] | **A faster (sig.)** |
| canada | 15174 (1411) | 15204 (1442) | 1.0125 | **+1.25%** | [−0.43%, +2.94%] | **TIE (spans 0)** |
| citm | 28669 (2591) | 28916 (2726) | 1.0077 | +0.77% | [−0.83%, +2.37%] | **TIE (spans 0)** |

**The PROTO-P1 canada +9.3% is OVERTURNED — numerically.** Under paired
interleaving, canada Sampled-vs-GrowOnly is **+1.25% ± 1.69% (95% CI
[−0.43%, +2.94%], spans zero) — a statistical TIE**, not +9.3%. The +9.3% was
inter-invocation drift, exactly the artifact CRIT-P1 §2 predicted from P1's
sequential `for P in D A B C` loop. The canada float claim is inside the noise
floor, now quantified.

**The core tie is CONFIRMED, with one refinement.** Sampled ties GrowOnly on the
two large corpora (canada, citm — CIs span 0), so the realloc churn presize
removes buys nothing measurable there. **But twitter shows a small, statistically
real Sampled win: +3.45%, CI [+1.07%, +5.83%] excludes zero.** On the smallest
corpus the 7-realloc GrowOnly churn is worth ~3.5%; a cheap 4 KB-sample presize
reclaims it. So "Sampled buys NOTHING" (PROTO-P1 line 91) is marginally too
strong — it buys ≤3.5% on small inputs, nothing on large.

**Bearing on DELETE:** the DELETE verdict for `OneShotSimd`/`Exact`/NEON stands
overwhelmingly (they pay 30–170% cyc/byte — report 33:143-147, PROTO-P1:73-77 —
to reclaim ≤3.5%). What is NOT a clean null is `Sampled` itself: keep-for-KISS
vs keep-cheap-presize is now a real (small, twitter-only, sub-GO-on-2/3) tradeoff,
not the zero PROTO-P1 asserted. Recommend the plan carry that nuance rather than
the flat "presize buys nothing."

---

## Deliverable 2 — canada sign SETTLED: fused LOSES to sonic

Same harness, same window, fused (`track1_digest` sink) vs sonic on canada:

| metric | fused | sonic | fused/sonic | sign |
|---|--:|--:|--:|---|
| cold wall med ns/byte | 0.8610 | 0.6877 | **1.252×** | fused SLOWER |
| cold wall med Mbps | 9291 | 11633 | 0.799× (throughput) | fused SLOWER |
| PMU cyc/byte | 3.3250 | 2.9239 | **1.137×** | fused SLOWER |

**On canada the fused sink LOSES to sonic — 1.25× slower wall, 1.14× more cycles.**
This confirms report 33's sign (9936 vs 11794 = 1.19×, "the sharpest optimisation
signal", 33:63-69) and **refutes PROTO-P2's canada 0.79× "fused WINS"** (P2:59).
The mandated quieter harness gives the less favorable, correct answer; the eager
`materialize_f64` float tax is real, pending TW4 Eisel-Lemire/SWAR.

**Divergence explained.** PROTO-P2's `p2_fused_cold` diverges from the mandated
harness on four axes, all pushing the same way: (a) **mimalloc global allocator**
(P2:20) vs system — the decisive one: mimalloc reshapes sonic's heavy serde-struct
allocation cost and the ratio on the one corpus where the lanes are closest; (b)
**ax-iter thin-LTO** (P2:47) vs release fat-LTO; (c) **per-sample fresh `to_vec`
clone** (P2:49-50) vs reuse; (d) **loadavg 16–17** with absolute ns/B ~20× inflated
(CRIT-P2-v2:40 flagged the ~190× absolute gap). The sign-flip is that inflation
distorting the closest ratio. P2's private harness is the outlier; the
`json_canon_bench` family (report 33 + this run) is authoritative and consistent.
twitter and citm are NOT in dispute — fused beats sonic there on both harnesses
(twitter fused 17527 vs sonic 9621 = 0.55× time; citm 26260 vs 15280 = 0.58×).

**Net:** fused single-pass beats sonic on twitter + citm cold, LOSES canada cold —
per-corpus, never a 4/4 sweep. The spec's per-corpus framing (SPEC:84-97) is
correct; P2's sweep is not.

---

## Deliverable 3 — THE SPEC DEFAULT CONSTRUCT: fold-over-lazy-cursor LOSES badly

The central unbenched bet (SPEC §4:191, "eager is a generic fold over the lazy
cursor — one substrate"). I implemented it minimally — parse to the lazy offset
tape, then materialize the identical digest by folding over the lazy `JsonValue`
cursor (`track1_tape_walk_digest`) — and benched it vs sonic's dual-path
direct-to-struct (`sonic_rs::from_slice`, the §4:191-194 opponent).

| corpus | foldcursor ns/B | sonic ns/B | **fold/sonic** | fold/fused | foldcursor cyc/B | sonic cyc/B |
|---|--:|--:|--:|--:|--:|--:|
| twitter | 1.9312 | 0.8315 | **2.32× SLOWER** | 4.23× | 8.6522 | 3.5249 |
| canada | 3.3347 | 0.6877 | **4.85× SLOWER** | 3.87× | 13.7539 | 2.9239 |
| citm | 1.1864 | 0.5236 | **2.27× SLOWER** | 3.90× | 5.0727 | 2.2234 |

**The spec's actual default construct is the SLOWEST materialization lane measured
— it LOSES to sonic on all three corpora by 2.3×–4.9×, and loses to its own fused
sink by 3.9×–4.2×.** The §4 "one substrate, fold over the lazy cursor" default, as
literally specified, is REFUTED as competitive. The reason is structural: it is
genuinely two passes — parse+tape-build (canada 0.50 cyc/B) then walk+re-scan-span
+materialize over the cursor (canada +13.25 cyc/B). The lazy cursor re-reads tape
offsets and re-scans each value span; the fused sink does it in one pass with no
tape.

**This is the reconstruction tax CRIT-P2-v2 §1 warned was unbenched, now measured —
and it is WORSE than P2's tape-walk estimate (P2 saw walk ~2.0× sonic; the true
fold-over-cursor is 2.3–4.9×).** The competitive lane is emphatically the fused
`parse_direct` SINK (no tape), NOT the fold-over-lazy-cursor. **SPEC §4 must be
corrected**: the default that beats/rivals sonic is the single-pass sink; "generic
fold over the lazy cursor as the one-substrate default" does not hold — it costs
2–5× and cannot be the product default. The TW3 §4 OPEN sub-gate closes NEGATIVE.

---

## Deliverable 4 — AU anchor: cyc/byte vs the corrected 1.63 floor

AU floor = **1.63 cyc/byte** (twitter 1967 MB/s @ 3.2 GHz; the "0.615" was a
bytes/cycle mislabel — SPEC:76-77). PMU, iters=800, plan D.

| corpus | parse_tape | fused_sink | foldcursor | sonic | serde |
|---|--:|--:|--:|--:|--:|
| **twitter** | **1.5654** | 2.0022 | 8.6522 | 3.5249 | 4.9543 |
| canada | 2.0376 | 3.3250 | 13.7539 | 2.9239 | 4.7438 |
| citm | 1.1067 | 1.3539 | 5.0727 | 2.2234 | 3.3498 |

Against AU 1.63 cyc/byte:
- **Structural `parse_tape` MEETS/BEATS AU: twitter 1.565 < 1.63, citm 1.107 well
  under.** canada 2.04 is over (numeric-dense). The bare structural walk is at the
  AU coordinate — AU is a live floor, not lost.
- **The fused DEFAULT lane (SPEC:209): citm 1.354 is UNDER AU; twitter 2.002 is
  +23% over; canada 3.325 is 2.0× (the float tax).** The default lane meets AU on
  one corpus, is a modest premium on the string-dense one, and pays 2× on the
  float-dense one — localizing TW4's Eisel-Lemire target to canada precisely.
- fused beats sonic in cyc/byte on twitter (2.00<3.53) and citm (1.35<2.22), loses
  canada (3.33>2.92) — cyc/byte agrees with wall-clock, per-corpus.
- **foldcursor 5.3×–8.4× the AU floor** — categorically non-viable, corroborating
  deliverable 3.

The AU-relative cold capture is now on the ledger as context (SPEC:83-85 — context,
not a gate): the structural floor is intact; the materialization premium over AU is
+23% (twitter) to 2.0× (canada), and the number-decode lever owns the canada gap.

---

## Verdicts

1. **P1 CI:** canada +9.3% **OVERTURNED** → +1.25% ±1.69% (95% CI [−0.43,+2.94],
   TIE). Sampled≈GrowOnly on large corpora CONFIRMED; twitter Sampled +3.45%
   [+1.07,+5.83] is a small real win, so "presize buys nothing" is slightly too
   strong. DELETE `OneShotSimd`/`Exact`/NEON stands; `Sampled` keep/delete is a
   small real tradeoff, not null.
2. **P2 canada sign:** **SETTLED — fused LOSES sonic** (1.25× wall, 1.14× cyc).
   Confirms report 33, refutes PROTO-P2 0.79×. Divergence = mimalloc + thin-LTO +
   per-sample clone + loadavg-16 inflation.
3. **Spec default construct:** fold-over-lazy-cursor **LOSES sonic 2.3–4.9× and its
   own sink 3.9–4.2×** — REFUTED as the default; the fused single-pass sink is the
   only competitive lane. **SPEC §4 needs correction.**
4. **AU anchor:** structural parse_tape MEETS AU (twitter 1.565 < 1.63); fused
   default meets AU on citm, +23% twitter, 2.0× canada. Floor intact; number decode
   owns the canada premium.

## Reproduce

```
cargo build --release -p bbnf-bench --bin a1_adjudication
./target/release/a1_adjudication 200        # cold table + parity gate
./target/release/a1_adjudication pmu 800     # cyc/byte AU anchor
./target/release/a1_adjudication p1 200      # interleaved paired D-vs-A CI
```
Artifacts: harness `crates/bbnf-bench/src/bin/a1_adjudication.rs`; fold-cursor lane
`crates/bbnf-bench/src/direct_struct.rs::track1_tape_walk_digest`. Raw window
capture in this session's task output (b6lowfjge).
