# SK-V18 S-P1 — HARDENING CONSOLIDATED (schema-free 3Z CHALLENGE close-out)

This ledger closes the S-P1 profile pass. It records the convergence posture of the
schema-free 7-lens CHALLENGE, the disposition of every REVISE folded back into the four
artifacts, the final G6 WIRE-or-RETIRE verdict, and the next move into S-P2. The four
governed artifacts are `a0-rebaseline-ledger.md`, `a1-json-hot-leaves.md`,
`a2-css-hot-leaves.md`, and `SYNTHESIS-PROFILE.md` (the binding ledger S-P2 consumes),
all under `restart/skinny/tranches/sk-v18/research/p1/`. The raw captures they are graded
against live under `.../p1/raw/` (`css_canon_n200.txt`, `css_sample.txt`, `json_sample.txt`,
`capture.log`).

## 1. Convergence posture (per-cycle r)

The CHALLENGE ran the 7-lens panel (CH1 MEASUREMENT-INTEGRITY, CH2 ADMIT-MECHANISM,
CH3 ACTUAL-PROFILING, CH4 G6-GATE-RIGOR, CH5 S-P2-HANDOFF-COMPLETENESS, CH6 OVERFIT-PRUNE,
CH7 HONESTY-CLOSURE) to schema-free convergence. The convergence predicate is
`r = accept/(accept+revise+reject) >= 0.95 AND reject == 0`, held for two consecutive
cycles; a cycle is VOID if fewer than 4 lenses return a parseable TALLY.

| cycle | posture | r | reject | consec after |
|-------|---------|------|--------|--------------|
| V1 | REVISE items raised; below threshold → FOLD applied in place | r < 0.95 | 0 | 0 |
| V2 | converged (r ≥ 0.95, reject = 0) | r ≥ 0.95 | 0 | 1 |
| V3 | converged (r ≥ 0.95, reject = 0) — second consecutive | r ≥ 0.95 | 0 | 2 → STOP |

**Final: converged = true, cycles = 3, consec = 2, voids = 0, reject = 0 across all cycles.**
No cycle was VOID (all 7 lenses returned TALLY every cycle); no REJECT was ever raised —
no claim in any artifact was found false, contrived, or fabricated. The only corrective
traffic was REVISE (precision/grounding tightening), all of which was folded after V1 before
the two clean cycles. Per-cycle TALLY lines and the exact r are emitted to the orchestration
log only (the FOLD task re-derives gaps in-memory and edits the markdown in place; it does
not persist per-lens verdicts to disk), so the on-disk evidence of convergence is the
corrected state of the four artifacts themselves, enumerated in §2.

## 2. Disposition of every REVISE folded

The V1 REVISE items were folded into the artifacts before V2/V3. Each is mechanism-correct,
single-edit, non-architectural, and grounded in the raw capture. Disposition:

| # | lens | gap raised | fix folded (now on disk) | status |
|---|------|------------|--------------------------|--------|
| 1 | CH1 / CH7 | `capture.log` header stamps `0fbee121f`, but the `track1_rich` workload did not exist at that SHA | A0 + SYNTHESIS corrected the provenance: header SHA is the S-P0 audit-convergence SHA only; the `track1_rich` rows came from the bit-rot-fixed `784ceb418` binary (verified `git cat-file -e 0fbee121f:…/css_canon_bench.rs` → absent) | FOLDED |
| 2 | CH1 | absolute Mbps risked being read as a re-locked baseline | A0/A2/SYNTHESIS each carry an explicit LOAD-HONESTY block citing `host_loadavg 4.35 6.03 5.70`; absolute Mbps declared DIRECTIONAL/depressed, load-bearing outputs narrowed to same-run ratios + hot-leaf rank | FOLDED |
| 3 | CH2 / CH7 | the bit-rot fix risked being read as a count-only relabel of the pruned `track1_fact_stream` | A0 + SYNTHESIS reframed `rich_summary` honestly as **full-value-materialization, lazy-rich** (9 typed fields re-derived from `(source, offset)` spans, writing nothing to the arena), equal-depth value work vs the lightningcss full CSSOM — closing residual R14/H1 | FOLDED |
| 4 | CH3 | the CSS sample could be misread as attributing the rich product directly | A2 added the "What the sample actually drove" caveat: the driver loops `parse_full` (recognizer) only, NOT `rich_summary`; the leaf ranking is recognizer-measured and bounds the rich scan cost by construction, not by direct sampling | FOLDED |
| 5 | CH3 | parser-share denominator needed an explicit basis | A2 fixed the denominator to 4379 (sum of parser leaves), with the `main` corpus-loop frame (1770, 28.8% total) reported but excluded; total-share computed against 6149 | FOLDED |
| 6 | CH4 | G6 verdict needed a stated numeric threshold and a retarget caveat | A2 + SYNTHESIS ground the WIRE verdict in the measured 94.1% scalar-scan parser-share vs the ~8% wire threshold, and add the sharp caveat that the dead NEON was written for a DIFFERENT/flatter function than the hot leaf and must be RETARGETED, not wired as-is | FOLDED |
| 7 | CH4 / CH6 | grammar-neutrality of the wired primitive needed to be explicit | A2 + SYNTHESIS record the Lock-14/G3 constraint: `find_component_delim` is replicated byte-identically across 7 `css_l4_*/generated.rs` files; the retargeted NEON must land as a shared grammar-neutral runtime primitive the generated scan CALLS, sequenced AFTER P3 collapse | FOLDED |
| 8 | CH5 | S-P2 handoff needed the concrete rich-projection symbols to sample | A2 + SYNTHESIS enumerate the projection surface S-P2 must sample directly (`rich_summary` :305, `nodes()` :307, `typed_value`→`classify` :201/:229, `selector_count`/`is_at_rule` :316/:309) with the note that `classify` + lazy re-walk dominate once the scalar scan is NEON-accelerated | FOLDED |
| 9 | CH6 / CH7 | R12 (`json/scan.rs`) disposition needed proof it is off the product path | A1 + SYNTHESIS record that `json/scan.rs` appears ZERO times in `json_sample.txt`; the direct path is scan-free; verdict CHEAP-TO-NEUTRALIZE, not a G5 target | FOLDED |

All nine REVISE items are folded; zero remain open. No REJECT was ever raised, so no claim
required retraction.

## 3. Final G6 WIRE-or-RETIRE verdict

**G6 = WIRE**, under dav1d discipline, with a mandatory retarget. Numeric basis (load-robust,
rank-invariant to host load): the CSS scalar scan is the #1 and #2 self-time leaves —
`find_component_delim` (79.5% parser-share) + `consume_balanced_at` (14.6%) = **94.1% combined
parser self-time**, far above the ~8% wire threshold. There is real, large headroom.

Three binding conditions on the WIRE (all on disk in A2 §"Verdict" and SYNTHESIS §3):

1. **Retarget, do not wire as-is.** The dead NEON kernels `runtime_simd::find_css_significant`
   and `find_comment_close` are confirmed `#[cfg(test)]`-only (R7 verified by inspection: sole
   callers are the parity guards in `runtime/src/lib.rs`). They were written for a flatter
   "stop at first delimiter" skip and do NOT cover the balanced-bracket/string/comment
   *consumption* the hot `find_component_delim`+`consume_balanced_at` machine performs. The
   NEON must be retargeted to the live `find_component_delim` flat-skip inner loop
   (`generated.rs:662–680`), with the balanced-consume recursion reusing the same inner skip.
2. **Grammar-neutral primitive, sequenced after P3.** `find_component_delim` is replicated
   byte-identically across 7 `css_l4_*/generated.rs` files. The retargeted NEON must land as a
   shared grammar-neutral runtime primitive the generated scan CALLS (the `runtime_simd`
   surface), NOT re-emitted per-grammar — else it re-forks the very shape G3 un-forks. The WIRE
   therefore consumes the P3-collapsed single CSS scan: collapse first, then wire the singular
   call site.
3. **dav1d discipline:** scalar reference FIRST, checkasm-style differential parity over the
   four corpora, aarch64 NEON only (x86 is a prune target).

**RETIRE is rejected** — deleting a kernel that targets a 94%-share hot path is the wrong
move. Retire only `find_comment_close` specifically, and only if retargeting to the
balanced-consume recursion later proves unsafe (comments are cold in these corpora).

The contrasting JSON disposition holds and is consistent: JSON G5 has **no hot leaf** (the
direct `track1_digest` path is scan-free; `json/scan.rs` is ZERO-sampled), so it is
cheap-to-neutralize, NOT a G5 target — do not author a JSON classifier. Two opposite
dispositions, both grounded in measured share.

## 4. Next move — ready-for-S-P2

The four artifacts are hardened and converged; `SYNTHESIS-PROFILE.md` is the binding,
profile-first ledger S-P2 consumes. The mandate carried forward:

- **JSON G1 MUST-preserve:** regenerate the fused, scan-free, monomorphized-sink byte-dispatch
  bodies (`parse_object_value_at_direct` + `parse_array_element_at_direct`, 91.5% combined);
  keep `unescape_string` Cow-borrow and `materialize_u64` integer fast paths. No structural
  pre-scan on this path.
- **CSS G6 WIRE:** retarget the NEON to the 94.1% scalar scan as a shared grammar-neutral
  primitive, sequenced after P3 collapse, under dav1d discipline.
- **S-P2 must sample `rich_summary` directly** to attribute the rich-projection plane (the
  recognizer-only sample did not surface `classify`/`nodes()`/`typed_value`); expect `classify`
  + the lazy `nodes()` re-walk to dominate once the scalar scan is NEON-accelerated.
- **Sequencing law:** S-P1 is a HARD dependency of G5/G6 — no orphan kernel may be authored
  without a profile-anchored hot leaf. Profile-first.
- **Open caveat (not a blocker):** absolute Mbps this pass is load-depressed
  (`loadavg 4.35/6.03/5.70`) and is NOT a re-locked baseline; a quiet re-capture is required
  before any absolute throughput claim. The >SOTA verdict (PASS on all 4 corpora, rich/lcss
  1.66×–3.38×) is load-robust and stands.

**Next move: ready-for-S-P2.**
