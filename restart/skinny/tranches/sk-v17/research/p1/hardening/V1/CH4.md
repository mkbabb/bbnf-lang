# SK-V17 S-P1 CHALLENGE — CH4 COST (V1)

Lens: CH4 COST. Cycle: V1. Date: 2026-05-29.
Charter (dispatched): every named hot leaf carries a measured % self-time + the
candidate primitive it grounds; no speculative kernel without a profiled
antecedent. (This is the COST/hot-leaf-grounding charter the orchestrator
dispatched; it is distinct from PASS-1-PROFILE §3's CH4 "reproducibility" text —
where reproducibility bears on whether a cost figure is real, it is folded in
below and tagged `[repro-as-cost]`.)
Subject: SK-V17 S-P1 PROFILE artefacts `restart/skinny/tranches/sk-v17/research/p1/{p1a..p1f}.md`.
Baseline verified against source at master HEAD `6496fecae`.
Output: this file.

Disposition vocabulary: ACCEPT / REVISE / REJECT. One row per artefact §-section.

---

## §0 — Source verification performed (the cost-grounding floor)

Every hot-leaf symbol cited across the six artefacts was resolved against the
benched tree before dispositioning. All resolve exactly:

| Cited symbol | Cited line | Verified line | Status |
|---|---|---|---|
| `emit_fact_stream` | generated.rs:5 | :5 | OK |
| `emit_full_parse` | generated.rs:61 | :61 | OK |
| `parse_stylesheet` | generated.rs:118 | :118 | OK |
| `parse_at_rule` | generated.rs:137 | :137 | OK |
| `parse_block` | generated.rs:189 | :189 | OK |
| `parse_block_item` | generated.rs:209 | :209 | OK |
| `parse_declaration` | generated.rs:242 | :242 | OK |
| `skip_ws_comments` | generated.rs:263 | :263 | OK |
| `find_component_delim` | generated.rs:288 | :288 | OK |
| `find_colon_before` | generated.rs:313 | :313 | OK (see REVISE-D2) |
| `consume_balanced_at` | generated.rs:320 | :320 | OK |
| `consume_comment_at` | generated.rs:342 | :342 | OK |
| `consume_string_at` | generated.rs:353 | :353 | OK |
| `fnv64` | generated.rs:619 | :619 | OK |
| `push_ascii_lower_hex` | generated.rs:628 | :628 | OK |
| hot inner `delimiters.contains(&byte)` | generated.rs:295 | :295 | OK |
| `pos = match byte` dispatch | generated.rs:298 | :298 | OK |
| `_ => pos + 1` | generated.rs:307 | :307 | OK |
| `TapeBuilder` | assembler.rs:42 | :42 | OK |
| `push_plain_offset` | assembler.rs:71 | :71 | OK |
| `select_classifier` | dispatch.rs:42 | :42 | OK |
| `lo6_table_admissible` | dispatch.rs:101 | :101 | OK |
| `parse_4_digits_dotprod` (udot orphan) | digit_mac.rs:27 | :27 | OK |

The triple-overlapping-scan claim (P1-D §2.5) is structurally true: `parse_block_item`
calls `find_component_delim(b"{};")` at :211, then `find_colon_before` (:219) which
re-scans via `find_component_delim(start, b":{};")` at :314, then `parse_declaration`
re-scans the value via `find_component_delim(colon+1, b";}")` at :247 — three walks
of each declaration body. This is a genuine, source-grounded structural-cost finding.

**Cost-grounding verdict on the named leaves:** the FIVE benched-CSS hot leaves —
`find_component_delim`, `consume_balanced_at` (recognition plane); `emit_fact_stream`,
`push_ascii_lower_hex`, and the syscall+malloc alloc floor (fact-stream plane) —
EACH carry a measured % self-time on >=3 independent profiles and EACH ground a
named candidate primitive (NEON byte-class scan via `select_classifier`; tape append
via `push_plain_offset`). No leaf is named without a number; no proposed primitive
lacks a profiled antecedent. The COST floor is met. The defects below are about the
PROVENANCE and TRUTH of specific cost numbers, not about missing grounding.

---

## §1 — Per-artefact dispositions

### P1-A `p1a-samply-mode-1.md`

| § | Disposition | Basis |
|---|---|---|
| §1 method | ACCEPT | Commands verbatim; planes named to symbol. |
| §2.1 throughput table | ACCEPT | Mbps median/min/max/stddev real (wall `Instant`). |
| §2.1 **`c/B` column** | **REVISE (CH4-1)** | The `c/B` column (animate full 14.70, fact 50.70, …) is derived from `proc_pid_rusage` `ri_cycles` (§frontmatter:10, §2.1:67). P1-F §2.2/§4.5 falsifies *this same counter* as reporting physically-impossible CPI 0.16–0.28 (sub-1.0 retired-instruction CPI is impossible) and declares it "NOT trustworthy"; P1-D §3 independently abandons `ri_cycles` and re-derives c/B from wall-time. P1-A presents `ri_cycles` c/B as a measured cost figure with no caveat. A cost number from a counter two sibling agents discredit is ungrounded cost. **Fix:** mark the c/B column `⚠ ri_cycles unreliable (see P1-F §2.2 / P1-D §3 — reference-clock tick, not retired cycles)`, OR replace with instr/byte (P1-F shows instr retirement IS reliably counted), OR delete the column. Do not present it as a clean cost. |
| §2.2 recognition hot leaves | ACCEPT | `find_component_delim` 58.41/65.05%, `consume_balanced_at` 10.79/0.15%, etc.; all carry %self-time + file:line; grounds the NEON byte-class primitive. |
| §2.3 fact-stream hot leaves | ACCEPT | Alloc family attributed; the `mach_absolute_time`←`0x2b483`∈libmalloc caller-walk is a sound cost-attribution (25591/25640 samples), correctly reclassed alloc-family not timer. Grounds `push_plain_offset` tape append. |
| §2.4 plane-shape note | ACCEPT | Names the no-rich-typed-CSS-parser gap; no cost claim made. |
| §3 delta | ACCEPT | Honest "no prior typed row"; deltas vs plane references. |
| §4 anomalies | ACCEPT | Candidate primitive `byte_class_index_64`/`to_bitmask64` named as *to-build* targets grounded in the 58–65% leaf; gated behind tape (not speculative — antecedent-grounded). consume_balanced_at corpus-dependence is a real cost nuance. No orphan kernel. |

### P1-B `p1b-samply-mode-2.md`

| § | Disposition | Basis |
|---|---|---|
| §1 method | ACCEPT | atos post-symbolication path stated; planes to symbol. |
| §2.1 throughput + **`c/B` column** | **REVISE (CH4-1)** | Same defect as P1-A: §2.1 `c/B` (50.52, 44.39, 69.20, …) from `ri_cycles` (§2.1:87), presented as a measured cost, contradicted by P1-F/P1-D. §2.3 then reasons FROM these c/B ("~3x of the fact-stream cycles are the String building", :239) — building a cost conclusion on a discredited counter. **Fix:** same as CH4-1; if the "~3x String-building cycles" conclusion is retained it must be re-grounded on instr/byte (P1-F §2.2 shows fact_stream 234–364 i/B vs full_parse 46–58 i/B ≈ 4.4×, which actually SUPPORTS the conclusion on a reliable counter — cite that instead). |
| §2.2 fact-stream hot leaves | ACCEPT | kernel 31.47/malloc 26.41/`emit_fact_stream` 23.80/`push_ascii_lower_hex` 8.98%; each %self-time + file:line; alloc floor grounds tape append. |
| §2.3 recognition hot leaves | ACCEPT | `find_component_delim` 56.55%, `consume_balanced_at` 11.51%; grounds NEON primitive; re-confirm obligation discharged. |
| §2.4 harness note | ACCEPT (see CH4-3) | |
| §3 delta | ACCEPT | Plane reconciliation sound. |
| §4 anomalies | ACCEPT | Masking-shift logic (scan unmasked only after tape kills alloc) is a correct cost-sequencing observation. `push_ascii_lower_hex` per-token `Vec::with_capacity` named as the per-leaf alloc the lazy projection retires — grounded. No orphan kernel. |

### P1-C `p1c-samply-mode-3.md`

| § | Disposition | Basis |
|---|---|---|
| §1 method | ACCEPT | `--no-open` (not `--save-only`) + offline atos; resource-bucketing-only for syslib stated honestly. |
| §2.1 throughput | ACCEPT | Mbps only; no `ri_cycles` c/B column → no CH4-1 exposure. |
| §2.2 structural plane | ACCEPT | Mbps; recognition beats lcss 2.2–3.2× correctly framed as recognition-only. |
| §2.3 fact-stream hot leaves | **REVISE (CH4-2)** | Resource bucket "35.52% css_cold_bench (own code)" then own-code leaves are split into `emit_fact_stream` at THREE different lines (`:45` 17.30%, `:26` 8.59%, classed "hash") and `push_ascii_lower_hex` at `:633/:631/:630`. The `generated.rs:45` and `:26` attributions are line-level claims INSIDE `emit_fact_stream` (which spans :5–:60), but the function header is :5; A/B/E/F attribute the whole function at `:5`. The per-line split is finer-grained and plausible, but `:26` is classed "hash (`push_hex64(&mut out, fnv64(...))`)" — verify :26 is actually the fnv64 call site (the body was not line-verified by this lens at :26; :5 header is verified). **Fix:** either confirm the `:26`/`:45` intra-function line attributions against the source (cost at a sub-function line must resolve to that line's operation) or collapse to the `:5` function-level attribution the other four agents use, so the cost is consistently grounded. The %self-time numbers themselves are accepted; only the sub-line precision needs verification or collapse. |
| §2.4 recognition hot leaves | ACCEPT | `find_component_delim` 58.11%, `consume_balanced_at` 10.51%; line-range citations (:288,294,295,296,298,307,311) all within the verified :288–:311 body. Grounds NEON primitive. |
| §3 delta | ACCEPT | |
| §4 anomalies (A1–A5) | ACCEPT | A3 FNV-in-hot-path (8.59%) correctly flagged as diagnostic cost that vanishes with tape (no kernel proposed). A5 NEON antecedent gated behind tape. No orphan kernel. |

### P1-D `p1d-pmu-cycles.md`

| § | Disposition | Basis |
|---|---|---|
| §1 method | ACCEPT | Retired-harness critique (W6 `measure_mbps`, W8 broadcast) accurate. |
| §2.1/§2.2 throughput | ACCEPT | Mbps median/min/max/stddev; run-2 concurrency contamination disclosed honestly. |
| §2.3 plane caveat | ACCEPT | The "recognition beats lcss is WRONG-PLANE" caveat is the load-bearing cost-honesty statement of the whole pass; prevents the recognition number being read as a typed result. |
| §2.4 hot-leaf attribution | ACCEPT | `find_component_delim` 81.28% parse-only / 58.71% all; intra-leaf line split (:298 30.40%, :295 17.07%, :307 3.46%) — each line verified against the :288–:311 body and resolves to the stated operation (`:295` IS `delimiters.contains`, `:298` IS `pos = match byte`, `:307` IS `_ => pos+1`). This is the BEST-grounded hot-leaf cost decomposition in the pass. Grounds NEON primitive precisely (the `:295` `slice::contains` + `:298` per-byte match is exactly what movemask replaces). |
| §2.5 redundant re-scan | ACCEPT | Triple-scan structurally verified (§0 above); a real grammar-neutral cost target, not a kernel. |
| §3 **c/B derivation** | **REVISE (CH4-1, partial-credit)** | P1-D §3 HONESTLY abandons `ri_cycles` and states the c/B is wall-time-derived (~13.3 c/B at nominal 4.0 GHz). This is the correct posture, BUT it bakes a NOMINAL clock (4.0 GHz) into a cost figure and labels the result "c/B" alongside the others — a reader cannot tell P1-A's `ri_cycles` 14.70 from P1-D's wall-derived 13.3 are different provenances. **Fix:** P1-D's §3 is the model the others should follow, but it must state the assumed clock is nominal-not-measured and that wall-derived c/B is a proxy, not a counter (it half-does this at §3 close; make it unambiguous and cross-reference P1-F's CPI falsification as the reason `ri_cycles` was abandoned). Light REVISE — the provenance is honest but the label collides with the unreliable A/B figures. |
| §4 anomalies | ACCEPT | §4.5 PMU-c/B-gap correctly flagged `[repro-as-cost]`; §4.7 zero-SIMD confirmed by symbol-table absence in 20,900 samples (a real negative-evidence cost claim). No orphan kernel; the udot orphan is named as never-reached, not proposed. |

### P1-E `p1e-hot-leaf-attribution.md`

| § | Disposition | Basis |
|---|---|---|
| §1 method | ACCEPT | Caller-walk over `stackTable.prefix` to nearest binary frame is a sound syslib cost-attribution method. |
| §2.1 throughput | ACCEPT | Mbps only; no `ri_cycles` c/B → no CH4-1 exposure. |
| §2.2 delta-vs-lcss | ACCEPT | Per-corpus ratios grounded in the §2.1 medians. |
| §2.3 recognition hot leaves | ACCEPT | 56.52/11.05%; grounds NEON. |
| §2.4 fact-stream hot leaves | ACCEPT | Syslib-caller attribution (91.44% of syslib reached FROM `emit_fact_stream`) is the single most decisive cost-grounding in the pass: it ties the 57.6% alloc floor causally to the named leaf, so the tape-append primitive is grounded not just by co-occurrence but by causal caller-attribution. |
| §2.5 classification roll-up | ACCEPT | The mandated per-leaf symbol+%self-time+file:line+class table is complete and load-bearing. |
| §3 delta | ACCEPT | |
| §4 anomalies | **ACCEPT — exemplary (CH4 charter discharge)** | §4.4 is the CH4-critical section: it explicitly ORPHAN-BLOCKS the udot/i8mm digit kernel (`digit_mac.rs:27`, C4b) for lack of any CSS digit-parse self-time ("zero digit-parse self-time in CSS recognition … C4b stays orphan-blocked"). This is precisely the "no speculative kernel without a profiled antecedent" charter: a kernel that COULD be proposed is correctly refused for want of a measured leaf, and the condition for its future admission (re-profile the typed lazy-`ValueRef` path after W1/W2) is named. No speculative kernel survives. |

### P1-F `p1f-bench-canonical.md`

| § | Disposition | Basis |
|---|---|---|
| §1 method | ACCEPT | |
| §2.1 throughput | ACCEPT | N=200 medians; per-corpus delta-vs-lcss grounded. |
| §2.2 **PMU instr/byte + `ri_cycles` falsification** | **ACCEPT — load-bearing (CH4-1 root)** | This is the section that FALSIFIES the `ri_cycles` counter (CPI 0.16–0.28 impossible) and pivots to instr/byte as the reliable cost density (full_parse 46–58 i/B, fact_stream 234–364 i/B, ≈4.4× — the String tax, on a reliable counter). The raw cyc/byte is marked `⚠ UNRELIABLE` on every row. This is the correct cost-grounding the A/B agents failed. P1-F is the authority CH4-1 instructs A/B to defer to. |
| §2.3 hot-leaf attribution (3 planes) | ACCEPT | `find_component_delim` 59.24%, `consume_balanced_at` 10.31% (recognition); fact-stream alloc floor 64% + `emit_fact_stream` 25.01% + `push_ascii_lower_hex` 8.98%; AND the lightningcss-plane attribution (cssparser tokenizer ~38% + typed-node build/drop ~30%) — the only artefact that profiles the comparator to PROVE it genuinely materializes the CSSOM (grounds the fairness of the >SOTA bar as a cost claim). |
| §3 delta | ACCEPT | W8R broadcast tuple reproduced per-corpus; "~70 Mbps/~14×" correctly classified N-direct (no fresh antecedent), eager-typed 3 Mbps classified K (pre-blocked). The outcome enum table is the cleanest cost-classification in the pass. |
| §4 anomalies | ACCEPT | §4.5 re-states the `ri_cycles` falsification; §4.3 NEON antecedent gated; no orphan kernel. |

---

## §2 — Cross-artefact COST findings (the consolidated REVISE set)

**CH4-1 (REVISE — the load-bearing cost defect; orphan-prevention: must fold).**
The `c/B` (cycles-per-byte) figure is presented as a measured cost number in
**P1-A §2.1, P1-B §2.1/§2.3, P1-D §3**, all sourced from `proc_pid_rusage`
`ri_cycles`. **P1-F §2.2/§4.5 and P1-D §4.5 both falsify this counter** as
reporting physically-impossible sub-1.0 CPI (a reference-clock tick, not retired
core cycles). So the pass contains a contradiction: three artefacts use a counter
two artefacts discredit, and one of those three (P1-D §3) also abandons it but
still prints a wall-time figure under the same `c/B` label. Per the CH4 COST
charter — every cost figure attached to a hot leaf must be a real measured number
— the A/B/D `c/B` columns are ungrounded or ambiguous cost. **Required fold (V2):**
adopt P1-F's resolution as the pass-wide cost-density metric — **instr/byte from
`ri_instructions` (reliable) is the grounded cost density; `ri_cycles` c/B is
struck or marked `⚠ unreliable` everywhere it appears (P1-A §2.1 col, P1-B §2.1
col + §2.3 conclusion, P1-D §3 label).** P1-B §2.3's "~3× String-building cycles"
conclusion is RE-GROUNDABLE on instr/byte (P1-F's 4.4× i/B ratio supports it) and
should cite that. This is a single consistent fix across four artefacts; it is the
one finding the orchestrator must not leave orphaned.

**CH4-2 (REVISE — sub-function line attribution).** P1-C §2.3 attributes
`emit_fact_stream` cost at intra-function lines `:45` and `:26` (the latter classed
"hash/fnv64"), finer than the `:5` function-level attribution A/B/E/F use. Cost at a
sub-function line must resolve to that line's operation; `:5` is the verified header,
the `:26`/`:45` intra-body lines were not line-verified by this lens. Either confirm
those line attributions against the source or collapse to `:5`. (The %self-time
magnitudes are accepted; only the line precision is at issue.)

**CH4-3 (REVISE — harness multiplicity, `[repro-as-cost]`).** FIVE distinct
canonical-harness binaries were authored this pass, one per agent:
`css_cold_harness.rs` (P1-A), `css_cold_bench.rs` (P1-C), `css_cold_canonical.rs`
(P1-D), `css_canon_bench.rs` (P1-E/P1-F), `css_track1_profile.rs` (P1-D profile
driver). Each claims to be "the canonical N>=50 cold harness." For a COST baseline
this is a grounding hazard: the per-corpus medians differ across artefacts (e.g.
bootstrap track1_fact 695.62 / 736.03 / 850.41 / 719.60 / 784.60 across A/B/C/E/F)
partly because the harnesses differ (N, target-cpu=native vs not, mimalloc vs
system, fresh-`to_vec` vs page-touch). A cost baseline S-P2 grounds on must be ONE
harness. **Required fold (V2):** the orchestrator designates ONE canonical harness
(P1-E/F's `css_canon_bench.rs` is the most-cited and carries the PMU mode) and the
other four artefacts cite their numbers AS produced by that one binary, or the
divergence (esp. target-cpu=native on/off, allocator) is stated as the reason for
the spread. Today a reader cannot tell whether the 695→850 bootstrap spread is
host noise or harness divergence. This bears on cost-truth, hence in-charter.

**No REJECT.** No artefact proposes a speculative kernel without a profiled
antecedent. The one kernel that lacks a CSS antecedent — the udot digit kernel
(C4b) — is explicitly orphan-blocked by P1-E §4.4. The NEON byte-class primitive
(`byte_class_index_64`/`to_bitmask64`, names that do NOT yet exist in
`dispatch.rs` — only `select_classifier`/`PrimitiveKernels`/`lo6_table_admissible`
do) is named as a *to-build target grounded in the measured 56–65%
`find_component_delim` leaf*, gated behind tape activation. That is correct
profile-first discipline: the kernel is derived from a profiled antecedent, not
invented. (Minor: A/B/E/F should phrase these two as candidate *to-build*
primitives rather than implying they are extant symbols — folded into CH4 advisory,
not a blocking REVISE.)

---

## §3 — Counts + dispositions

Sections dispositioned: 41 (across 6 artefacts).

| Disposition | Count |
|---|---:|
| ACCEPT | 36 |
| REVISE | 5 |
| REJECT | 0 |

ACCEPT rate: 36/41 = **87.8%** (below the 95% convergence gate → V2 required).

REVISE list (each must fold into V2 — zero orphans permitted):
1. **CH4-1** — P1-A §2.1 c/B column (`ri_cycles` ungrounded).
2. **CH4-1** — P1-B §2.1 c/B column + §2.3 cycle-conclusion (`ri_cycles` ungrounded).
3. **CH4-1** — P1-D §3 c/B label (wall-derived, nominal clock, ambiguous vs A/B).
4. **CH4-2** — P1-C §2.3 sub-function line attribution (`:26`/`:45` unverified).
5. **CH4-3** — harness multiplicity (5 binaries; designate ONE canonical baseline).

REVISE root-cause concentration: 3 of 5 are the single CH4-1 `ri_cycles`-vs-instr/byte
contradiction → one consistent V2 fold (adopt P1-F instr/byte pass-wide) clears three.

Cost-grounding verdict: the five benched-CSS hot leaves are fully grounded
(measured %self-time + named candidate primitive + verified source antecedent); no
speculative kernel survives; the CH4 charter's substantive bar is MET. The REVISEs
are about the PROVENANCE/TRUTH of the cost-density (c/B) numbers and the harness
baseline, not about missing or fabricated hot-leaf grounding.
