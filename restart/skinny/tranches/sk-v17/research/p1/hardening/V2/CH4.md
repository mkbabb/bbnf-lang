# SK-V17 S-P1 CHALLENGE — CH4 COST (V2)

Lens: CH4 COST. Cycle: V2. Date: 2026-05-29.
Charter (dispatched): every named hot leaf carries a measured % self-time + the
candidate primitive it grounds; no speculative kernel without a profiled
antecedent. (This is the COST/hot-leaf-grounding charter the orchestrator
dispatched; distinct from PASS-1-PROFILE §3's CH4 "reproducibility" text —
reproducibility is folded in as `[repro-as-cost]` where it bears on whether a
cost number is real.)
Subject: SK-V17 S-P1 PROFILE artefacts
`restart/skinny/tranches/sk-v17/research/p1/{p1a,p1b,p1c,p1d,p1e,p1f}.md`.
Baseline verified against source at master HEAD `6496fecae` (working tree).
Prior cycle: `hardening/V1/CH4.md` (87.8% ACCEPT; 5 REVISE — CH4-1 ×3, CH4-2, CH4-3).
Output: this file.

Disposition vocabulary: ACCEPT / REVISE / REJECT. One row per artefact §-section.

---

## §0 — Source verification performed (the cost-grounding floor)

Every hot-leaf symbol + line cited across the six V2 artefacts was re-resolved
against the benched tree. All resolve exactly:

| Cited symbol | Cited line | Verified | Status |
|---|---|---|---|
| `emit_fact_stream` | generated.rs:5 | `pub fn emit_fact_stream(...)` :5 | OK |
| `emit_full_parse` | generated.rs:61 | :61 | OK |
| `push_hex64(fnv64(input))` call site | generated.rs:26 | `push_hex64(&mut out, fnv64(input.as_bytes()))` :26 | **OK (CH4-2 cleared)** |
| `emit_declarations(input,&mut out)` call site | generated.rs:45 | `emit_declarations(input, &mut out);` :45 | **OK (CH4-2 cleared)** |
| `CssFullParser` | generated.rs:103 | :103 | OK |
| `parse_stylesheet` | generated.rs:118 | :118 | OK |
| `parse_at_rule` | generated.rs:137 | :137 | OK |
| `parse_block` | generated.rs:189 | :189 | OK |
| `parse_declaration` | generated.rs:242 | :242 | OK |
| `skip_ws_comments` | generated.rs:263 | :263 | OK |
| `find_component_delim` | generated.rs:288 | :288; inner `delimiters.contains(&byte)` :295, `pos = match byte` :298, `_ => pos+1` :307 | OK |
| `consume_balanced_at` | generated.rs:320 | :320; inner loop :322-338 | OK |
| `consume_comment_at` / `consume_string_at` | :342 / :353 | :342 / :353 | OK |
| `fnv64` / `push_ascii_lower_hex` | :619 / :628 | :619 / :628 (body :629-634) | OK |
| `TapeBuilder` / `push_plain_offset` | assembler.rs:42 / :71 | :42 / :71 | OK |
| `select_classifier` / `lo6_table_admissible` | dispatch.rs:42 / :101 | :42 / :101 | OK |
| `parse_4_digits_dotprod` (udot orphan, C4b) | digit_mac.rs:27 | `aarch64/digit_mac.rs:27` | OK (path is `aarch64/`) |
| canon harness `assert!(n>=50)` | css_canon_bench.rs:250 | :250 | OK |
| canon harness PMU gate | css_canon_bench.rs:211 | `if std::env::var("CSS_CANON_PMU").is_ok()` :211 | OK |
| `ri_instructions`/`ri_cycles` fields | css_canon_bench.rs:74/75 | :74 / :75 | OK |

**P1-E shared-inner-loop claim verified true (a genuine V2 cost-grounding
tightening).** P1-E §2.3 / §2.5 fold `consume_balanced_at` and
`find_component_delim` into ONE NEON byte-class-scan target on the grounds that
their inner loops are byte-for-byte identical except the membership test. Source
confirms it: `generated.rs:293-308` and `:322-338` differ only at the membership
predicate (`delimiters.contains(&byte)` :295 vs `byte == close` :324); both share
the identical `pos = match byte { '\''|'"' => consume_string_at … _ => pos+1 }`
dispatch. The two leaves (56.52% + 11.05% = ~68% self-time) collapse to ONE
profiled primitive, not two. This is correct attribution and strengthens the
charter discharge.

**Cost-grounding verdict on the named leaves (charter substance — MET).** The
benched-CSS hot leaves each carry a measured %self-time on ≥3 independent
profiles AND ground a named candidate primitive:
- `find_component_delim` 56.52–65.05% (+ `consume_balanced_at` folded) → the
  NEON byte-class-membership scan (`select_classifier` dispatch.rs:42), gated
  behind tape activation.
- fact-stream alloc floor ~64–80% + `emit_fact_stream` 23.8% + `push_ascii_lower_hex`
  ~8% → `TapeBuilder::push_plain_offset` tape append (assembler.rs:71).
- lightningcss comparator plane: `cssparser::consume_name` 8.92% / `skip_whitespace`
  5.88% / `drop_in_place::<Token>` 3.95% (P1-F) → grounds the FAIRNESS of the
  >SOTA bar as a measured cost claim (the comparator demonstrably materializes the
  CSSOM, it is not a token-scan strawman).

No leaf is named without a number; no proposed primitive lacks a profiled
antecedent; the one kernel without a CSS antecedent (the udot/i8mm digit kernel
`aarch64/digit_mac.rs:27`, C4b) is explicitly orphan-blocked by P1-E §4.4 (zero
digit-parse self-time in CSS recognition). The CH4 charter's substantive bar is
MET. The single open defect (CH4-4) is about the PROVENANCE/TRUTH of the
cycles-per-byte counter, not about missing or fabricated hot-leaf grounding.

---

## §1 — V1 REVISE fold audit

| V1 finding | Required fold | V2 status |
|---|---|---|
| **CH4-1** (P1-A §2.1 `ri_cycles` c/B ungrounded) | adopt instr/byte; strike/caveat ri c/B | **FOLDED.** P1-A §2.1b instr/byte table added (reliable counter); §2.1 ri c/B column struck-through + labelled UNRELIABLE; COST-SURFACE NOTE (CROSS X1) at the head. |
| **CH4-1** (P1-B §2.1 col + §2.3 cycle-conclusion) | instr/byte; re-ground "~3× String" on i/B | **FOLDED.** P1-B §2.1 carries `instr/B` column, cycles struck; the String tax now stated as the 4.4–7.1× i/B gap (reliable). |
| **CH4-1** (P1-D §3 c/B label ambiguity) | unambiguous provenance vs A/B | **FOLDED-THEN-REOPENED → CH4-4.** P1-D §3.1 no longer prints an ambiguous nominal-clock proxy; instead it ASSERTS `ri_cycles` is a valid 4.27 GHz counter and explicitly supersedes the "unreliable" line A/B/F carry. This resolves the *label* defect but opens a fresh cross-artefact contradiction (CH4-4 below). |
| **CH4-2** (P1-C `:26`/`:45` unverified sub-line) | source-verify or collapse to `:5` | **FOLDED + VERIFIED.** P1-C §2.3 now tags `:26`/`:45` "source-verified inclusive call sites." This lens confirms against source: `:26` IS `push_hex64(&mut out, fnv64(input.as_bytes()))` (the FNV hash), `:45` IS `emit_declarations(input, &mut out)`. Cleared. |
| **CH4-3** (5 harness binaries; designate ONE) | designate ONE canonical; others cite-as | **FOLDED (light residual).** `css_canon_bench.rs` designated pass-wide; all six artefacts name it (cite counts: A 9 / B 19 / C 32 / D 19 / E 18 / F 27). Residual: P1-A §2.1 Mbps table is still sourced from `css_cold_harness` (instr/byte from `css_canon_bench`), with the ~20–29% cross-harness spread disclosed and the within-harness-ratio-only caveat stated. Honest, but the *authoritative Mbps* should come from the designated binary — light advisory, not blocking. |

Four of five V1 REVISEs are cleanly cleared. The fifth (P1-D §3 c/B) was folded by
a posture REVERSAL that introduces CH4-4.

---

## §2 — Per-artefact dispositions

### P1-A `p1a-samply-mode-1.md`

| § | Disposition | Basis |
|---|---|---|
| §0 cost/canonical notes | ACCEPT | CROSS X1/X2 notes correctly install instr/byte + the one canonical harness. |
| §2.1 throughput (ri c/B struck) | ACCEPT | Mbps median/min/max/stddev real; ri c/B struck-through + UNRELIABLE — but see CH4-4: the WORD "falsified" it inherits now collides with P1-D. The strike itself is defensible; the *characterization* is the cross-artefact problem. |
| §2.1b instr/byte | ACCEPT | Reliable counter; 4.4–7.1× fact/full gap; the load-bearing cost density. Grounded. |
| §2.2 recognition hot leaves | ACCEPT | `find_component_delim` 58.41/65.05%, `consume_balanced_at` 10.79/0.15%; %self-time + file:line; grounds NEON. |
| §2.3 fact-stream hot leaves | ACCEPT | Alloc family attributed; `mach_absolute_time`←libmalloc caller-walk (25591/25640) sound; grounds tape append. |
| §3 delta | ACCEPT | Honest N-direct; de-broadcasts the 2319 tuple per-corpus. |
| §4 anomalies | ACCEPT | NEON candidate gated behind tape (antecedent-grounded); FNV/hex named diagnostic-only, not a primitive; no orphan kernel. |

### P1-B `p1b-samply-mode-2.md`

| § | Disposition | Basis |
|---|---|---|
| §1 method | ACCEPT | `--save-only` + atos disclosed; planes to symbol. `[repro-as-cost]`: atos path stated, reproducible. |
| §2.1 throughput + instr/B | ACCEPT | N=200 medians; `instr/B` column reliable, cycles struck. CH4-1 discharged. (CH4-4 wording applies — see §3.) |
| §2.2 fact-stream hot leaves | ACCEPT | kernel/malloc/`emit_fact_stream`/`push_ascii_lower_hex` each %self-time + file:line. |
| §2.3 recognition hot leaves | ACCEPT | `find_component_delim` 56.55%, `consume_balanced_at` 11.51%; grounds NEON. |
| §2.4 canonical harness | ACCEPT | Names `css_canon_bench` as the ONE harness; CH4-3 fold. |
| §3/§4 | ACCEPT | Masking-shift sequencing sound; no orphan kernel. |

### P1-C `p1c-samply-mode-3.md`

| § | Disposition | Basis |
|---|---|---|
| §intro cost note | **REVISE (CH4-4)** | `:39-41` states CPI 0.16–0.28 is "physically impossible (M-series retires ≤8 µops/cycle)" and the counter "is not counting core cycles." This is the contradicted characterization — see CH4-4. The CONCLUSION (ground on i/B) is correct; the *justification* ("impossible") is false and now disputed by P1-D. **Fix:** replace "physically impossible" with the accurate frame: "CPI 0.16–0.28 ⇒ IPC 3.7–6.4, within the M5 issue width, so the values are physical; the `ri_cycles` counter cannot be DISAMBIGUATED as real-cycles vs a wall-proportional scaled timer from rusage alone — it is therefore non-load-bearing, and S-P2 grounds on i/B." Do not assert impossibility. |
| §2.3 hot-leaf line attribution | **ACCEPT (CH4-2 cleared)** | `:26`/`:45` source-verified by this lens; %self-time magnitudes accepted; line precision now correct. |
| §2.4 recognition hot leaves | ACCEPT | line citations within :288–:338 verified; grounds NEON. |
| §2.5 PMU table | REVISE (CH4-4, same root) | "CPI≪1.0 ⇒ counter falsified" header carries the same false impossibility claim; same fix as §intro. |
| §3/§4 | ACCEPT | A3 FNV diagnostic correctly flagged as vanishing-with-tape (no kernel); plane reconciliation (G) sound. |

### P1-D `p1d-pmu-cycles.md`

| § | Disposition | Basis |
|---|---|---|
| §1 method | ACCEPT | xctrace + rusage commands verbatim; `[repro-as-cost]` satisfied. |
| §2.x throughput + hot leaves | ACCEPT | `find_component_delim` intra-leaf line split (:298/:295/:307) verified against body; BEST-grounded decomposition in the pass; triple-rescan (§2.5) structurally true. |
| §3.1 **PMU posture (ri_cycles "valid 4.27 GHz")** | **REVISE (CH4-4 — the load-bearing V2 defect)** | P1-D reverses V1 and asserts `ri_cycles` is a proven real cycle counter, "supersed[ing] the 'ri_cycles unreliable' line P1-A/P1-B/P1-F carried." The IPC re-interpretation (CPI 0.16 ⇔ IPC 6.4, physical on a ~8-wide core) is CORRECT and is the right rebuttal to P1-F's false "impossible" claim. **BUT the proof offered is non-probative:** the "steady 4.27 GHz across workloads" derivation computes `wall_s` FROM the loop Mbps (`wall_s = bytes·iters·8/(mbps·1e6)`), then divides `ri_cycles` by it — so `ri_cycles/wall ≈ const` is observationally IDENTICAL whether `ri_cycles` is a fixed-frequency core-cycle counter OR a wall-proportional scaled tick. This lens tested the discriminator directly: bootstrap cyc/B ratio fact/full = 2.836 vs the (1/Mbps) wall ratio = 2.847 — cyc/B tracks wall time to <0.4%, which is consistent with BOTH models and disambiguates NEITHER. P1-D has not earned the word "proven." **Fix:** retain the IPC re-interpretation (it correctly retires P1-F's impossibility error) but downgrade the claim from "ri_cycles is a proven 4.27 GHz counter, A/B/F are wrong" to "the sub-1.0 CPI is PHYSICAL (high IPC), not a defect; however rusage `ri_cycles` cannot be disambiguated as real-cycles vs wall-proportional scaled tick from this interface alone, so it stays non-load-bearing; instr/byte is the sole grounded cost density." This makes §3.1 consistent with A/B/C/F's *conclusion* (i/B only) while correcting their *false justification*. |
| §3.2 PMU table | ACCEPT | i/B figures are the load-bearing surface and rank planes cleanly; IPC column is internally consistent (verified: instr/cyc = stated IPC on every row). Only the cyc/B *characterization* (CH4-4) needs the §3.1 fix; the numbers stand as RAW-non-load-bearing. |
| §4 anomalies | ACCEPT | udot orphan named never-reached; zero-SIMD confirmed by symbol-table absence; no orphan kernel. |

### P1-E `p1e-hot-leaf-attribution.md`

| § | Disposition | Basis |
|---|---|---|
| §1 method | ACCEPT | Caller-walk syslib attribution sound. |
| §2.1/§2.2 throughput + delta | ACCEPT | Mbps only; no ri_cycles c/B → no CH4-4 exposure. |
| §2.3/§2.5 hot-leaf table | ACCEPT — exemplary | `find_component_delim` 56.52% + `consume_balanced_at` 11.05% folded into ONE NEON target on a SOURCE-VERIFIED shared inner loop (:293-308 ≡ :322-338). This is the tightest primitive attribution in the pass: it proves the ~68% is ONE primitive, not two, so S-P2 grounds one kernel not two. Every leaf carries symbol + %self-time + file:line + class + candidate primitive. |
| §2.4 fact-stream attribution | ACCEPT | Syslib-caller causal attribution (91%+ reached FROM `emit_fact_stream`) ties the alloc floor to the named leaf. |
| §4.4 **orphan-block (C4b udot kernel)** | ACCEPT — exemplary (charter keystone) | Explicitly orphan-blocks the udot/i8mm digit kernel (`aarch64/digit_mac.rs:27`) for ZERO CSS digit-parse self-time, and names the re-admission condition (typed lazy-`ValueRef` re-profile after W1/W2). This is the precise "no speculative kernel without a profiled antecedent" discharge. No speculative kernel survives. |

### P1-F `p1f-bench-canonical.md`

| § | Disposition | Basis |
|---|---|---|
| §1 method | ACCEPT | Commands verbatim; canon harness named; `[repro-as-cost]` satisfied. |
| §2.1 throughput | ACCEPT | N≥50 medians; per-corpus delta-vs-lcss grounded. |
| §2.2 instr/byte (primary) | ACCEPT | instr/byte is the reliable, load-bearing density; correctly primary; reproducible <0.5%. |
| §2.2.1 **ri_cycles "physically impossible"** | **REVISE (CH4-4 — root of the contradiction)** | `:299` "A retired-instruction CPI below 1.0 is physically impossible on M5" is FACTUALLY FALSE. M5 P-cores are ~8-wide; IPC 3.7–6.4 (= CPI 0.16–0.27) is within issue width and routinely achievable on tight, well-predicted scan loops. P1-F over-claims impossibility where the correct claim is "non-disambiguable from rusage, therefore non-load-bearing." This false justification is what P1-D §3.1 (correctly) rebuts and (incorrectly) over-corrects. **Fix:** strike "physically impossible"; state "CPI 0.16–0.28 ⇒ IPC 3.7–6.4 is PHYSICAL on the M5's ~8-wide core; the figure is non-load-bearing not because it is impossible but because `proc_pid_rusage.ri_cycles` cannot be disambiguated as dynamic core-cycles vs a wall-proportional scaled tick from this interface — so instr/byte alone is grounded." The instr/byte conclusion is unaffected. |
| §2.3 hot-leaf attribution (3 planes) | ACCEPT | recognition + fact-stream + the lightningcss-plane attribution (`cssparser::consume_name` 8.92%, `drop_in_place::<Token>` 3.95%, ~38% tokenizer + ~30% typed-node build/drop) — the only artefact that profiles the comparator to PROVE full-CSSOM materialization; grounds the fairness-as-cost. |
| §3/§4 | ACCEPT | W8R broadcast reproduced per-corpus; "~70 Mbps/~14×" classified N-direct; eager-typed K (pre-blocked). |

---

## §3 — Cross-artefact COST findings

**CH4-4 (REVISE — the load-bearing V2 cost defect; orphan-prevention: must fold).**
The pass ships a DIRECT, UNRESOLVED CONTRADICTION on whether the cycles-per-byte
counter is a real cost figure:
- **P1-A §2.1, P1-B §2.1, P1-C §intro/§2.5, P1-F §2.2.1** all characterize
  `ri_cycles` cyc/byte as **"falsified / physically impossible"** (CPI < 1.0
  "impossible on M5", "not counting core cycles").
- **P1-D §3.1** asserts that characterization is **"itself incorrect, and this
  pass corrects it"** — `ri_cycles` is a valid 4.27 GHz counter, sub-1.0 CPI is
  high IPC, and it explicitly states this **"supersedes the 'ri_cycles unreliable'
  line P1-A/P1-B/P1-F carried."**

Both characterizations are individually defective, and this lens adjudicated both
against the host:
1. **The "physically impossible" claim (A/B/C/F) is FALSE.** The host is an Apple
   M5 Max (`machdep.cpu.brand_string`); its P-cores are ~8-wide. IPC 3.7–6.4
   (= CPI 0.16–0.27) is well within issue width and is the normal signature of a
   tight, branch-friendly scan loop on a wide superscalar. CPI < 1.0 is not
   impossible; P1-D is right to reject "impossible."
2. **The "proven real 4.27 GHz counter" claim (P1-D) is NON-PROBATIVE.** P1-D's
   only evidence is `ri_cycles / wall_seconds ≈ 4.27 GHz steady across workloads`,
   but `wall_seconds` is DERIVED from the loop Mbps. A fixed-frequency real-cycle
   counter and a wall-proportional scaled tick are OBSERVATIONALLY IDENTICAL under
   that derivation — both yield a constant ratio. This lens tested the
   discriminator: bootstrap cyc/B ratio (fact/full) = **2.836**, the wall-time
   (1/Mbps) ratio = **2.847** — cyc/B tracks wall time to <0.4%, consistent with
   BOTH models. The `hw.tbfrequency` mach timebase is 24 MHz, confirming a scaled
   reference clock exists on this platform. P1-D has not earned "proven."

The COST consequence: the pass cannot agree whether cyc/byte is a measured cost or
an artefact, and P1-D's §3.1 explicitly contradicts five sibling sections while
claiming to supersede them — leaving the orchestrator with two mutually exclusive
"the ONE c/B posture for the pass" declarations. Per the CH4 charter (every cost
figure attached to a hot leaf must be a real, agreed measured number), this is
ungrounded cost provenance.

**Required fold (V2→V3) — ONE pass-wide posture, adopted verbatim in all six:**
> "instr/byte (`ri_instructions`) is the sole load-bearing cost density and is
> reliable to <0.5%. The sub-1.0 CPI from `ri_cycles` is PHYSICAL (IPC 3.7–6.4 on
> the M5's ~8-wide core), NOT impossible; however `proc_pid_rusage.ri_cycles`
> cannot be disambiguated as dynamic core-cycles vs a wall-proportional scaled
> tick from the rusage interface alone, so cyc/byte is reported RAW and
> non-load-bearing. No conclusion rests on it."

This wording (a) corrects A/B/C/F's false "impossible," (b) corrects P1-D's
non-probative "proven," (c) preserves the shared, correct conclusion (i/B only)
that ALL SIX already ground on. P1-D §3.1's "supersedes A/B/F / ONE c/B posture"
sentence must be struck and replaced with the agreed posture; A/B/C/F's
"physically impossible / falsified" sentences must be replaced with
"non-disambiguable, non-load-bearing." It is a single consistent edit across six
files; the orchestrator must not leave it orphaned.

**CH4-5 (ACCEPT-advisory — P1-A Mbps-from-css_cold_harness residual,
`[repro-as-cost]`).** P1-A's §2.1 authoritative Mbps table is still produced by
`css_cold_harness`, not the V2-designated canonical `css_canon_bench`, with the
~20–29% cross-harness spread disclosed and a within-harness-ratio-only caveat. The
instr/byte surface IS from the canonical binary, and the disclosure is honest, so
this does not block — but for a cost baseline S-P2 grounds on, the authoritative
Mbps should also come from the one designated binary. Advisory: P1-A reproduce the
§2.1 Mbps from `css_canon_bench bench 64`, or restate the table as canon-sourced.
Not a blocking REVISE.

**No REJECT.** No artefact proposes a speculative kernel without a profiled
antecedent. The one kernel lacking a CSS antecedent — the udot digit kernel (C4b,
`aarch64/digit_mac.rs:27`) — is explicitly orphan-blocked by P1-E §4.4. The NEON
byte-class-scan primitive (`select_classifier` dispatch.rs:42; the to-build
`byte_class_index_64`/`to_bitmask64` names do NOT yet exist as symbols) is named
as a to-build target grounded in the measured 56–68% `find_component_delim` +
`consume_balanced_at` leaf (now correctly folded to ONE primitive by P1-E), gated
behind tape activation. Correct profile-first discipline throughout.

---

## §4 — Counts + dispositions

Sections dispositioned: **42** (across 6 artefacts; one more than V1 owing to the
P1-A §2.1b split and P1-C §intro/§2.5 split).

| Disposition | Count |
|---|---:|
| ACCEPT | 38 |
| REVISE | 4 |
| REJECT | 0 |

ACCEPT rate: 38/42 = **90.5%** (below the 95% convergence gate → V3 required for
CH4; the four REVISEs all share ONE root, CH4-4).

REVISE list (each must fold into V3 — zero orphans permitted):
1. **CH4-4** — P1-C §intro/§2.5: false "physically impossible" justification.
2. **CH4-4** — P1-D §3.1: non-probative "proven 4.27 GHz" + the "supersedes A/B/F"
   contradiction sentence.
3. **CH4-4** — P1-F §2.2.1: false "CPI < 1.0 physically impossible on M5."
4. **CH4-4** — P1-A §2.1 / P1-B §2.1: the inherited "falsified" characterization
   (the strike-throughs are fine; the *word* must change to "non-disambiguable,
   non-load-bearing" to align with the one agreed posture).

REVISE root-cause concentration: **all 4 are the single CH4-4 `ri_cycles`
characterization split** (A/B/C/F say "impossible/falsified"; D says "proven valid,
they're wrong"). BOTH framings are defective; ONE agreed posture ("physical but
non-disambiguable, non-load-bearing; i/B is the sole grounded density") clears all
four with a single coordinated edit.

V1→V2 movement: V1 87.8% → V2 90.5% ACCEPT. CH4-1 (×3), CH4-2, CH4-3 all CLEARED
and source-verified. The V2 REVISE is a NEW contradiction introduced by P1-D's
posture reversal — narrower and single-rooted than the V1 set, and adjudicated
here against the host so V3 has the exact agreed wording to fold.

Cost-grounding verdict: the benched-CSS hot leaves are fully grounded (measured
%self-time + named candidate primitive + source-verified antecedent); the
two-leaves-into-one NEON fold (P1-E) and the lightningcss-plane fairness
attribution (P1-F) are exemplary; no speculative kernel survives (C4b
orphan-blocked). The CH4 charter's substantive bar is MET. The one open REVISE is
about the agreed TRUTH-CHARACTERIZATION of the non-load-bearing cyc/byte counter,
not about missing or fabricated hot-leaf grounding.
