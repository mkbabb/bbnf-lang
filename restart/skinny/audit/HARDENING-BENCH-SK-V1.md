# HARDENING-BENCH-SK-V1

## §1 Target identification

- **Target**: `restart/skinny/BENCH.md` (post-redress, 1780 lines).
- **Cycle**: SK-V1 (first independent audit of the post-redress skinny corpus).
- **Lens stack**: Lanes 1-9 (Lane 2 N/A — single-wave) + Lenses F/G/H/I/J/K + Lenses L/M/N.
- **Sister-quadrant cross-references read**: `restart/skinny/SUBSTRATE.md` §8 (TapeBuilder API), `restart/skinny/COMPILER.md` §1.3 (host-fn-free rationale + probe definitions), `restart/skinny/INDEX.md` (deviation ledger), `restart/skinny/WORKSPACE.md` §2 (LOC budget), `restart/MASTER-PLAN.md` §4 (Hard Architectural Gates lines 108-169), `restart/ARCHITECTURE.md` §11 (Performance Targets lines 1481-1529), `restart/locks/14-LOCKS.md` Lock 8, `restart/corpora/SOTA.md:50-89, 130-136`.
- **Time consumed**: ~38 minutes against 45-minute cap.

## §2 Cohort verdict

| Lane | Verdict | KEEP | REINVENT | DISCARD | Recommendation |
|---|---|---:|---:|---:|---|
| 1 — Lock-Adherence | honoured-with-narrow-amendment | 11 | 1 | 0 | Lock 8 honoured throughout; Lock 1 sealed via SUBSTRATE.md §8 reference NOT cited from §1.2 — surface required. |
| 2 — Sequencing | N/A | — | — | — | Single-wave skinny. |
| 3 — Cohesion | honoured | — | — | — | One stale H-outcome reference at §6.3:675 + outcome-ID list at §10.2:1347 missing F-positive/F-noise/M, contains stale H. |
| 4 — SOTA Anchoring | LOAD-BEARING / honoured | 8 | 1 | 0 | Every threshold cites a competitor + corpus + platform. The BEAT_BOUND construction is the load-bearing artefact of this redress. |
| 5 — Grammar-Authoritative | honoured | — | — | — | No grammar-name dispatch; JSON is the sole skinny grammar by design. |
| 6 — LOC Budget | honoured-with-narrow-amendment | 5 | 2 | 0 | §11.1 sums to ~2,200 LOC; WORKSPACE.md:73 caps `bbnf-bench` at 2,000 LOC + reconcile-to-V1 at 4,000. CSS prior probe optionality is correctly scoped, but the ≤ 600 LOC for `track2/css_prior.rs` + `track2/json/` (measurement-driven) + new probes pushes the WORKSPACE skinny ceiling. |
| 7 — Friction Forecast | honoured | — | — | — | RESULTS.md verdict-first + reading order is well-targeted. |
| 8 — Carry & Deferral | honoured | 7 | 1 | 0 | §9 omissions all name a V1 receiver. The §9.1 CSS-prior probe defers cleanly. |
| 9 — Greenfield Discipline | honoured | — | — | — | No legacy code; no quick fixes; root-cause framing throughout. |
| F — LLM bias | honoured-with-narrow-amendment | — | — | — | One pseudo-precise numeric in §7.8.1 Probe A ("≤ 50 ns/call canonically ~10-30 ns"); two unfalsifiable adjectives ("generous", "honest"). |
| G — Overfitting | honoured | — | — | — | The CSS prior probe (§9.1) is the explicit anti-overfit lever; it correctly bounds JSON-shape inference. |
| H — Hallucination + provenance | **AMENDMENT-REQUIRED-NARROW** | 0 | 2 | 0 | Two technical-correctness faults: §7.8.3 cold-cache eviction primitives are wrong for aarch64 (`__dsb` does not evict; `dc civac` does); incomplete for x86_64 (`_mm_clflush` per-line semantics not described); TLB + branch-predictor cooling absent. |
| I — Contrivance | honoured-with-narrow-amendment | — | — | 1 | §8.3 CI runner discount table introduces variable thresholds across runners. The discount-applied-to-threshold model adds complexity that may not load-bear if every NO-GO requires local re-run anyway. |
| J — Host-language leverage | honoured | — | — | — | `getrusage` + criterion + `mimalloc` + standard cargo bench are appropriately leveraged. |
| K — Meta-grammar discipline | honoured | — | — | — | The bench harness does not invent semantic apparatus; it is straightforward criterion + gate. |
| **L — Premise fidelity** | honoured-with-narrow-amendment | — | — | — | Two probes for `@host fn` cut land cleanly; eager-decode bands defensible (see §3.L below). One MASKING-classification gap remains at the alternate-pext-mask-plan probe — confirmatory framing is honest but the cross-platform plan-divergence finding does not actually update the matrix. |
| **M — Falsifiability** | honoured-with-narrow-amendment | — | — | — | The matrix has multiple NO-GO branches the skinny could plausibly land in (G, I, K, L, M). Outcome A is now distinctly stricter than B/C. **Two falsifiability holes**: (a) the F-positive/F-noise/E-D-C cascade has a structural gap when Track 2 ∈ (S × 1.05, S × 1.10] AND Track 1 > Track 2 × 1.10 — no outcome matches; (b) the F-noise "noise threshold + 5% headroom" claim doesn't resolve to the (1.05, 1.10] band cleanly. |
| **N — Graduation mechanicality** | honoured | — | — | — | No new V1-deviating apparatus; the redress is internal to the bench spec. |

**Final decision**: **SK-AMENDMENT-REQUIRED-NARROW**.

## §3 Lane and lens findings (selected)

### §3.L Lens L — Premise fidelity (load-bearing)

The redress at §7.8.1 split the host-call probe into two questions, each with its own threshold. Steelman the design.

| Probe | Question | Threshold | Defensibility |
|---|---|---|---|
| `host_call_dispatch_overhead` | per-invocation registry-vs-direct cost | ≤ 50 ns/call M1 Pro | Defensible. A virtual call (vtable indirection + branch-predict miss) is typically 5-10 ns; a hash-map registry lookup adds ~20-40 ns; bounds-check + downcast adds another 5-10 ns. 50 ns is generous but not absurdly so. The pseudo-precise "canonically ~10-30 ns" softens commitment without measurement; trim to "single-digit ns for direct call, 20-40 ns for registry indirect". |
| `host_call_eager_decode` (twitter) | gross-time JSON-shape eager decode | 5-15% delta | Defensible. Twitter has ~700+ string fields with ~5% escape density and avg ~16-byte string length; UTF-8 + escape decode at ~2-4 ns/byte over ~80 KB of strings = 160-320 µs eager work; against a ~400 µs total parse, that's 40-80% — but the V1 substrate's `decode_string` already does the work, and the probe measures only the *dispatch* overhead delta on top of the eager work, not the eager work itself. The 5-15% band is for **the registry-overhead-on-eager-work**, not the eager-work cost. **The spec is ambiguous which it measures**. Per Probe A measuring dispatch-only and Probe B measuring "eager-decode work" gross-time, the bands should be commented as eager-decode work + dispatch overhead summed. Surgery: clarify what fraction is eager-decode-work and what fraction is dispatch overhead in the band rationale. |
| `host_call_eager_decode` (citm) | gross-time JSON-shape eager decode | 3-8% delta | Defensible. Citm has many short keys (8-16 bytes); decode work scales with string bytes. |
| `host_call_eager_decode` (canada) | gross-time JSON-shape eager decode | < 2% delta | Defensible. Canada is ~98% numeric; few strings; the eager-decode call is rarely emitted. |

**Verdict**: FAITHFUL with band-rationale clarification. The probes do test what they claim. The Probe A 50 ns/call threshold is correctly scoped to dispatch-only; the Probe B bands measure end-to-end-with-eager-work. The amendment is editorial (clarify the band composition) not architectural.

The §7.8.2 alternate-plan probes were reframed as confirmatory with one plausibly-better candidate (`alternate_pext_mask_plan` on x86_64). The reframing is honest. The remaining gap: the "Inverted dominance" branch (alternate_pext_mask_plan < canonical × 0.90 on x86_64) routes to "RESULTS notes the cross-platform plan divergence as a tranche-H input" — but does NOT downgrade the SOTA-beat probability for Intel. The matrix verdict could be outcome A on M1 Pro (where canonical wins) yet outcome F-equivalent on x86_64 (where canonical is pessimal). If V1 SOTA close gates run on x86_64 (per Lock 8: "simdjson On-Demand 7 GB/s on Intel Skylake"), the skinny on M1 Pro would over-predict V1 outcome on Intel. **Lens L MASKING signal**. Surgery: the probability-update mapping (§10.3) should include a row for "alternate_pext_mask_plan < canonical × 0.90 on x86_64" that downgrades V1-SOTA probability on the Intel line.

The §1.5 Track 2 substrate-fairness story does not name `TapeBuilder` even though SUBSTRATE.md §8 explicitly defines it as the API the BENCH agent uses for Track 2. INDEX.md ledger row 6 (`Tape<'input>` sealing) classifies the SK-V1-vs-V1 deviation as MECHANICAL with named inversion — but the inversion *is* `TapeBuilder`. BENCH.md §1.2 references runtime::tape generically; §10.6 checklist says "calls runtime::tape APIs only". The named API contract (§8 SUBSTRATE) should be cited from §1.2 BENCH for Lens N closure: surgery is one cross-reference line.

### §3.M Lens M — Falsifiability (load-bearing)

The matrix has the strongest set of NO-GO branches the skinny has carried — outcomes G (substrate failure), I (parity oracle fail), J (schema fail), K (SIMD parity fail), L (SIMD throughput fail on canada), M (peak RSS > 3× competitor). All five are honest NO-GO branches the skinny could plausibly land in. The matrix is no longer a confirmation-bias engine. Lens M overall: honoured.

**Falsifiability hole 1** (structural gap at F-band edge). The §6.2.1 classification order tests:

- Step 7: F-positive / F-noise (Track 2 ∈ (S × 1.05, S × 1.10]) — by Track 1 sub-band.
- Steps 8-12: E/D/C/B/A (each requires Track 2 ≤ S × 1.05).

What is the outcome when **Track 2 ∈ (S × 1.05, S × 1.10] AND Track 1 > Track 2 × 1.10**? F-positive requires Track 1 ≤ Track 2 × 1.05; F-noise requires Track 1 ∈ (Track 2 × 1.05, Track 2 × 1.10]. Neither admits Track 1 > Track 2 × 1.10. Step 8 onward requires Track 2 ≤ S × 1.05 — fails. **Result: no outcome matches.** This is a real adversarial input: substrate borderline-weak (Track 2 = S × 1.07) AND codegen gap (Track 1 = Track 2 × 1.30, a known F+D combination).

Surgery: add an outcome F-codegen-gap row "Track 2 ∈ (S × 1.05, S × 1.10] AND Track 1 > Track 2 × 1.10" → CONDITIONAL — substrate warning + codegen gap (compound), or (cleaner) collapse F-positive/F-noise/F-codegen-gap into a single F outcome whose action sentence enumerates the Track 1 sub-band.

**Falsifiability hole 2** (F-positive/F-noise band rationale). §6.1 row F-noise: "within bench noise (criterion `noise_threshold(0.02)` plus 5% headroom)". The band is `Track 2 × 1.05 < Track 1 ≤ Track 2 × 1.10`. The criterion noise threshold of 2% is the RATIO drift the gate accepts as identical — it does not directly map to "5% headroom + 2% = 7%" or to "ratio range (1.05, 1.10]". The conflation is between *iteration-to-iteration drift* (criterion noise) and *track-to-track ratio difference* (different code, different inputs). 5% to 10% codegen overhead is a real engineering signal, not "noise". Calling it noise sets up the F-noise verdict to license dispatch on borderline codegen gaps that a stricter classifier would route to F-positive or worse.

Steelman: the redress intention was to distinguish "codegen meaningfully matches hand" from "codegen indistinguishable from hand within measurement uncertainty". This is a real distinction, but the threshold derivation is hand-waved. Surgery: replace "criterion `noise_threshold(0.02)` plus 5% headroom" with measurement-driven boundary — e.g., "the criterion 95% confidence interval upper bound for Track 1 overlaps Track 2 × 1.05". This is testable; "noise plus headroom" is not.

**M threshold (peak RSS 3× competitor on canada) defensibility**: the 3× ratio is generous against sonic-rs's deliberate lazy-materialisation memory trade. Sonic-rs anchors at ~2-3× input size (canada 2.2 MB → ~5-7 MB peak); 3× would be 15-21 MB. A tape + payload arena on canada with ~280K structural offsets at 8 bytes each = 2.24 MB tape + ~3-5 MB typed root = ~5-7 MB total. 3× is not generous, it is the substrate's natural operating point. The threshold may NEVER fire. Steelman: the concern is web-server/batch-ingestion memory residency under concurrent load, where 3× is the floor; below this, even a single slow GC or heap sweep blocks throughput. **Verdict**: 3× is correct as a floor, but the spec should compute a forward-projection ("3× sonic-rs canada ≈ 21 MB; tape substrate target ≈ 5-7 MB → ≈ 0.7-1.0× competitor; 3× is therefore a safety net, not a primary gate"). Without that calculation, the M-outcome NO-GO branch becomes ceremonial. Surgery: add the projection line in §9.6.

**BEAT_BOUND defensibility**: outstanding. The redress correctly binds outcome A on `min(S × 0.95, T_README)`. §6.4 numerics demonstrate `T_README` binds across all three corpora (380 µs vs 403 µs; 750 µs vs 789 µs; 2.8 ms vs 2.987 ms). The redress closes the prior ratification hole where a 400 µs Track 2 on twitter (S = 424 µs, S × 0.95 = 403 µs, BEAT_BOUND-old = 403 µs) would have ratified outcome A while missing the README spec. Lens M strongly endorses this redress.

**Reproducibility schema enforcement**: §5.3 explicitly states a missing-field row is INVALID and removed before classification, AND that `schema_version` mismatch is a hard FAIL ("the gate refuses to classify across schema versions silently"). This is the load-bearing falsifiability gate. The implementation in §8.2 gate.rs binary calls `schema_enforce(&rows)?` which exits 2 on missing-field. The mechanism is wired correctly.

### §3.H Lens H — Hallucination + provenance gaps

Two technical-correctness faults at §7.8.3 cold-cache probe.

**Fault 1**: `core::arch::aarch64::__dsb` is not a cache eviction primitive. From the ARM Architecture Reference Manual (ARM DDI 0487, §B2.3.13), `DSB` (Data Synchronization Barrier) ensures completion of memory accesses before the barrier — it does not invalidate cache lines. The architectural primitives for cache invalidation on aarch64 are `dc civac` (clean and invalidate by VA to PoC), `dc cvac` (clean), `dc ivac` (invalidate). The Rust intrinsics `core::arch::aarch64::__dc_civac` (where available; varies by toolchain) are the relevant calls. The buffer-touch-then-drop pattern can pressure the cache, but the spec's named primitive is wrong.

**Fault 2**: `_mm_clflush` flushes a single cache line per call (the line containing the address argument). To cool the corpus + parser data structures + dispatch tables, the call must iterate over the entire region in 64-byte (cache-line-size) strides. The spec writes "x86_64 `_mm_clflush` over the corpus bytes" without the iteration. Additionally, `_mm_clflush` on Intel post-Skylake has been deprecated in favor of `_mm_clflushopt` and `_mm_clwb` for newer microarchitectures.

**Fault 3**: TLB cooling is absent. The probe attempts to flush data caches but the iTLB and dTLB are not cooled. On a cold first parse the TLB miss latency on the parser's instruction footprint and the corpus bytes is a meaningful fraction of cold-vs-warm delta. The spec does not address this.

**Fault 4**: Branch predictor and dispatch-table cooling. The cold-cache premise includes "branch predictor unprimed, cold dispatch table" but the probe does not actually cool either — flushing data caches does not flush branch history.

The < 1.2× "suspicious" branch is the right epistemic guard, but the suspicion threshold may fire on every run because the eviction technique is incomplete. Surgery: replace the named primitives with the correct aarch64 (`dc civac` loop or `clear_cache` syscall + buffer-pressure) and x86_64 (`_mm_clflush` loop in 64-byte strides over corpus + parser hot-data ranges) sequences; add explicit "TLB and branch-predictor state are not cooled by this probe; the cold/warm ratio reported is dCache + iCache delta only" qualifier.

### §3.G Lens G — Overfitting (sharper for skinny)

The §9.1 CSS prior probe is the explicit Lens G lever. It tests substrate generality on a non-JSON shape with a generous 1.5× lightning-css threshold. **Verdict**: KEEP. This is the strongest anti-overfit signal in the post-redress BENCH spec. Without it, the JSON-only skinny would over-predict V1 SOTA on CSS (Tranche H.W4). The probe is correctly scoped: substrate-only walker, no codegen, no Pratt, no cost-model — the question is "does the substrate generalise" not "does the V1 stack generalise". Optionality is correct (defer with explicit deferral note). The 1.5× threshold may be overly generous (lightning-css is itself ~4.16 ms on bootstrap; 1.5× = 6.24 ms is a wide window) — but a substrate-only walker without any of the H-tranche tuning landing within 1.5× is genuinely a positive signal. Lens G honours.

### §3.I Lens I — Contrivance / over-engineering

§8.3 CI runner discount table introduces complexity that may not load-bear. The model: thresholds are scaled by runner discount factor (× 1.15 for `macos-14`, × 1.40 for `ubuntu-latest`, × 1.00 for self-hosted bare metal). A failing CI bench requires local re-run before NO-GO is final.

**Steelman challenge**: the discount table reduces noise. Without it, every CI run on a macos-14 GHA virtualised runner would emit false NO-GO and waste developer attention.

**Counter-steelman**: if every NO-GO requires local re-run before commit, the discounted thresholds in CI provide *only* an early warning, and the full classification matrix is run locally. The CI gate could simply emit "advisory non-blocking" status and let the local bench be the sole authoritative gate. The discount table adds 15-20 LOC to gate.rs (matrix expansion) and a `runners.toml` file — non-trivial, and its load-bearing role is unclear.

**Verdict**: ASPIRATIONAL — the runner discount table is V1 J.W1 work; for the skinny, the simpler model is "CI bench is advisory, not gating; local bench is authoritative". Surgery: collapse §8.3 + §8.4 into a single "CI is advisory" paragraph; remove `runners.toml`; reclaim ~50 LOC from the gate.rs budget.

### §3.4 Lane 4 — SOTA Anchoring (the load-bearing lens)

Every threshold cites a competitor + corpus + platform. SOTA.md:50-56 gives the canonical M1 Pro numbers for sonic-rs, simd-json, serde_json on twitter / citm / canada. BENCH.md §3.3 reproduces them; §6.4 derives BEAT_BOUND from them; §6.1 outcome rows reference S = min(in-run anchors). Lane 4 is honoured.

The redress that splits F into F-positive + F-noise was a Lens M improvement, not a Lane 4 improvement — and the F-noise rationale weakens the lane (as called out in §3.M). The G+H collapse is a Lane 4 improvement (eliminates the dependent variable Track 1 which the substrate failure makes irrelevant).

The M outcome (peak RSS) introduces a non-throughput Lock 8 row — but it is gated on `getrusage` not on competitor anchor numbers. This is correct: the SOTA gate is throughput; M is substrate-viability. The action text correctly distinguishes "substrate that hits SOTA-class throughput at 3× memory is not viable for concurrent-parse workloads" — i.e., M is not a SOTA gate, it is a usability gate, and the matrix is honest about that.

### §3.6 Lane 6 — Generated-Code + LOC Budget

§11.1 budget: target ≤ ~2,200 LOC. WORKSPACE.md:73 caps `bbnf-bench` at 2,000 LOC. **Discrepancy**: 200 LOC over-budget without explicit reconciliation.

Per-file:
- `fixtures.rs` ≤ 120
- `metadata.rs` ≤ 250 (was 200; +50 for schema_version + per-corpus parity hashes + RSS + cold_cache_mode fields)
- `parity.rs` ≤ 100
- `gate.rs` ≤ 350 (was 300; +50 for matrix expansion: F-split, G-collapse, M-add, BEAT_BOUND classifier, runner discount logic)
- `bin/gate.rs` ≤ 60
- `track2/json/` measurement-driven 800-1,500 LOC (was capped at 500; new measurement-driven model)
- `track2/css_prior.rs` ≤ 600 LOC (new, optional)
- `benches/json_parity.rs` ≤ 250 (was 200; +50 for probe additions)
- `benches/simd_scan.rs` ≤ 150 (was 100; +50 for per-corpus parity)

If Track 2 lands at the upper end (1,500) + CSS prior probe (600 if implemented) = 2,100 LOC for handwritten parsers alone, plus harness ~1,180 = **~3,280 total**. This significantly exceeds the WORKSPACE.md:73 ceiling of 2,000.

Steelman: WORKSPACE.md:73 row 9 lists the bbnf-bench skinny budget as 2,000 *with* a "Track 2 handwritten parser ≤500" assumption that the BENCH redress invalidated. The WORKSPACE row is now inconsistent with BENCH §11.1.

Surgery: WORKSPACE.md row 9 must be amended to ~3,000-3,500 LOC to reconcile. This is a cross-quadrant fault: the BENCH redress dropped the Track 2 LOC ceiling without updating WORKSPACE.

The skinny total (31,400 LOC handwritten) is computed from WORKSPACE.md:75. Adding 1,000-1,500 LOC to bbnf-bench moves the total to ~32,400-32,900, still well within Lock 13 budget but inconsistent with the INDEX.md headline. **Lens N MECHANICAL** but a documented amendment.

### §3.8 Lane 8 — Carry & Deferral

§9.1 (CSS prior probe) — receiver named (V1 H.W4 entry gate), gate named, deferral conditions explicit. **Honoured**.
§9.2 (incremental parsing) — receiver V1 I tranche; impact statement present.
§9.3 (LOC gates) — receiver F.W3 for nine-grammar scale.
§9.4 (WASM) — receiver V2.
§9.5 (Pratt + auto-detection) — receiver H.W2.
§9.6 (memory residency, now gated as M) — V1 J.W1 retains strict gate; skinny gates at generous 3×.
§9.7 (multi-core) — V1 single-threaded (no impact).
§9.8 (error-path bench) — receiver tranche I.
§9.9 (path/select API) — receiver tranche G.

All deferrals name receiver + gate. The §9.6 redress upgraded memory residency from report-only to NO-GO at 3× — Lens M improvement; Lane 8 maintains discipline.

§9.5 contains a soft phrase: "JSON's hot path uses SIMD scan unconditionally on x86_64 / aarch64 in skinny; auto-detection is bypassed by hardcoding the strategy in the skinny codegen path." This blurs the carry — the cost-model dispatch logic is what the alternate_pext_mask_plan probe partially bounds. Surgery: cross-reference §7.8.2 from §9.5 to make the masking-bound explicit.

## §4 Cross-quadrant invariants check

| Invariant | Source | BENCH.md status |
|---|---|---|
| TapeBuilder API | SUBSTRATE.md §8 | **Not cited from BENCH §1.2** — surgery: add cross-reference line. |
| host-fn-free + two probes | COMPILER.md §1.3 | Cited correctly; the two probes match exactly (`host_call_dispatch_overhead` + `host_call_eager_decode`); thresholds align. |
| Single-plan extraction + alternate-plan stub | INDEX.md row 4 | §7.8.2 covers this. The "alternate_pext_mask_plan" candidate is NEW vs INDEX.md row 4 — INDEX.md says "BENCH carries a small alternate-plan stub" (singular); §7.8.2 describes three alternates. Cohesion intact: more is more, and the redress added the plausibly-better candidate explicitly. |
| Skinny gates at generous 3× memory; V1 J.W1 strict 1.5× | §9.6 | Aligns with INDEX.md ledger; receiver V1 J.W1 named correctly. |
| Outcome ID enumeration in RESULTS template | §10.2 line 1347 | **STALE** — `<A|B|C|D|E|F|G|H|I|J|K|L>` reflects pre-redress matrix. Should be `<A|B|C|D|E|F-positive|F-noise|G|I|J|K|L|M>`. |
| §6.3 Outcome G + H reference | §6.3 line 675 | **STALE** — H was collapsed into G by the redress; line 675 still says "Outcomes G and H exist precisely because the bench is the arbiter". Surgery: change to "Outcomes G/I/J/K/L/M exist precisely because the bench is the arbiter". |

## §5 Per-item table (selected high-signal rows)

| Site (path:line) | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| BENCH.md:619 (outcome F-positive) | F-positive: substrate borderline-weak, codegen positive | The codegen ratio ≤ Track 2 × 1.05 means generator competitive with hand. | Distinguishes a positive codegen finding from a noise finding in a substrate-warning band. | Adds matrix complexity; one more outcome ID to communicate. | The split is a real distinction — generator-matching-hand on weak substrate is a useful signal that should not collapse with substrate-warning + noise. | KEEP |
| BENCH.md:620 (outcome F-noise) | F-noise: substrate borderline-weak, codegen indistinguishable from hand within bench noise | The "criterion `noise_threshold(0.02)` plus 5% headroom" claim. | The intent is correct. | The 5% headroom is hand-waved against the (1.05, 1.10] band; CI of Track 1 may exceed Track 2 × 1.05 by far more than criterion 2% drift. | Replace with measurement-driven boundary: "Track 1 95% CI upper bound overlaps Track 2 × 1.05". | REINVENT |
| BENCH.md:621 (outcome G) | G: substrate failure (collapsed prior G+H) | Track 2 > S × 1.10 dominates Track 1 outcome. | Cleaner — codegen riding broken substrate is not separately classifiable. | "any" Track 1 swallows information that might inform substrate redesign. | The challenge fails: if the substrate fails, Track 1 ratio is determined by substrate, not by codegen design. The collapse is correct. | KEEP |
| BENCH.md:626 (outcome M) | M: peak RSS > 3× competitor on canada | Substrate viability for concurrent-parse workloads. | Honest NO-GO branch absent from prior matrix. | 3× threshold may never fire (substrate target is ≤ 1× sonic-rs). | The threshold is correct as a safety net; without the projection calculation, the gate appears ceremonial. | KEEP with §9.6 amendment to add forward-projection. |
| BENCH.md:651-665 (§6.2.1 classification order) | Classification order: J → I → K → L → M → G → F-pos/F-noise → E → D → C → B → A | First-match wins; correctness/floor before throughput. | Order is defensible: correctness > substrate > codegen. | Hole: Track 2 ∈ (S × 1.05, S × 1.10] AND Track 1 > Track 2 × 1.10 has no matching outcome. | Add F-codegen-gap row OR collapse F-positive/F-noise into single F with Track 1 sub-band reported in action text. | REINVENT |
| BENCH.md:675 (§6.3) | "Outcomes G and H exist..." | Stale post-redress reference. | — | The H reference contradicts §6.1 collapse. | Editorial. | DISCARD (sentence-level) |
| BENCH.md:984-988 (§7.8.1 Probe A threshold) | ≤ 50 ns/call M1 Pro | Per-call dispatch overhead. | Defensible threshold. | The "canonically ~10-30 ns" is pseudo-precise without measurement source. | Cite a microbench reference (Rust virtual call cost paper, criterion overhead profile, or just "M1 Pro virtual call overhead measured at ~5-10 ns; bounds-check + table lookup adds ~20-40 ns"). | REINVENT |
| BENCH.md:1043-1066 (§7.8.3 cold_first_parse) | Cold cache eviction via `__dsb` / `_mm_clflush` | Cool L1+L2+L3 caches between iterations. | Cold-cache measurement is a real signal. | `__dsb` is a barrier, not eviction; `_mm_clflush` is per-line, not range; TLB + branch-predictor not cooled. | Replace with correct primitives (`dc civac` aarch64, 64-byte stride loop x86_64, qualifier on TLB+branch-predictor scope). | REINVENT |
| BENCH.md:1216-1245 (§9.1 CSS prior probe) | Substrate-only CSS walker, ≤ lightning-css × 1.5 | Anti-overfit substrate-generality probe. | Strongest Lens G lever in the post-redress BENCH spec. | Optional + report-only — could be omitted under budget pressure. | The optionality is appropriate; the gate flag-deferral is honest. | KEEP |
| BENCH.md:1284-1301 (§9.6 memory residency M) | Outcome M gates peak RSS > 3× competitor on canada | Hard gate, was prior report-only. | Eliminates a confirmation-bias path. | Threshold derivation argument is asserted not computed. | Add forward-projection (substrate target ~5-7 MB on canada vs sonic-rs 5-7 MB ≈ 1×; M is 3× safety net). | KEEP with §9.6 amendment |
| BENCH.md:1347 (§10.2 outcome ID enumeration) | `<A|B|C|D|E|F|G|H|I|J|K|L>` | RESULTS template outcome enumeration. | — | Stale: missing F-positive/F-noise/M; contains stale H. | Editorial. | DISCARD (line-level) |
| BENCH.md:1466-1473 (§10.3 probability mapping) | F-positive / F-noise / G / I / J / K / L / M | Outcome → V1-parity P + SOTA-beat P. | Maps to new outcomes; covers M and the F-split. | The "alternate_pext_mask_plan inverted dominance on x86_64" Lens-L MASKING is not represented. | Add a row "MASKING: cross-platform plan divergence on x86_64" lowering V1-SOTA-beat probability on the Intel line. | REINVENT |
| BENCH.md:1572-1581 (§11.1 LOC budget) | gate.rs ≤ 350; metadata.rs ≤ 250; track2 measurement-driven | LOC budget per file. | Reasonable for the redressed apparatus. | Total ≤ ~2,200 + Track 2 measurement-driven (800-1,500) + CSS prior optional (600) = up to ~4,300 vs WORKSPACE.md:73 ceiling 2,000. | Reconcile WORKSPACE.md:73 to ~3,500 LOC, OR reclaim through CI-runner-discount removal (Lens I). | REINVENT (WORKSPACE-side amendment) |
| BENCH.md:1573 (metadata.rs ≤ 250) | Schema fields: schema_version + per-corpus parity + RSS + cold_cache_mode | RowMetadata struct. | Plausible under steelman: the field count grew from ~25 to ~32, and TOML serialization + capture stays mechanical. | Tight. The cold_cache_mode + per-corpus parity hash + plan_variant + host_call_mode logic is non-trivial. | A 50 LOC overshoot is plausible; the budget should be ≤ 280 LOC to hedge. | KEEP-with-budget-hedge |
| BENCH.md:1575 (gate.rs ≤ 350) | Matrix expansion: F-split, G-collapse, M-add, BEAT_BOUND, runner discount | Threshold matrix classifier. | Plausible under steelman if the classifier is table-driven. | The 12-step classification order + per-corpus + per-runner discount + per-track sub-band is ~60-80 LOC just for the cascade; plus matrix-row data ~80 LOC; plus per-corpus rendering ~100 LOC. 350 LOC is borderline-tight. | Hedge to ≤ 400; or reclaim from removing runner-discount logic (Lens I REINVENT). | KEEP-with-budget-hedge |

## §6 Punch list (ordered surgical edits)

| # | Site | Surgery | Lens | Owner | Scope |
|---|---|---|---|---|---|
| 1 | BENCH.md:1347 | Replace `<A|B|C|D|E|F|G|H|I|J|K|L>` with `<A|B|C|D|E|F-positive|F-noise|G|I|J|K|L|M>` (or current outcome set). | Lane 3 cohesion / Lens M | BENCH author | Editorial |
| 2 | BENCH.md:675 (§6.3) | Replace "Outcomes G and H exist" with "Outcomes G/I/J/K/L/M exist". | Lane 3 cohesion | BENCH author | Editorial |
| 3 | BENCH.md:651-665 (§6.2.1) | Add F-codegen-gap row OR consolidate F-positive/F-noise/F-codegen-gap into single F outcome with Track 1 sub-band reported in action text. Close the structural gap at Track 2 ∈ (S × 1.05, S × 1.10] AND Track 1 > Track 2 × 1.10. | Lens M (load-bearing) | BENCH author | Matrix |
| 4 | BENCH.md:620 (F-noise rationale) | Replace "criterion `noise_threshold(0.02)` plus 5% headroom" with measurement-driven boundary: "Track 1 95% confidence interval upper bound overlaps Track 2 × 1.05". | Lens M (load-bearing) | BENCH author | Threshold |
| 5 | BENCH.md:1043-1066 (§7.8.3) | Replace `core::arch::aarch64::__dsb` with the correct cache-evict primitive (`dc civac` loop or `clear_cache` syscall + buffer pressure). For x86_64, document the `_mm_clflush` 64-byte stride loop over corpus + parser hot-data ranges. Add explicit qualifier: "TLB and branch-predictor state are not cooled by this probe; the cold/warm ratio reported is dCache + iCache delta only". | Lens H (provenance + correctness) | BENCH author | Probe definition |
| 6 | BENCH.md:984-988 (§7.8.1 Probe A) | Replace "canonically ~10-30 ns" with "M1 Pro virtual call ~5-10 ns; bounds-check + registry lookup adds ~20-40 ns; total per registry call ~30-50 ns" (or cite a microbench source). | Lens F (LLM bias / pseudo-precision) | BENCH author | Editorial-with-rationale |
| 7 | BENCH.md:1043-1066 (§7.8.3 < 1.2× branch) | Add explicit qualifier: "If the eviction primitives in this probe are insufficient for the platform, the < 1.2× cold/warm ratio fires the suspicious branch automatically and the row is recorded as inconclusive." | Lens H | BENCH author | Editorial |
| 8 | BENCH.md:1216-1245 (§9.6 — peak RSS forward-projection) | Add forward-projection calculation: "tape (8-byte tokens × ~280K offsets on canada) ≈ 2.24 MB; payload arena empty on hot path; typed root ≈ 3-5 MB; total ~5-7 MB ≈ 1× sonic-rs canada peak. M outcome's 3× threshold is a safety net for substrate redesign drift, not a primary gate." | Lens M | BENCH author | Editorial |
| 9 | BENCH.md:1466-1473 (§10.3 probability mapping) | Add row for "alternate_pext_mask_plan < canonical × 0.90 on x86_64" → MASKING: cross-platform plan divergence; V1-SOTA-beat probability on Intel line drops by 0.10-0.20 against M1 Pro line. | Lens L (load-bearing) | BENCH author | Probability mapping |
| 10 | BENCH.md:1130 (§1.2) | Add cross-reference: "Track 2's substrate access is via `TapeBuilder<'a>` per `SUBSTRATE.md` §8 — the named-inversion contract that V1 graduation closes (per `INDEX.md` deviation ledger row 6)." | Lane 1 lock-adherence + Lens N | BENCH author | Cross-ref |
| 11 | BENCH.md §11.1 ↔ WORKSPACE.md:73 | WORKSPACE row 9 (`bbnf-bench` skinny budget 2,000 LOC with `Track 2 handwritten parser ≤500`) inconsistent with BENCH redress (Track 2 measurement-driven 800-1,500 LOC + CSS prior optional 600 LOC). Reconcile WORKSPACE row 9 to ~3,500 LOC; recompute INDEX.md "31,400 LOC handwritten" to ~32,500-33,000. | Lane 6 (LOC budget) + Lens N | WORKSPACE + INDEX authors | Cross-quadrant |
| 12 | BENCH.md:996-998 (§7.8.1 Probe B band rationale) | Clarify the eager-decode bands measure dispatch-overhead-on-eager-work, with eager-work itself being a fixed ~40-80% of twitter parse time. State explicitly: "the 5-15% twitter band is the additional cost of registry-routed eager decode atop the eager-decode work itself, not the eager-decode work cost". | Lens L | BENCH author | Editorial |
| 13 | BENCH.md §8.3-§8.4 (CI runner discount) | Collapse to "CI bench is advisory non-gating; local bench is authoritative". Remove `runners.toml`. Reclaim ~50 LOC from gate.rs budget. | Lens I (over-engineering) | BENCH author | Apparatus reduction |
| 14 | BENCH.md:1573-1575 (LOC budget hedge) | Hedge `metadata.rs ≤ 280` (was 250); `gate.rs ≤ 400` (was 350); reclaim 50 LOC if Lens-I item 13 lands. | Lane 6 | BENCH author | Budget hedge |
| 15 | BENCH.md §9.5 cross-ref to §7.8.2 | Add: "The cost-model dispatch (Pratt vs SIMD) is partially bounded by §7.8.2 alternate-plan probes — specifically alternate_pext_mask_plan tests one plausibly-better cost-model output on x86_64." | Lane 8 (carry) | BENCH author | Cross-ref |

## §7 Lane verdict line totals

- Lane 1 (Lock-Adherence): 11 KEEP, 1 REINVENT, 0 DISCARD.
- Lane 2 (Sequencing): N/A.
- Lane 3 (Cohesion): 0 REINVENT, 2 DISCARD (line-level editorial).
- Lane 4 (SOTA Anchoring): 8 KEEP, 1 REINVENT (F-noise rationale).
- Lane 5 (Grammar-Authoritative): honoured.
- Lane 6 (LOC Budget): 5 KEEP, 2 REINVENT (WORKSPACE reconciliation; metadata.rs/gate.rs hedge).
- Lane 7 (Friction): honoured.
- Lane 8 (Carry/Deferral): 7 KEEP, 1 REINVENT (cross-ref).
- Lane 9 (Greenfield): honoured.
- Lens F: 1 REINVENT (pseudo-precision in Probe A).
- Lens G: KEEP (CSS prior probe is the load-bearing anti-overfit lever).
- Lens H: 2 REINVENT (cold-cache primitive correctness).
- Lens I: 1 REINVENT (CI runner discount).
- Lens J: honoured.
- Lens K: honoured.
- Lens L: 1 REINVENT (probability-mapping for x86_64 plan divergence) + 1 KEEP (host-call probes).
- Lens M: 2 REINVENT (classification gap; F-noise rationale).
- Lens N: honoured.

Counts (selected rows): 32 KEEP, 12 REINVENT, 2 DISCARD (line-level editorial).

KEEP fraction: ~70% (within 60-80% target band; healthy mix).

## §8 Final readiness

> **Decision: SK-AMENDMENT-REQUIRED-NARROW**
>
> The redress lands the load-bearing improvements: BEAT_BOUND construction closes the prior outcome-A ratification hole; M outcome upgrades memory residency from report-only to gated NO-GO; G+H collapse eliminates a redundant matrix row; per-corpus SIMD parity hashes and canada-binding throughput floor sharpen the substrate-correctness gate; the host-call probe split into dispatch-overhead + eager-decode tests two distinct masking modes with defensible per-corpus bands; the §9.1 CSS prior probe is the strongest anti-overfit lever in the post-redress spec.
>
> The amendments needed are narrow and editorial-to-mechanical, not architectural. The dominant defects are: (a) two stale H-outcome references (§6.3 line 675 and §10.2 line 1347 outcome-ID enumeration); (b) a structural classification-order gap at Track 2 ∈ (S × 1.05, S × 1.10] AND Track 1 > Track 2 × 1.10 (Lens M load-bearing); (c) the F-noise rationale is hand-waved against the criterion noise threshold (Lens M load-bearing); (d) the §7.8.3 cold-cache eviction primitives are technically wrong for aarch64 (`__dsb` is a barrier, not eviction) and incomplete for x86_64 (no stride loop, no TLB/branch-predictor cooling); (e) the §1.2 Track 2 contract does not name `TapeBuilder` though SUBSTRATE.md §8 commits to it as the API; (f) the §11.1 LOC budget exceeds WORKSPACE.md:73 row 9 by ~1,000-1,500 LOC due to the Track 2 measurement-driven model; (g) one Lens-L MASKING signal (alternate_pext_mask_plan x86_64 inverted dominance) does not propagate to the §10.3 probability mapping.
>
> Lens M (the load-bearing falsifiability lens) returns honoured-with-narrow-amendment. The matrix has multiple honest NO-GO branches the skinny could plausibly land in. The classification order has a real but mechanically-closable gap; the F-band threshold derivation needs sharpening. Lens L returns FAITHFUL-with-band-clarification on the host-call cut and FAITHFUL-with-MASKING-propagation on the cost-plan cut. Lens H returns AMENDMENT-REQUIRED-NARROW on cold-cache primitive correctness — fixable with one paragraph rewrite.
>
> Hereupon: dispatch the SK-V2 amendment agent with the §6 punch list (15 surgical items, all narrow); BENCH.md re-runs through SK-V2 audit before SKINNY-SUITE consolidation. Cross-quadrant amendment: WORKSPACE.md:73 must be reconciled to the Track 2 measurement-driven LOC model, and INDEX.md headline LOC total must update accordingly.
