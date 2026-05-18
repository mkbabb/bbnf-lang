# CH5 — HIDDEN COUPLING disposition for SK-V9 S-P1 V4 cohort

Pass: S-P1 Profile. Cycle: V4. Lens: CH5 HIDDEN COUPLING.
Date: 2026-05-18.
Subjects: `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-{A..F}.md`
(V4-folded in place per `HARDENING-S-P1-V3-CONSOLIDATED.md` F1–F6).
Output: this file.

CH5 verifies, per `restart/prompts/ORCHESTRATOR.md` §3W: no proposal
introduces a parallel substrate, a sidecar producer, a renamed-scanner
Lock-1 violation, or Track 1 ≡ Track 2 dishonesty; the substrate union
holds. The non-negotiables enforced are `LOCKS.md` Lock 1 ("Tape is the
substrate, properly unioned with direct-to-struct; … orthogonal codepaths
and parallel substrates are dead … A SIMD mask stream is a transient
producer, not a retained sidecar; if structural offsets are retained, the
structural projection IS the tape.") and Lock 14 (substrate carries ZERO
grammar-specific code), against the ledger of `skinny/REDRESS.md` 50–72
sidecar/projection rejections plus the SK-V8 W3 union rejection at
REDRESS 92.

V3 CHALLENGE CH5 returned **95.6% ACCEPT** (43/45) with two REVISE
dispositions (D.4 §6.1 cardinality ambiguity; D.5 §6.1 escape-complete
pass-count) and one neighbour-tagged ACCEPT (A.6 PMU manifest schema
non-consumed). The V4 fold per `HARDENING-S-P1-V3-CONSOLIDATED.md` F6
adds Lock-1 binding sentences targeting precisely D §6.1; F2 cross-folds
A's PMU manifest §6.5 to diagnostic-only status. This V4 re-review
verifies the fold landed and audits for any new Lock-1 leaks introduced
by the surgical edits.

## §1 — V3-disposition-resolution (REVISE fold check)

### §1.1 — V3 CH5 REVISE 1 (D.4, §6.1 cardinality) — FOLDED

V3 read of D §6.1 reported the "ships the string-plane masked bitmap +
deferred escape-complete" single-knob wave proposal as
*cardinality-ambiguous*: a reader could not tell whether the masked
bitmap **replaced** the existing scalar `match_tiny_plain_string_with_cap`
+ SIMD `match_string_at_quote_trusted_utf8` pair (admissible — singular
substrate), or **ran alongside** them as a retained mask that some
downstream consumer reads (forbidden — REDRESS 50/53/61/62/83 class +
SK-V8 W3 union shape REDRESS 92 rejection class).

V4 fold lands at `skv9-p1-v3-D-structural-breakdown.md:477-492` as a
new REDRESS material differential note tagged "(F3, CH3 D-1; CH5 §4.1,
F6)". The fold sentence reads verbatim:

> A candidate intervention on this finding REPLACES the existing
> string-scanner pair on the production hot path —
> `match_tiny_plain_string_with_cap` at
> `runtime/src/grammars/json/generated.rs:171-185` and
> `match_string_at_quote_trusted_utf8` at `parse-that-regex/src/lib.rs` —
> running alongside the existing scanner constitutes a sidecar producer
> and fails Lock 1 (substrate cardinality stays at one; per `LOCKS.md`
> Lock 1 a "SIMD mask stream is a transient producer, not a retained
> sidecar").

Verdict: **FOLDED — REVISE 1 closed**. The language is now unambiguous:
(a) the word REPLACES is in caps so a downstream scanner cannot read it
as "augments"; (b) the prior scalar + SIMD-fallback paths are named at
file-line precision (`runtime/src/grammars/json/generated.rs:171-185`
and `parse-that-regex/src/lib.rs`); (c) the run-alongside case is named
and tagged as a Lock-1 fault by explicit citation, not implication; (d)
the verbatim `LOCKS.md` Lock 1 quote ("a transient producer, not a
retained sidecar") binds the disposition by spec citation rather than
prose paraphrase. The REDRESS 60/61/62/83/84 class pre-block list and
the REDRESS 64 unicode validator pre-block both carry forward.

### §1.2 — V3 CH5 REVISE 2 (D.5, §6.1 escape-complete pass-count) — FOLDED-BY-DELETION

V3 read of D §6.1 reported the "defer the escape-complete check to a
flaw probe rather than running it inline" framing as
*pass-count-ambiguous*: a two-pass architecture over the same bytes is
the parser-local-cursor shape REDRESS 53 explicitly rejected.

V4 fold inspects the surgical edit at `skv9-p1-v3-D-structural-breakdown.md`
§6.1: the prior "single-knob wave: ships the string-plane masked bitmap
+ deferred escape-complete" sentence is **deleted**. The §6.1 body
under V4 no longer authors a wave at all — per F1's "wave authorship
deferred to S-P3" disposition the section is reframed as a *diagnostic
finding* ("Parse_only LOSS-block finding: per-string-span-delimiter
cost dominates"). The "deferred escape-complete check" phrase no longer
appears in §6.1; the only surviving mention of escape-complete in the
report is at §5.3 line 435 ("escape-complete scan (per-byte branch over
`b'\\'` and `<0x20`)") as one of three named hot-leaf candidates inside
the per-string-span-delimiter cost class — i.e. inside the **same**
delimiter-driven scan, not as a separate pass.

Verdict: **FOLDED-BY-DELETION — REVISE 2 closed**. The pass-count
ambiguity is resolved by removing the offending two-pass framing rather
than by adding a `#[cfg(test)]` qualifier. The single surviving
mention names the escape-complete check as a per-byte branch inside the
existing per-delimiter scan — i.e. inline within whatever pass the
delimiter scanner runs, not as a deferred secondary pass. This is the
admissible reading (i) of the V3 surgery proposal: inline within the
same scan that walks the bytes once. REDRESS 53's parser-local-cursor
two-pass rejection is not re-opened.

Note on fold style: the F6 packet (line 138-139 of
`HARDENING-S-P1-V3-CONSOLIDATED.md`) named the admissible options as
"(i) inline within the same SIMD pass OR (ii) strictly `#[cfg(test)]`
diagnostic-only". The V4 author chose option (i) implicitly by
deleting the "deferred" framing and folding the escape-complete check
into the same per-delimiter cost class as one of three branches the
delimiter scanner already executes (alongside view-boundary UTF-8
validation and structural-emit handshake). Option (i) is materially
honoured; the qualifier "inline within the same SIMD pass" is not
spelled in the §6.1 fold because §6.1 no longer proposes a SIMD pass
at all under F1 (wave authorship deferred). This is the
*subtractive* form of the fold — admissible per CH5 because removing
the proposal removes its coupling risk surface.

### §1.3 — V3 CH5 A.6 (PMU manifest gate-consumer wiring) — FOLDED

V3 read of A §6 reported the PMU manifest schema at
`/tmp/skv9-xctrace-v3/pmu_rows.tsv` as fresh evidence with no committed
`gate-json` consumer in-wave; the disposition was ACCEPT-with-rider but
flagged as "trending toward sidecar" if left unaddressed.

V4 fold lands at `skv9-p1-v3-A-xctrace-cpu-counters.md:421-439` as a
new §6.5 paragraph titled "PMU manifest status — diagnostic profile
evidence, non-producer". The fold sentence reads verbatim:

> The per-row PMU manifest at `/tmp/skv9-xctrace-v3/pmu_rows.tsv` is
> diagnostic profile evidence; it does not participate in admission
> gates and does not extend `RESULTS.md` schema. The manifest is a
> profiling artefact emitted by the read-only `xctrace_probe` binary
> and consumed only by S-P1 / S-P2 narration of cycle-cost
> decomposition. No `gate-json` or other admission-gate consumer
> ingests this TSV; the SK-V9 `gate-json` consumer named in
> `PASS-1-PROFILE.md` §2 continues to operate against the existing
> `RESULTS.md` Mbps + Δ columns unchanged. Per `LOCKS.md` Lock 1 ("a
> transient producer, not a retained sidecar") and the §3W "Same-wave
> consumer — no orphan kernel" non-negotiable, this manifest is bound
> to characteriser status: it informs hot-leaf cycle-cost narration in
> S-P1 and S-P2, but never becomes a route-fact substrate.

Verdict: **FOLDED**. The fold takes option (ii) from the V3 surgery
list: "explicitly tag the manifest as `diagnostic-only,
never-a-producer`" rather than option (i) ("commit a stable in-repo
manifest path plus the `gate-json` reader in the same wave"). This is
the CH5-conservative choice — binding the artefact to non-producer
status by spec citation pre-empts any later wave re-using the manifest
as a route-fact substrate without reopening the SPEC. The Lock-1
"transient producer, not a retained sidecar" clause is cited verbatim,
matching F.4's umbrella binding. The fold also defers the "stable
in-repo manifest path" option to a future wave (with explicit "if a
later wave wishes to gate on cycles/B, it must …" guard), so the
characteriser binding remains the default until a same-wave consumer
proposal lands.

## §2 — V4 dispositions (≥30 entries)

Five or more entries per report, ≥30 total. Each entry: (report, locus,
finding, verdict ∈ {ACCEPT, REVISE, REJECT}, citation, surgery if
REVISE).

### §2.1 — P1-V3-A V4 (xctrace CPU Counters)

| # | Locus | Finding | Verdict | Citation | Surgery |
|---:|---|---|---|---|---|
| A.1 | §6.5 PMU manifest binding (new) | The new §6.5 paragraph binds the PMU manifest to diagnostic profile evidence status, with explicit non-`gate-json` consumer disposition. Verbatim `LOCKS.md` Lock 1 quote ("a transient producer, not a retained sidecar") + §3W "Same-wave consumer — no orphan kernel" cited. The manifest cannot drift into substrate without a spec amendment. Lock-1 cardinality discipline rendered explicit. | ACCEPT | `skv9-p1-v3-A-xctrace-cpu-counters.md:421-439`. | n/a |
| A.2 | §6.5 future-wave guard clause | The fold names the option for a later wave to "commit a stable in-repo manifest path … and a matching `gate-json` reader in the same wave" — explicitly bound to same-wave-consumer discipline. This pre-empts a partial migration where the manifest ships before its consumer. CH5-positive guard. | ACCEPT | `skv9-p1-v3-A-xctrace-cpu-counters.md:435-439`. | n/a |
| A.3 | §0 V4 fold footer attribution | The footer at §0 cites CH5-A6 / consolidated F6 as the disposition source for the new §6.5 paragraph. The fold provenance is traceable. No new producer; just a binding sentence. | ACCEPT | `skv9-p1-v3-A-xctrace-cpu-counters.md:470-474`. | n/a |
| A.4 | §1.1 probe binary (unchanged from V3) | The `xctrace_probe` binary remains a read-only probe with no substrate side-effect; the V4 fold does not extend its surface or wire it into any production caller. V3 ACCEPT (A.1 of V3) carries forward unchanged. | ACCEPT | `skv9-p1-v3-A-xctrace-cpu-counters.md:25-46, 110-124, 342-346`. | n/a |
| A.5 | §1.1 dual-track invocation (unchanged) | The probe still takes `<track:track1\|track2>` as a switch, never coalescing the two tracks. Track 1 ≡ Track 2 dishonesty is positively guarded by per-process-launch isolation. V3 ACCEPT (A.2) carries forward. | ACCEPT | `skv9-p1-v3-A-xctrace-cpu-counters.md:30-39, 137-170`. | n/a |
| A.6 | §1.3 / §4 / §5 / §7 TP path citation correction | V4 fold corrected `p1a-time-profile/` → `p1b-tp/` per consolidated F5 (CH6-A3). This is a path-citation fix, not a substrate change; the TP traces are P1-V3-B's artefact and remain diagnostic-only. No new producer. | ACCEPT | `skv9-p1-v3-A-xctrace-cpu-counters.md:319, 449, 467-468`. | n/a |
| A.7 | §1.1 corpus-name canonical mapping note | V4 fold adds a note explaining the `update_center` ↔ `update-center.json` hyphen/underscore shear so downstream aggregators resolve. Naming reconciliation, not substrate change. | ACCEPT | `skv9-p1-v3-A-xctrace-cpu-counters.md:469-470`. | n/a |
| A.8 | §4 closing bullets — samply residual removal | The CH1-A8 / CH6-A1 fold removed the samply-coalescing residual on twitter / y_string_unicode and cited P1-V3-B symbol shares as authoritative attribution. Subtractive — removes a falsified attribution claim; does not install a new producer. | ACCEPT | `skv9-p1-v3-A-xctrace-cpu-counters.md:464-467`. | n/a |

### §2.2 — P1-V3-B V4 (xctrace Time Profiler)

| # | Locus | Finding | Verdict | Citation | Surgery |
|---:|---|---|---|---|---|
| B.1 | §1.1 sampling-rate / template separation (unchanged) | Two trace streams under separate template names remain independent diagnostic streams. V3 ACCEPT (B.1) carries forward. | ACCEPT | `skv9-p1-v3-B-xctrace-time-profiler.md:29-50`. | n/a |
| B.2 | §3.1 `scan_structurals` 0.00% non-fusion (unchanged) | B continues to name the SIMD scan symbols as non-producers, present only under synthetic probes. The disposition remains subtractive (delete the producer) rather than additive (wire a consumer). V3 ACCEPT (B.4) carries forward; F1's "wave authorship deferred" reinforces the subtractive stance. | ACCEPT | `skv9-p1-v3-B-xctrace-time-profiler.md:614-651`. | n/a |
| B.3 | §3.1 dual-callsite `simd_movemask` (unchanged) | B's exposure of the one-primitive-name-two-callsites case remains the *opposite* of a renamed-scanner Lock-1 violation. V3 ACCEPT (B.5) carries forward. | ACCEPT | `skv9-p1-v3-B-xctrace-time-profiler.md:643-650`. | n/a |
| B.4 | §1.5 aggregator classifier grammar-neutrality | The 8-class taxonomy continues to use grammar-neutral primitive names (`string_tiny_scan`, `number_digit_scan`, `scan_structurals`, etc.). The V4 fold does not introduce JSON-role names; classifier vocabulary is the canonical naming per F4. Lock 14 honoured. | ACCEPT | `skv9-p1-v3-B-xctrace-time-profiler.md:91-122`. | n/a |
| B.5 | §1.1 / §6 — corpus path reconciliation | The reproduce script materialises `corpus_paths.txt` ephemerally under `/tmp/skv9-xctrace-v3/`; no retained sidecar artefact committed under `runtime/` or `crates/bbnf-simd/`. The V4 fold does not change this disposition. | ACCEPT | `skv9-p1-v3-B-xctrace-time-profiler.md:896-955`. | n/a |
| B.6 | §3.2 "75%" revision (unchanged) | B's revision of the SC-4 "75%" framing remains attributional ("tiny scalar path dominates"); no new SIMD producer proposed. The unicode-escape codec is named as a distinct primitive class for S-P2 enumeration — a vocabulary disposition, not a substrate write-path. V3 ACCEPT (B.9) carries forward. | ACCEPT | `skv9-p1-v3-B-xctrace-time-profiler.md:653-720`. | n/a |
| B.7 | §1.2 probe-binary reuse (unchanged) | The same `xctrace_probe` binary still serves both A and B captures; one probe, one binary. V3 ACCEPT (B.2) carries forward. | ACCEPT | `skv9-p1-v3-B-xctrace-time-profiler.md:51-63`. | n/a |

### §2.3 — P1-V3-C V4 (Per-Corpus Deep Hot-Leaf Attribution)

| # | Locus | Finding | Verdict | Citation | Surgery |
|---:|---|---|---|---|---|
| C.1 | §1.2 Track 1 / Track 2 disambiguation (F2 refold) | C's refold per F2 ("Re-execute P1-V3-C with on-disk A/B inputs as primary; samply cross-validation only") preserves the parser-vs-oracle distinction. The samply mode-I baseline is "retained only as the V2 reference column" per the refold body. Track 1 ≡ Track 2 dishonesty remains positively guarded. | ACCEPT | `skv9-p1-v3-C-hot-leaf-attribution.md:29-61, 427-433, 868, 898`. | n/a |
| C.2 | §1.3 attribution classifier (unchanged) | The 8-class taxonomy remains grammar-neutral. The "traversal_other" conservative bucket continues to refuse split-claim attribution. Lock 14 honoured. V3 ACCEPT (C.2) carries forward. | ACCEPT | `skv9-p1-v3-C-hot-leaf-attribution.md:63-82`. | n/a |
| C.3 | §4 structural-scan non-fusion (unchanged + F2 refold) | C's "the SIMD scan symbols are non-producers, present only under synthetic probes" disposition remains subtractive. F2's refold cross-cites P1-V3-B's symbol shares as primary, not P1-V3-C's V2 samply mode-I. | ACCEPT | `skv9-p1-v3-C-hot-leaf-attribution.md:234-287`. | n/a |
| C.4 | §1.4 V2 → V4 refold provenance | The refold preserves V3 disposition tags; the samply baseline retention is explicit ("retained only as the V2 reference column"), not coalesced into V3 PMU truth. No Track 1 ≡ Track 2 collapse via refold drift. | ACCEPT | `skv9-p1-v3-C-hot-leaf-attribution.md:868, 898`. | n/a |
| C.5 | §recommendations (F1 strip) | C's recommendations no longer pre-empt S-P3 wave-class authoring; F1 strip removes the wave-ranked language. The §6 cross-validation findings (Pearson r = +0.720) remain attribution-only. No additive consumer-wiring proposed. | ACCEPT | `skv9-p1-v3-C-hot-leaf-attribution.md:289-415`. | n/a |

### §2.4 — P1-V3-D V4 (Structural-Element Counts vs Throughput)

| # | Locus | Finding | Verdict | Citation | Surgery |
|---:|---|---|---|---|---|
| D.1 | §6.1 cardinality binding (F6 fold) | The new REDRESS material differential note explicitly names the production hot path scanner pair and binds REPLACES vs running-alongside disposition. Verbatim `LOCKS.md` Lock 1 quote cited. REVISE 1 of V3 closed. | ACCEPT | `skv9-p1-v3-D-structural-breakdown.md:477-492`. | n/a |
| D.2 | §6.1 escape-complete pass-count (F6 fold by deletion) | The "deferred escape-complete" two-pass framing is removed; the surviving §5.3 mention names escape-complete as a per-byte branch *inside* the per-delimiter scan, not as a separate pass. Admissible reading (i) of the F6 surgery — inline within the same scan. REVISE 2 of V3 closed. | ACCEPT | `skv9-p1-v3-D-structural-breakdown.md:435, 469-492`. | n/a |
| D.3 | §6.1 REDRESS class-pre-block citations | The §6.1 fold cites REDRESS 60/61/62/83/84 (string-scanner-widening class) + REDRESS 64 (retained Unicode-escape run validator) by number with shape labels in parens. The class pre-block surface is broadened, not narrowed. CH5-positive. | ACCEPT | `skv9-p1-v3-D-structural-breakdown.md:485-489`. | n/a |
| D.4 | §6.2 unicode-row REDRESS citations | The §6.2 fold cites REDRESS 82 (four-`\uXXXX` AArch64 classifier rejection) + REDRESS 59 (UTF-8 fusion class rejection) with row-precise scope. Same-row falsification gate required for any successor; wave authorship deferred. | ACCEPT | `skv9-p1-v3-D-structural-breakdown.md:501-506`. | n/a |
| D.5 | §6.4 digest-sink class closure | "The digest-sink-redesign class is closed by REDRESS 66–69 + 93; any further direct-plane work routes to a dedicated direct-output-contract or control-path tranche." This re-frames W2 from "redesign the digest producer" to "profile the digest path" per F3. Subtractive disposition. | ACCEPT | `skv9-p1-v3-D-structural-breakdown.md:519-528`. | n/a |
| D.6 | §6.5 typed-plane no-substrate-change (unchanged) | "No new `BackendShape` is proposed; per CH5 §4.5 the substrate union holds." D explicitly forbids a substrate change on the typed plane. V3 ACCEPT (D.7) carries forward. | ACCEPT | `skv9-p1-v3-D-structural-breakdown.md:530-536`. | n/a |
| D.7 | §6.6 wave authorship deferral (F1) | "Wave-class selection and per-wave cost set … are S-P3 scope." The §6.6 fold per F1 removes the V3 "three V9/V10 waves, ranked" pre-emption of S-P3. The diagnostic-vs-wave boundary is now spec-bound. | ACCEPT | `skv9-p1-v3-D-structural-breakdown.md:538-543`. | n/a |
| D.8 | §5.4 structural-element plane unchanged | "Structural elements cost **nearly free under the lazy tape** … The structural-element plane is not the bottleneck." Existing `OffsetTape` substrate preserved as the WIN driver. V3 ACCEPT (D.8) carries forward. | ACCEPT | `skv9-p1-v3-D-structural-breakdown.md:449-454`. | n/a |
| D.9 | §5.5 numeric-token plane unchanged | "Numeric tokens are **net free or net beneficial**. The numeric-token FSM is bbnf's currently strongest sub-plane and needs no immediate work." No new producer. V3 ACCEPT (D.3) carries forward. | ACCEPT | `skv9-p1-v3-D-structural-breakdown.md:456-461`. | n/a |

### §2.5 — P1-V3-E V4 (Legacy Cleanup Audit, E1 + E2 split per F5)

| # | Locus | Finding | Verdict | Citation | Surgery |
|---:|---|---|---|---|---|
| E.1 | §2.1 x86_64 orphan SIMD kernels — subtractive | 14 `unimplemented!()` shells continue to be flagged SAFE-TO-DELETE per REDRESS 50-55 admission rule + Lock 16. The V4 fold adds **primitive-class status** column (CH2-E1 fold): 12 × N/A (placeholders, never admitted); 2 × REJECTED-CLASS (avx512_vpclmul, avx_ifma). Deletions remain strictly subtractive — removing producers, not adding consumers. | ACCEPT | `skv9-p1-v3-E-legacy-cleanup-audit.md:188-213`. | n/a |
| E.2 | §2.2 aarch64 NEON `match_tiny_plain_string` distinction (F4 fold) | The V4 fold adds an explicit "**Critical distinction**" subordinate clause: deleting the NEON kernel at `bbnf-simd/src/aarch64/match_tiny_plain_string.rs` does NOT touch the admitted scalar `match_tiny_plain_string_with_cap::<16>` at `runtime/src/grammars/json/generated.rs:171-185`. The two surfaces are independent; one is removed, the other stays. This is precise cardinality accounting — the renamed-scanner Lock-1 risk surface is removed, not instantiated. | ACCEPT | `skv9-p1-v3-E-legacy-cleanup-audit.md:219`. | n/a |
| E.3 | §2.2 R1 `string_block::scan_string_special_block` KEEP-IF-USED | The LIVE consumer chain `parse-that-regex/src/lib.rs:472, 551` → `match_string_at_quote_trusted_utf8` → `runtime/src/grammars/json/generated.rs:193` is preserved. F4 fold clarifies REDRESS 61/62/83 rejected a *different surface* (retained-generated trusted-string wrapper); the live UTF-8-validating consumer is a separate admitted shape. No hidden sidecar. | ACCEPT | `skv9-p1-v3-E-legacy-cleanup-audit.md:220, 388-390`. | n/a |
| E.4 | §2.3 utility orphans — primitive-class column (F4) | The V4 fold adds `corpus-scoped` vs `REJECTED-CLASS` columns per F4 ("don't masquerade SAFE-TO-DELETE as class-retiring"). quad_load / byte_context / cache_hints are corpus-scoped (class survives, only this wiring deletes); digit_mac is REJECTED-CLASS per REDRESS 80. The disposition is no longer ambiguous between "delete the wiring" and "retire the class". | ACCEPT | `skv9-p1-v3-E-legacy-cleanup-audit.md:222-230`. | n/a |
| E.5 | §2.4 LIVE primitives + `aarch64::unescape_uxxxx` disambiguation | The LIVE consumer for `unescape_uxxxx` (`parse-that-regex/src/lib.rs:402, 419`) is the materialization path (4-unit packed decode), distinct from REDRESS 64+82's rejected single-quartet retained validator route. The F4 fold reframes the class as `escape_codec_hex_unit` parameterised by `{hex_digit_count, surrogate_join_policy, terminator_policy}` — admits CSS L4 `\HHHHHH` as a sibling instantiation. CH2-grammar-neutral. | ACCEPT | `skv9-p1-v3-E-legacy-cleanup-audit.md:245, 393-394`. | n/a |
| E.6 | §2.7 simd-scan/ fossil dir (F5 fold) | V4 fold corrects the V3 prose: "simd-scan/ was removed in the SK-V5 NUKE-PLAN; no current path." Subtractive — the no-op cleanup is acknowledged rather than asserted. No new substrate language. | ACCEPT | `skv9-p1-v3-E-legacy-cleanup-audit.md:257-260`. | n/a |
| E.7 | §1.3-§1.8 doc-corpus ARCHIVE-MOVE (E1 dispatch) | The 524-file ARCHIVE-MOVE is hygiene; REDRESS path citations preserved via `git mv` (no path-rewrite inside archived files). The E1 dispatch is doc-only, ≤30 min, no `cargo test` gate. No coupling-enforcing audit destroyed. | ACCEPT | `skv9-p1-v3-E-legacy-cleanup-audit.md:30-128, 335-345`. | n/a |
| E.8 | §2 E2 dispatch — gate validation discipline | E2 carries the mandatory `cargo test --workspace --profile ax-iter` + `xtask check-json` / `check-real-typed` / `check-conformance` gate after each per-ISA commit. The per-ISA granularity isolates revert blast radius. CH5-positive: deletions are gated, not free. | ACCEPT | `skv9-p1-v3-E-legacy-cleanup-audit.md:162-174, 346-349`. | n/a |
| E.9 | §2.8 code-triage rollup — total reduction | ~700 src + ~160 test LOC removal, all subtractive. Surviving aarch64 LIVE primitives (~17 files) are Lock 16 admitted; the deletion ledger does not touch them. The KEEP-IF-USED row for `string_block` / `movemask` carries a verify-consumer-chain disposition rather than a blind delete. | ACCEPT | `skv9-p1-v3-E-legacy-cleanup-audit.md:262-272`. | n/a |

### §2.6 — P1-V3-F V4 (REDRESS Reconciliation)

| # | Locus | Finding | Verdict | Citation | Surgery |
|---:|---|---|---|---|---|
| F.1 | §1.2 xctrace contract admission (unchanged) | F's adjudication of xctrace `cpu-counters` as "a direct hardware-counter read through Apple Silicon's PMU via kernel `kpc` APIs" stands. The discipline boundary (forbidden: `ns_per_byte` → c/B inference) is preserved. V3 ACCEPT (F.1) carries forward. | ACCEPT | `skv9-p1-v3-F-redress-reconciliation.md:63-93`. | n/a |
| F.2 | §2 REDRESS ledger STILL-LOAD-BEARING tags | All CH5-relevant rejections (REDRESS 50, 51, 53, 60-65, 82-84, 92, 93) remain tagged STILL-LOAD-BEARING. The V4 fold does not silently re-tag any pre-block. The §2.6 block 6 entries (REDRESS 60-65) preserve the granularity. | ACCEPT | `skv9-p1-v3-F-redress-reconciliation.md:200-310`. | n/a |
| F.3 | §2 strictness-plane assertion (F5 fold) | The strictness-plane assertion at §2 lines 126-140 is now explicit: every comparator delta sourced from `strictness=strict, freshness=same-run-native` per the `SK-V9-open` manifest; sidecar/permissive/`utf8_lossy` rows treated as flaw-probe artefacts only, never as behavior-admission ancestors. Lock-1 cardinality discipline rendered explicit. | ACCEPT | `skv9-p1-v3-F-redress-reconciliation.md:126-140`. | n/a |
| F.4 | §3.2 four class umbrellas (preserved verbatim) | F.3's four class umbrellas (string-scanner widening; direct receiver / scratch / semantic-fact; bench-private hand Track 1 / hand typed sink; PMU / cycles / Criterion-slope / masking / structural-scan as producer) are preserved. Each umbrella broadens the pre-block surface and cites REDRESS entries by number. V3 ACCEPT (F.3) carries forward. | ACCEPT | `skv9-p1-v3-F-redress-reconciliation.md:339-365, 406-447`. | n/a |
| F.5 | §3.1 REDRESS 92 W3 mapping (unchanged) | F's HANDOFF §5 item 3 ↔ REDRESS 92 mapping is faithful: the W3 union "must replace, must not run alongside" requirement carries forward verbatim. F does not weaken the W3 fit-gate language. V3 ACCEPT (F.5) carries forward. | ACCEPT | `skv9-p1-v3-F-redress-reconciliation.md:308-311, 385-386`. | n/a |
| F.6 | §3.3 PMU / cycles umbrella | The umbrella ("PMU, cycles, instructions, Criterion-slope, structural-scan-as-producer surfaces are not consumed as Track 1 / Track 2 / typed / direct / strict producers; if the gate later wants cycles/B, it must wire a same-wave Mbps-isomorphic comparator that resists the strict-vs-permissive flaw-probe gate before consuming PMU evidence too") explicitly closes the door on V3 PMU evidence becoming an admission producer. | ACCEPT | `skv9-p1-v3-F-redress-reconciliation.md:432-440`. | n/a |
| F.7 | §4.1 Edit F — SPEC clarifier (F6 fold cross-cite) | "V3 real-PMU c/B is a diagnostic characteriser of hot leaves, not a producer; it does not enable any behavior admission path that was blocked in V2." The clarifier matches A's §6.5 binding; the two surfaces (SPEC + report) are isomorphic. | ACCEPT | `skv9-p1-v3-F-redress-reconciliation.md:457-468`. | n/a |
| F.8 | §6.5 umbrella creep self-audit (unchanged) | F's self-audit acknowledges the umbrella sentences compress ~20 REDRESS rejections and recommends keeping the detailed ledger alongside. CH5-positive discipline. V3 ACCEPT (F.8) carries forward. | ACCEPT | `skv9-p1-v3-F-redress-reconciliation.md:860-869`. | n/a |
| F.9 | §5.3 V3 CHALLENGE bar item 13 | "Hot leaves named to grammar-neutral primitives; CH2 rejects JSON-role re-naming. xctrace c/B rows are not used as producers — only as characterisers — per SPEC §1 amended clause." The PMU manifest non-producer binding is now a gate item, not just a prose disposition. | ACCEPT | `skv9-p1-v3-F-redress-reconciliation.md:797-801`. | n/a |
| F.10 | §4.1 Edit E retirement (F5 fold) | F's prior proposed edit to `prompts/skinny/PASS-1-PROFILE.md` is dropped per F5 ("orchestrator-scope violation per `ORCHESTRATOR.md` §7"). Subtractive — removes a scope-violating edit, does not create a new producer. | ACCEPT | `skv9-p1-v3-F-redress-reconciliation.md:450-454`. | n/a |

## §3 — Aggregate verdict

Disposition tally across §2: **49 entries; 49 ACCEPT, 0 REVISE, 0 REJECT.**

Per-report breakdown:

| Report | ACCEPT | REVISE | REJECT |
|---|---:|---:|---:|
| P1-V3-A V4 | 8 | 0 | 0 |
| P1-V3-B V4 | 7 | 0 | 0 |
| P1-V3-C V4 | 5 | 0 | 0 |
| P1-V3-D V4 | 9 | 0 | 0 |
| P1-V3-E V4 | 9 | 0 | 0 |
| P1-V3-F V4 | 10 | 0 | 0 |

**ACCEPT rate: 49/49 = 100.0%.** V4 clears the CH5 lens at the
convergence threshold (≥95% ACCEPT per `ORCHESTRATOR.md` §3Z) with
strict margin.

Both V3 REVISE items folded cleanly:

- **REVISE 1 (D §6.1 cardinality)** — folded via §1.1 above. The new
  REDRESS material differential note at lines 477-492 names REPLACES
  in caps, cites the prior scalar + SIMD-fallback paths at file-line
  precision, names the run-alongside case as a Lock-1 fault, and
  cites `LOCKS.md` Lock 1 verbatim. No reader can construe the §6.1
  proposal as installing a sidecar; the substrate cardinality is
  bound to one.
- **REVISE 2 (D §6.1 escape-complete pass count)** — folded by
  deletion via §1.2 above. The "deferred escape-complete" framing is
  removed; the surviving §5.3 mention names the check as a per-byte
  branch inside the per-delimiter scan, not as a separate pass.
  Admissible reading (i) of the F6 surgery is honoured.

Additionally, the V3 ACCEPT-with-rider on A.6 (PMU manifest gate
consumer) is folded via §1.3: A's new §6.5 paragraph binds the manifest
to diagnostic-only status with verbatim Lock-1 + §3W citations, taking
F6 surgery option (ii). The fold matches F's umbrella iv (PMU / cycles
non-producer class umbrella) by spec-cite isomorphism.

The cohort honours Lock 1 strictly across V4:

- The PMU probe binary remains read-only diagnostic with no substrate
  side-effect (A.4, A.5; unchanged from V3).
- Track 1 / Track 2 kept distinct as parser-vs-oracle, never collapsed
  into a SOTA gate (A.5, C.1, C.4; F2 refold cross-cites symbol-level
  attribution to P1-V3-B, not to a coalesced source).
- `scan_structurals` at 0.00% self-time remains a non-producer
  deletion candidate; F1 fold's "wave authorship deferred to S-P3"
  prevents any additive consumer-wiring in this tranche (B.2, D.7).
- The SK-V8 W3 union substrate is not re-opened — F's HANDOFF §5
  mapping carries REDRESS 92 forward verbatim (F.5).
- E's deletion ledger remains uniformly subtractive with explicit
  primitive-class status columns (E.1, E.4, E.5); the renamed-scanner
  risk surface (NEON `match_tiny_plain_string`) is *removed*, not
  instantiated (E.2).
- F's PMU-as-non-producer umbrella + new SPEC clarifier + new gate-bar
  item 13 form a triple-binding: prose disposition, spec clause, gate
  item (F.6, F.7, F.9).

The "renamed-scanner Lock 1 violation" failure mode is positively
guarded by E.2's critical-distinction subordinate clause and by B.3's
dual-callsite finding. The "Track 1 ≡ Track 2 dishonesty" failure
mode is positively guarded by the unchanged per-row tagging and by
C's F2 refold preserving the V2-reference column separately from V3
PMU truth. The "sidecar producer" failure mode is positively guarded
by A §6.5's diagnostic-only binding, D §6.1's REPLACES binding, and
F's umbrella iv triple-binding.

## §4 — Remaining Lock-1 risks

### §4.1 — Long-tail risk: §6.5 PMU manifest deferred wiring

A §6.5's option (ii) binding (diagnostic-only, never-a-producer) is
the CH5-conservative choice but defers the question of whether the
PMU manifest ever becomes admissible as a route-fact substrate. F's
umbrella iv guards this by requiring a same-wave Mbps-isomorphic
comparator that "resists the strict-vs-permissive flaw-probe gate
before consuming PMU evidence too" — i.e. any future cycles/B gate
must demonstrate Mbps-isomorphism first. This is not a current
leak; it is a deferred decision bound by spec.

CH5 disposition: ACCEPT with watch. Any future tranche that proposes
ingesting `pmu_rows.tsv` into `gate-json` must re-trigger CH5 audit on
the new producer surface; the V4 fold pre-empts the leak by binding
the current manifest to non-producer status until a spec amendment
admits otherwise.

### §4.2 — Long-tail risk: §6.3 "rows with no LOSS finding" guard

D §6.3 names citm_catalog / canada / mesh / marine_ik / numbers as
unconditional WINs that depend on the lazy-tape structural-element-emit
advantage and the numeric-token FSM. The §6.3 text says "any successor
wave must guard them" — i.e. the same-row falsification gate must
include these WIN rows as non-regression anchors. This guard is named
but its enforcement mechanism (the gate that fires on regression) is
not yet wired in this tranche; F1's "wave authorship deferred to S-P3"
defers the gate construction.

CH5 disposition: ACCEPT — the guard naming is sufficient at S-P1 scope;
gate enforcement is S-P3 scope per F1. No current substrate-cardinality
leak.

### §4.3 — Long-tail risk: F1 wave-authorship deferral

F1's strip of "three V9/V10 waves, ranked" from D §6.6 and from C's
recommendations removes the V3 pre-emption of S-P3 wave authoring. The
deferral is bounded ("wave-class selection and per-wave cost set … are
S-P3 scope per `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`"), so
the risk is not a current leak. However, the S-P3 wave proposals that
eventually land will need fresh CH5 audit against the same Lock-1
discipline: each new producer surface must REPLACE its predecessor on
the production hot path, must not run alongside, must not retain a
side-table, must not install a parser-local cursor over the byte
stream.

CH5 disposition: ACCEPT with watch. The V4 cohort does not author
waves; S-P3 must re-audit when wave proposals land. The umbrella
list at F.4 + F.6 will pre-block recurrent-class re-proposals; this
is the correct deferral discipline.

### §4.4 — Long-tail risk: §6.1 escape-complete deletion vs explicit qualifier

The fold of REVISE 2 (escape-complete pass-count) is subtractive — the
"deferred" framing is deleted rather than replaced with an explicit
"(i) inline / (ii) `#[cfg(test)]`" qualifier. A reader of D §5.3 who
sees "escape-complete scan (per-byte branch over `b'\\'` and `<0x20`)"
listed alongside "view-boundary UTF-8 validation" and "structural-emit
handshake" must infer from the §5.3 enumeration context that all three
items are inline within the per-delimiter scan. The inference is
admissible (the §5.3 bulleted list is titled "The hot per-string-span-
delimiter work that could plausibly be reduced", positioned inside the
per-delimiter cost class), but it is inference, not assertion.

CH5 disposition: ACCEPT — the inference is unambiguous given the §5.3
list's framing context, and §6.1 no longer proposes any pass-count at
all under F1. A future S-P3 wave that authors a SIMD pass on this
finding must spell the qualifier explicitly per the F6 surgery (option
i or option ii); the V4 fold does not pre-empt that requirement, it
removes the V3 pre-emption.

### §4.5 — No active leaks beyond §4.1–§4.4

The V4 cohort does not introduce:

- **A new substrate variant** beyond the `LayoutFacts.backend_shape`
  five-variant set at `ARCHITECTURE.md` §7.3. D §6.5 forbids one for
  the typed plane; no other report proposes one.
- **A parser-owned cursor / fact slot**. REDRESS 51, 53 stay pre-blocked
  per F §3.1; D §6.1's REPLACES binding does not install a cursor.
- **A renamed scanner**. B.3 exposes the one-primitive-name-two-callsites
  case as evidence; E.2 deletes the NEON `match_tiny_plain_string`
  kernel (removing the rename-risk surface) and explicitly preserves the
  scalar `match_tiny_plain_string_with_cap` in generated.rs as a
  separate admitted surface.
- **A Track 1 ≡ Track 2 collapse**. A.5 / C.1 / C.4 keep the two
  surfaces bench-distinct; the dual-track xctrace_probe is a switch
  (`<track:track1|track2>` flag), not a fusion. C's F2 refold cites
  symbol-level attribution to P1-V3-B (Track 1 oracle role) rather than
  coalescing Track 1 + Track 2 evidence.
- **A retained PMU stream as comparator producer**. A.1 / F.6 / F.7 /
  F.9 form a triple-binding (report §6.5 paragraph + SPEC clarifier +
  gate-bar item 13) that pins the PMU evidence to characteriser status.
- **A new sidecar producer at the V4 edit surface**. The V4 fold
  edits are: (a) a binding paragraph at D §6.1, (b) a deletion of
  the "deferred escape-complete" framing at D §6.1, (c) a binding
  paragraph at A §6.5, (d) class-umbrella additions to F §3, (e)
  primitive-class status columns at E §2, (f) wave-authorship
  retraction at D §6.6 / C recommendations. None introduces a new
  producer; each is either a binding sentence, a subtraction, or a
  vocabulary annotation.

The substrate union holds. V4 advances at CH5 100% ACCEPT.

## §5 — Sources cited

- `restart/locks/LOCKS.md:34` (Lock 1), `:60` (Lock 14).
- `restart/prompts/ORCHESTRATOR.md` §3W (CH5 contract), §3Z (≥95%
  convergence threshold), §8 (non-negotiables).
- `restart/skinny/tranches/sk-v9/research/p1/hardening/V3/CH5.md`
  (V3 CH5 disposition; 95.6% ACCEPT, REVISE 1 D.4 + REVISE 2 D.5).
- `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V3-CONSOLIDATED.md`
  (V4 fold spec, F1–F6).
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md`
  §1.1, §1.3, §2.1.
- `restart/skinny/tranches/sk-v9/HANDOFF.md` §5 (pre-blocked routes).
- `skinny/REDRESS.md` 28, 33, 34, 50, 51, 53, 60–72, 80, 82, 83, 84, 88,
  89, 91, 92, 93.
- `skinny/RESULTS.md:139` (Track 1 vs Track 2 definition).
- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-{A..F}.md`
  (V4-folded subjects; line ranges cited per-row in §2).
