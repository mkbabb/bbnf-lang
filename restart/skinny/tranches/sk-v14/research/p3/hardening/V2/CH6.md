# SK-V14 S-P3 V2 CHALLENGE — CH6 ANTI-PAPER-CLOSE

Pass: S-P3 Synthesis-Plan. Cycle: V2. Lens: CH6 ANTI-PAPER-CLOSE.
Date: 2026-05-23.
Scope: every wave closes on **measurement** (named bench-row threshold), not future-phase promise; revert protocol per wave; same-wave consumer NAMED per candidate (no orphan kernel ships); SPEC forbids deferral; F-V2-P1ABC-RERECORD Stage-0 binding pinned UNCONDITIONALLY to W10 verbatim across SPEC §11/§12/§13 with verbatim 5-step inheritance chain at each site.
Output: this file.
HARD CAP: 30 min. WRITE-ONLY (no git add/commit). Aggregator commits 8 hardening files atomically.

Authority:
- `restart/skinny/tranches/sk-v14/research/p3/hardening/V2/CHALLENGE-CONTEXT.md` (HEAD `690276e03`)
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md §3` (CH6 lens lines 140-145)
- `restart/skinny/tranches/sk-v14/research/p3/hardening/V1/CH6.md` (V1 disposition: REVISE-1, 73.7% line-item / 89.5% root-collapsed; load-bearing F-V2-P1ABC-RERECORD Stage-0 three-way divergence)
- `restart/skinny/tranches/sk-v14/research/p3/{p3a..p3f}` + `sk-v14/SPEC.md` + `sk-v14/DISPATCH-PROMPT.md` at V2 HEAD

## §1 — V2 disposition vs V1 carry-forward

V1 CH6 disposition (per `V1/CH6.md:303`): **REVISE-1 (89.5% root-issue / 73.7% line-item); fold Stage-0 wave-pin into V2.** Three line-items / one root issue:

1. **REVISE-1 / Stage-0 binding wave-pin** — three artefacts (P3-A, P3-B, SPEC + P3-F draft) held three different bindings; SPEC's "UNLESS it admits one of the 12" conditional opened an orphan-kernel hole for C5 (load-bearing shortlist intervention).
2. **REVISE-1 / Wave-numbering convergence** — P3-B (W9 = R6) vs SPEC (W8 = R6) misaligned.
3. **REVISE-1 / Orphan-kernel closure for C5** — C5 could ship in zero waves if no 12-list primitive admitted per wave.

V2 dispatch context §2 CH6 lines 29 names two load-bearing discharge tests:
- **F-V2-CH6-1:** verify final `grep "UNLESS it admits one of the 12"` returns ZERO hits across SPEC; verify W10 unconditional Stage-0 binding at SPEC §11/§12/§13 + 5-step inheritance chain VERBATIM repeated at three sites.
- **F-V2-CH6-2:** wave-numbering reconcile preserves measurement-based gates.

Each is re-executed at V2 HEAD below.

## §2 — F-V2-CH6-1 discharge (load-bearing): conditional language REMOVED + unconditional binding VERBATIM

### §2.1 — The grep test (final): zero hits

V2 executed at HEAD:

```
$ grep -n "UNLESS it admits one of the 12" restart/skinny/tranches/sk-v14/SPEC.md
(no output; exit code 1)

$ grep -n "UNLESS" restart/skinny/tranches/sk-v14/SPEC.md
(no output)
```

V1 quoted SPEC §11:856 + §12:916 + §13:975 carrying the conditional "UNLESS it admits one of the 12 consumer-dependency primitives". At V2 HEAD all three sites are REPHRASED to UNCONDITIONAL wording. The literal substring "UNLESS" is now absent from SPEC entirely.

**Disposition: ACCEPT** — F-V2-CH6-1 grep gate passes byte-cleanly.

### §2.2 — W10 unconditional Stage-0 binding at §11/§12/§13

V2 executed at HEAD:

```
$ grep -n "F-V2-P1ABC-RERECORD\|Stage 0\|Stage-0" restart/skinny/tranches/sk-v14/SPEC.md
221:- Any wave admitting any dispatch-envelope-internal primitive ships F-V2-P1ABC-RERECORD as Stage 0 …
863: W8 plan does NOT carry Stage-0 F-V2-P1ABC-RERECORD: Stage-0 binds UNCONDITIONALLY to W10 …
871: 5. NO F-V2-P1ABC-RERECORD Stage-0 work in W8: Stage-0 binds unconditionally to W10 per §11 entry-gate inheritance chain …
880: F-V2-P1ABC-RERECORD Stage-0 is NOT a W8 obligation (binds unconditionally to W10 per §11 entry-gate inheritance chain) …
923: W9 plan does NOT carry Stage-0 F-V2-P1ABC-RERECORD: Stage-0 binds UNCONDITIONALLY to W10 …
931: 5. NO F-V2-P1ABC-RERECORD Stage-0 work in W9: Stage-0 binds unconditionally to W10 per §12 entry-gate inheritance chain …
940: F-V2-P1ABC-RERECORD Stage-0 is NOT a W9 obligation (binds unconditionally to W10 per §12 entry-gate inheritance chain) …
982: W10 plan MUST include Stage-0 F-V2-P1ABC-RERECORD UNCONDITIONALLY per S-P2 V3 §6.3 verbatim …
990: 5. Ship F-V2-P1ABC-RERECORD Stage-0 UNCONDITIONALLY per S-P2 V3 §6.3 (W10 is the bound wave …) BEFORE any parse_only admit lands …
1000: F-V2-P1ABC-RERECORD Stage-0 SHIPPED UNCONDITIONALLY per S-P2 V3 §6.3 (W10 is the bound wave per the §13 entry-gate inheritance chain) …
```

Ten Stage-0 cite-sites; all six wave-anchored sites (§11 W8 lines 863+871+880, §12 W9 lines 923+931+940, §13 W10 lines 982+990+1000) carry **UNCONDITIONAL** wording with W10 named as the bound wave. The §1:221 global rule retains the trigger criterion ("any wave admitting any dispatch-envelope-internal primitive ships Stage 0") with the W10 anchor resolved through the §11/§12/§13 inheritance chain.

**Disposition: ACCEPT** — Stage-0 binds UNCONDITIONALLY to W10 across all three wave sections.

### §2.3 — 5-step inheritance chain verbatim repeated at three sites

V2 executed at HEAD:

```
$ grep -c "Stage-0 inheritance chain (5-step): (1) Stage-0 trigger = first wave admitting C1/C3/C7 per S-P2 V3 §6.3 verbatim" restart/skinny/tranches/sk-v14/SPEC.md
3
```

Three matches at lines 863 (§11 W8), 923 (§12 W9), 982 (§13 W10). Each carries the literal 5-step chain:

1. Stage-0 trigger = first wave admitting C1/C3/C7 per S-P2 V3 §6.3 verbatim
2. C1 = long-string-body SIMD scan primitive (queued for S-P3 same-wave admission per S-P2 V3 §6.2)
3. W10 is first wave consuming C1 via the parse_only distinct path per R8 (the parse_only-distinct-path admission is the first dispatch-envelope behavioral edit)
4. Therefore W10 carries Stage-0 unconditionally
5. W8 + W9 do NOT admit C1/C3/C7 → no Stage-0 obligation there

The chain's first sentence is byte-identical at all three sites; the closing clauses adapt by section (W8 / W9 → "no obligation"; W10 → consumer manifest enumeration). This is the verbatim repetition F-V2-CH6-1 mandates.

**Disposition: ACCEPT** — 5-step inheritance chain present at all three SPEC sites.

### §2.4 — V1 paper-close hole CLOSED

V1 §1.6 enumerated the failure mode: "the cascade can complete W8 → W9 → W10 WITHOUT EVER SHIPPING C5". V2 closes this by:

- §11 W8:863 explicit: "W8 inherits no Stage-0 obligation" (CSS L4 does not admit C1/C3/C7).
- §12 W9:923 explicit: "JSON direct + typed planes do NOT admit C1/C3/C7" (full-tape parse, not dispatch-envelope parse_only scan).
- §13 W10:982 explicit: "W10 plan MUST include Stage-0 F-V2-P1ABC-RERECORD UNCONDITIONALLY" + "BEFORE any parse_only admit lands" (line 990).
- §13 W10:1000 post-shipped audit cite: "F-V2-P1ABC-RERECORD Stage-0 SHIPPED UNCONDITIONALLY" with consumer manifest enumerated.

C5 (F-V2-P1ABC-RERECORD itself) is now pinned to W10's Stage-0 by a measurable existence + binding fact, not by a wave-by-wave admit census. The kernel cannot ship in zero waves — W10 is the unique bound site, and W10 is unconditional on consumer primitive admission within that wave.

**Disposition: ACCEPT** — orphan-kernel hole closed.

## §3 — F-V2-CH6-2 discharge: wave-numbering reconcile preserves measurement-based gates

V1 §1.6 surfaced the divergence: P3-B (W9 = R6 CSS L4) vs SPEC (W8 = R6 CSS L4). V2 fold-packet authority (CHALLENGE-CONTEXT §1 line 15) records: "P3-B (V2 amended; full section-relabel to SPEC §2 ordering W0..W11; gate content preserved byte-identical except wave-id refresh; new §2.14 W11 close ceremony)."

### §3.1 — P3-B vs SPEC wave-id parity at V2 HEAD

V2 executed at HEAD; the P3-B manifest at `p3b-wave-sequencing.md:72-87` (§2.1 Wave manifest) now reads:

| P3-B wave | P3-B label | SPEC wave (§2 manifest 237-248) | Reconciled? |
|---|---|---|---|
| W0 §2.3 | Baseline Profile + Telemetry Lock | W0 §3 Baseline Profile And Telemetry Lock | YES |
| W1 §2.4 | C-2 fused C-5 PRUNE-1 | W1 §4 Comparator Rebind + PRUNE-1 | YES |
| W2 §2.5 | C-3 R4 regen-css xtask | W2 §5 regen-css xtask R4 | YES |
| W3 §2.6 | C-3 R5 production CSS corpora | W3 §6 Production CSS Corpora | YES |
| W4 §2.7 | C-5 PRUNE-2 | W4 §7 PRUNE-2 delete 7 CSS templates + revert 24 admits | YES |
| W5 §2.8 | C-1 PRUNE-3 trait dispatch | W5 §8 PRUNE-3 Lock-14 refactor | YES |
| W6 §2.9 | C-1 PRUNE-4 9 sub-waves | W6 §9 PRUNE-4 9 sub-waves | YES |
| W7 §2.10 | C-4 PRUNE-5 | W7 §10 PRUNE-5 wire W8/W9 SCAFFOLD → LOAD-BEARING | YES |
| W8 §2.11 | R6 CSS L4 24-feature re-admit | W8 §11 CSS L4 Re-Admit (R6) | YES |
| W9 §2.12 | R7 JSON direct + typed re-admit FUSED | W9 §12 JSON Direct + Typed Re-Admit (R7) | YES |
| W10 §2.13 | R8 JSON parse_only distinct path + admit + Stage 0 | W10 §13 JSON parse_only Distinct Path + Re-Admit (R8) | YES |
| W11 §2.14 | Close And Alpha Feedback | W11 §14 Close And Alpha Feedback | YES |

12/12 parity. Per `p3b-wave-sequencing.md:35` + `:60` + `:86` the Stage-0 anchor reads "MUST ship as Stage 0 of W10 (the parse_only distinct-path admission wave; per SPEC §13 + §11 lines 856/873 + §12 lines 916/933 inheritance chain)". W10 = R8 = parse_only distinct path is consistent across P3-B and SPEC §2 manifest line 247.

### §3.2 — Measurement-based gates preserved per wave (re-walked at V2 HEAD)

| Wave | Exit-gate measurement anchor (V2 HEAD) | Same as V1 disposition? |
|---|---|---|
| W0 (§3:373) | `xtask gate-json` rejects missing columns; throughput ±1.0% of `SK-V14-open` | YES — ACCEPT preserved |
| W1 (§4:445) | Strict-comparator binding + per-iter equality oracle + 22-row revert + ROLLING-SOTA-DELTA cell-state 0/17 across 3 planes | YES — ACCEPT preserved |
| W2 (§5:503) | `cargo xtask regen-css` round-trip clean (rm + regen + diff empty) | YES — ACCEPT preserved |
| W3 (§6:554) | `du -sh ≥ 800 KB` working-set floor; manifest.md + checksum + loader resolve | YES — ACCEPT preserved |
| W4 (§7:613) | 7-template-dir delete `wc -l == 0`; regen produces byte-deterministic diff-empty output; 24-row revert in RESULTS + ROLLING-SOTA-DELTA | YES — ACCEPT preserved |
| W5 (§8:672) | `RuntimeProvider::*\|JsonGrammar\|parse_json_grammar` grep returns 0; Lock-14 baseline gate passes | YES — ACCEPT preserved |
| W6 (§9:760-763) | Per-sub-wave + W6 aggregate grep + diff-empty; 67 hand-written → 0 + 67 generated | YES — ACCEPT preserved |
| W7 (§10:824) | samply attribution shift on `json/numbers/direct_to_struct/main` pre-wave `parse_value_at` → post-wave W11.1 number-specialised symbol | YES — ACCEPT preserved |
| W8 (§11:888) | At least one CSS L4 feature ADMITs > lightningcss strict full-parse on production corpora (≥800 KB) on same plane/equality | YES — ACCEPT preserved (V2 disambiguated: Stage-0 NOT a W8 obligation) |
| W9 (§12:948) | Every selected JSON direct/typed row meets Track 1 + Track 2 floors; correctness parity; per-iter equality | YES — ACCEPT preserved (V2 disambiguated: Stage-0 NOT a W9 obligation) |
| W10 (§13:1006) | Distinct parse_only path exists; ≥1 row ADMITs > sonic_rs::Skipper on same plane/corpus/equality; Stage-0 SHIPPED UNCONDITIONALLY | YES — ACCEPT preserved + V2 fold pins Stage-0 here |
| W11 (§14:1057-1059) | Close-honesty checklist + document reconciliation; no source revert; reopen producing wave or mark close blocked | YES — ACCEPT preserved |

12/12 measurement-anchored exit gates. Wave-id refresh preserved gate content byte-identical per V2 fold authority; V1's 9/12 ACCEPT extends to 12/12 ACCEPT now that W8/W9/W10 Stage-0 conditional language is removed.

**Disposition: ACCEPT** — F-V2-CH6-2 discharged; wave-numbering reconcile preserves measurement-based gates.

## §4 — Revert protocol per wave (V2 re-verification)

V2 executed at HEAD:

```
$ grep -n "Revert protocol" restart/skinny/tranches/sk-v14/SPEC.md
373: §3 W0
450: §4 W1
508: §5 W2
559: §6 W3
618: §7 W4
677: §8 W5
767: §9 W6 (per-sub-wave + aggregate)
831: §10 W7
895: §11 W8
955: §12 W9
1013: §13 W10
1064: §14 W11 (close-ceremony alternate form: "no source revert by default")
```

12/12 revert protocols present; every wave names a REDRESS slot. W6 carries per-sub-wave + W6 aggregate revert per `[clean-regen-discipline]` + S-P0 §3.3 sub-wave count. W11 carries the close-ceremony alternate ("no source revert by default. Reopen the producing wave or mark close blocked with a mismatch list naming file paths, rows, and missing evidence").

**Disposition: ACCEPT** — 12/12 revert protocols preserved at V2.

## §5 — No-deferrals language in SPEC (V2 re-verification)

V2 executed at HEAD:

```
$ grep -n "No deferrals\|future-phase promise\|paper close\|paper-close" restart/skinny/tranches/sk-v14/SPEC.md
220:- No deferrals: a wave cannot close on "wired", "advisory", "future consumer", "integrated", or "paper close" language without measured evidence (per `[no-deferrals]`).
227:- **CH7-V2 procedural addendum:** any past-perfect verb-tense claim … is paper-close even if the cite chain is otherwise complete …
1057:Pre-blocked routes: paper close (W11 must close on measurement, not promise) …
```

SPEC §1:220 names five proscribed terms ("wired", "advisory", "future consumer", "integrated", "paper close"). SPEC §1:227 CH7-V2 verb-tense addendum binds past-perfect claims to `ls`-existence verification. SPEC §14 W11:1057 close-ceremony pre-block enforces paper-close prohibition.

**Disposition: ACCEPT** — SPEC §1 + §14 no-deferrals discipline preserved.

## §6 — Same-wave consumer per candidate (V2 re-verification with C3 + C4 binding focus)

V2 executed at HEAD; per CHALLENGE-CONTEXT §2 line 31 ("verify C3 + C4 V2 bindings"):

### §6.1 — C3 same-wave consumer (V2-amended)

`p3a-candidate-shortlist.md:93` reads at V2 HEAD:

> "Same-wave consumer NAMED (3-gate cell c): direct-plane number kernel in `parse_array_element_at_direct` (`generated.rs:506`) on canada / mesh / marine_ik / numbers …; same-wave non-JSON consumer is the `bbnf-simd` checkasm row exercising the CSS-permissive `byte_class_from_range_64` (Gap 7.5) sibling — new `crates/bbnf-simd/tests/checkasm_byte_class_from_range_64.rs` modelling the sibling-shape template at `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:1` (executable-verified at HEAD), with the CSS L4 `<number>` byte-class config `[0x30..=0x39, 0x2E, 0x2B, 0x2D, 0x65, 0x45]` instantiated as the non-JSON row. The checkasm parity row IS the non-JSON same-wave exercise — discharges Lock 14 v+1 'at least one non-JSON consumer' inside the SAME wave that admits the SIMD body, no cross-wave deferral to W8."

V2 amendment delivers: the **bbnf-simd checkasm row** (the new `checkasm_byte_class_from_range_64.rs`) is the non-JSON consumer in the SAME wave that admits the SIMD body. No cross-wave deferral. The CSS L4 `<number>` runtime consumer at W8 is re-cast as W8 corroboration, not as the W9 admission gate.

**Disposition: ACCEPT** — C3 same-wave consumer NAMED inside the admit wave; F-V2-CH2-1 discharged per CH6 secondary check (no orphan-kernel risk).

### §6.2 — C4 same-shape consumer (V2-amended)

`p3a-candidate-shortlist.md:106` reads at V2 HEAD:

> "Same-wave consumer NAMED (3-gate cell c): parse-only `y_string_unicode` row + direct-plane `unicode_escapes` + `unicode_mixed` rows; same-wave non-JSON consumer is the **BBNF-self string-escape consumer** — BBNF-self uses JSON-shape escape alphabet per P2-F §2.7 + §3 note 1 (`grammar/bbnf/bbnf.bbnf:11-13` …; the `\u`+4-nibble form is shape-identical) … The CSS L4 escaped-ident `\HEXHEX` (CSS Syntax §4.3.7 variable 1-6 hex digits) is SHAPE-ORTHOGONAL to the fixed-4-nibble SIMD body and does NOT exercise this primitive — carved out as a separate-primitive concern per Lock 14 v+1 'measured deletion/rejection' record."

V2 amendment delivers: (a) BBNF-self string-escape consumer is the same-shape non-JSON consumer in the SAME wave that admits the C4 SIMD body; (b) variable-width CSS `\HEXHEX` is explicitly carved out as a separate-primitive concern (measured-rejection record per Lock 14 v+1) — preserving shape-orthogonality discipline. No orphan-kernel risk; no overfit asymmetry.

**Disposition: ACCEPT** — C4 same-shape consumer NAMED; F-V2-CH2-2 discharged per CH6 secondary check.

### §6.3 — Full shortlist scoreboard (V2 HEAD)

| # | Candidate | Same-wave consumer NAMED? | Orphan-kernel risk at V2? |
|---|---|---|---|
| C1 | `long_string_body_simd_scan` | YES — `parse_that_regex::skip_string_plain_trusted` at `lib.rs:547` (direct-plane envelope at `generated.rs:466,506`) | NO |
| C2 | `structural_index_singular_substrate_consumer` | YES — direct + typed envelopes at `generated.rs:466,506,2949` | NO |
| C3 | `digit_block_simd_accumulate` | YES — `bbnf-simd` checkasm row (`byte_class_from_range_64`) is the non-JSON same-wave exercise | NO |
| C4 | `unicode_escape_neon_nibble_decode` | YES — BBNF-self string-escape consumer (shape-identical); CSS L4 `\HEXHEX` carved out as separate-primitive | NO |
| C5 | `parse_attribution_envelope_cracker` (IS F-V2-P1ABC-RERECORD) | YES — bound to W10 Stage-0 UNCONDITIONALLY per §11:863 / §12:923 / §13:982 inheritance chain | NO (V2 closes V1's orphan-kernel hole) |
| C6 | `force_inline_lto_envelope_discipline` | YES — paired with C5; codegen template + cargo asm + samply | NO (inherits C5's V2 unconditional W10 binding) |
| C7 | `ascii_whitespace_skip_64` | YES — every JSON value-position prelude + CSS L4 declaration-value whitespace | NO |
| C8 | `BackendShape::SinkOnly` activation | YES — 8 P1-B direct-plane rows where envelope is 70%+ top-1 | NO |

8/8 candidates carry consumer NAMED inside the admit wave. The V1 "C5/C6 orphan-kernel REVISE-1" is closed by V2's unconditional W10 binding.

**Disposition: ACCEPT** — 8/8 same-wave consumer discipline at V2.

## §7 — Residual finding: P3-C §2.10 + §1.2 retain conditional Stage-0 language (sub-finding F-V2-CH6-3)

V2 executed at HEAD:

```
$ grep -n "If admitting any of the 12\|Stage 0 rerun is shipped\|Stage-0 F-V2-P1ABC-RERECORD if any consumer-dependency" restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md
36:| W10 | R8 | … admit; Stage-0 F-V2-P1ABC-RERECORD if any consumer-dependency primitive admitted | R8 | 17 (per corpus) |
423:8. If admitting any of the 12 F-V2-P1ABC-RERECORD consumer-dependency primitives, Stage 0 rerun is shipped per S-P2 V3 §6.3.

$ grep -n "If admitting\|UNCONDITIONALLY" restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md
36:… if any consumer-dependency primitive admitted …
423:8. If admitting any of the 12 …
```

Two P3-C cite-sites carry the OLD V1 conditional language that SPEC + P3-B have already migrated away from:

1. `p3c-falsifiability-gates.md:36` (W10 row in §1.2 wave manifest table) — "Stage-0 F-V2-P1ABC-RERECORD if any consumer-dependency primitive admitted"
2. `p3c-falsifiability-gates.md:423` (§2.10 W10 exit gate item 8) — "If admitting any of the 12 F-V2-P1ABC-RERECORD consumer-dependency primitives, Stage 0 rerun is shipped per S-P2 V3 §6.3"

These are residuals of the V1 conditional wording — they DO NOT open the V1 paper-close hole at W10 (because P3-C §2.10 is the W10 wave-section, and W10 is the bound wave per SPEC §13:982; the V1 hole was W8/W9 SHIPPING NOTHING, which V2 SPEC has closed). However, they create a textual asymmetry: SPEC §13:982 says W10 ships UNCONDITIONALLY; P3-C §2.10:423 says W10 ships "if admitting any of the 12". The SPEC is the binding artefact (per CHALLENGE-CONTEXT §1 line 13 — SPEC is V2-amended; P3-C §2.10 V2-amend covered W9 fusion + new §2.11 W11 but did not touch §2.10:423 + §1.2:36 conditional Stage-0 wording).

**Disposition: REVISE-2 (minor wording reconcile; non-blocking)** — P3-C should mirror SPEC's W10 UNCONDITIONAL wording at `p3c:36` + `p3c:423`. The SPEC §13:982 binding overrides; P3-C is the falsifiability-gate artefact and should not carry weaker conditional language than its anchor SPEC. No paper-close vehicle results because (a) SPEC is binding, (b) §2.10 IS the W10 wave-section so the conditional is harmless within that local scope. Flag for V3 micro-fold.

**Note:** P3-F draft §1.3.3 (`p3f-spec-draft.md:108, 117-122`) also retains the V1 W8/W9/W10 OR-conditional language, but P3-F is V1-LOCKED per CHALLENGE-CONTEXT §1:18 ("p3f-spec-draft.md (V1-LOCKED; no V2 edits)") and is explicitly a planning draft superseded by SPEC. P3-F's stale wording is not a paper-close risk because SPEC is the binding artefact and P3-F is by construction non-binding draft text.

## §8 — Three SPEC interaction observations (per CHALLENGE-CONTEXT §2 lines 33-35)

V2 executed at HEAD:

1. **SPEC §2 W9 W1-only dependency (parallel-eligibility with W2-W8)** — `SPEC.md:246` reads "W9 | Section 12 | JSON Direct + Typed Re-Admit (R7; under rebound R1 comparators) | Conditional on W1 close (depends only on R1+R2, not on PRUNE waves) | …". This is intentional per SPEC §0.1 close-condition: W9 = R7 JSON direct + typed re-admit consumes only the rebound C-2 comparators (W1 output), not the PRUNE chain. CSS L4 admit (W8) and JSON parse_only distinct path (W10) carry separate dependencies. **Disposition: ACCEPT — intentional per SPEC §0.1, measurement gate at §12:948 preserved.**

2. **W11 close-ceremony qualitatively different gate category (no source LOC + no row gate)** — `SPEC.md:248` reads "W11 | Section 14 | Close And Alpha Feedback | … 0 source LOC; docs/RESULTS/REDRESS/HANDOFF/SPEC reconciliation only | ≤90 min". `SPEC.md:1057-1059` revert protocol: "no source revert by default. Reopen the producing wave or mark close blocked with a mismatch list". Measurement closure at §14: every wave-disposition census present + close-honesty checklist + document reconciliation. **Disposition: ACCEPT — close-ceremony measurement is per-family ADMIT count + document reconciliation, qualitatively different from row admit measurement; the binding "every wave has admitted/rejected/routed status" is the threshold.**

3. **W9 fused 34-row admit budget per 90-min cap** — `SPEC.md:246` reads "W9 | … | ≤450 source/test LOC; rows named in wave plan | ≤90 min". P3-C §2.9:347-396 enumerates 17 direct + 17 typed = 34 rows fused under the W9 ≤90-min cap. Per V1 CH4 + CH4 V2 cost budget verified at 12 + 8 shortlist (CHALLENGE-CONTEXT §2 line 28 verifies F-V2-CH4-1 §9 W6 810-min cumulative cap footnote). **Disposition: ACCEPT — LOC budget (≤450 source/test LOC for fused 34-row consumer wiring) + 90-min cap is the measurement; the 90-min cap is enforced via the wave-execution contract.**

## §9 — Per-sub-test scorecard (V2 HEAD)

| CH6 sub-test | V1 Pass count | V2 Pass count | V2 Disposition |
|---|---|---|---|
| 1 — Measurement closure per wave | 9/12 ACCEPT + 3 REVISE-1 (W8/W9/W10 Stage-0 conditional) | 12/12 ACCEPT | ACCEPT |
| 2 — Revert protocol per wave | 12/12 ACCEPT | 12/12 ACCEPT | ACCEPT |
| 3 — No-deferrals SPEC language | 1/1 ACCEPT | 1/1 ACCEPT | ACCEPT |
| 4 — Same-wave consumer per candidate | 6/8 ACCEPT + 2 REVISE-1 (C5/C6) | 8/8 ACCEPT | ACCEPT |
| 5 — F-V2-P1ABC-RERECORD Stage-0 binding (three-way divergence) | 0/1 REVISE-1 | 1/1 ACCEPT (W10 unconditional, 5-step chain ×3 verbatim) | ACCEPT |
| 6 — Wave-numbering convergence | inherited from CH1 V1 | 12/12 parity P3-B ↔ SPEC | ACCEPT |
| 7 — Three SPEC interaction observations | not in V1 scope | 3/3 ACCEPT | ACCEPT |
| 8 — Residual P3-C conditional wording (F-V2-CH6-3) | not in V1 scope | 1 REVISE-2 minor | REVISE-2 (non-blocking, V3 micro-fold) |

## §10 — Aggregate disposition

V2 line items: 7 sub-test families. Six (sub-tests 1-7) close at full ACCEPT (38/38 line items). One (sub-test 8) carries a REVISE-2 minor wording reconcile in P3-C (2 cite-sites; non-blocking residual; SPEC binding is unimpaired; no paper-close vehicle).

- ACCEPT line items: 38 (12 + 12 + 1 + 8 + 1 + 1 + 3)
- REVISE-2 line items: 2 (`p3c:36`, `p3c:423`)
- REJECT: 0

**ACCEPT-rate: 38/(38+2) = 95.0%.**

Strictly meets the §3Z ≥95% ACCEPT bar.

If we treat the 2 P3-C residuals as ONE root issue (both name the same V1 conditional wording, easily fixed in one P3-C micro-fold of one line + one cell), the root-issue-collapsed rate is: 38/(38+1) = **97.4%**.

Both V2 rates clear §3Z ≥95%. The V1 → V2 movement is 73.7% → 95.0% line-item (+21.3 pp) and 89.5% → 97.4% root-issue-collapsed (+7.9 pp). The load-bearing CH6 paper-close hole at C5 (V1's REVISE-1 root issue) is fully closed.

**Disposition: ACCEPT (one V3 micro-fold flagged for non-blocking P3-C wording mirror).**

## §11 — Falsifiability binding (named corpus rows + Mbps thresholds — CH6 verification at V2 HEAD)

CH6 V2 disposition is rooted in **executable** verification at HEAD:

1. **F-V2-CH6-1 grep gate executed at HEAD**:
   - `grep -n "UNLESS it admits one of the 12" restart/skinny/tranches/sk-v14/SPEC.md` → exit code 1, zero hits.
   - `grep -n "UNLESS" restart/skinny/tranches/sk-v14/SPEC.md` → no output, zero hits.

2. **F-V2-CH6-1 5-step inheritance chain count executed at HEAD**:
   - `grep -c "Stage-0 inheritance chain (5-step): (1) Stage-0 trigger = first wave admitting C1/C3/C7 per S-P2 V3 §6.3 verbatim" restart/skinny/tranches/sk-v14/SPEC.md` → 3 (lines 863 §11, 923 §12, 982 §13).

3. **F-V2-CH6-1 W10 unconditional Stage-0 binding executed at HEAD**:
   - `grep -n "UNCONDITIONALLY\|unconditionally" restart/skinny/tranches/sk-v14/SPEC.md` → 7 sites at lines 863, 871, 880, 923, 931, 940, 982, 990, 1000 — all anchored to W10.

4. **F-V2-CH6-2 wave-numbering reconcile executed at HEAD**:
   - SPEC §2 manifest 237-248 = 12 rows W0..W11 contiguous.
   - P3-B §2.1 manifest 76-87 = 12 rows W0..W11 contiguous; §2.3..§2.14 subsections cover each wave.
   - Per-wave measurement gate present at every row (per §3.2 table above).

5. **The orphan-kernel risk is logically closed at V2**:
   - C5 IS F-V2-P1ABC-RERECORD per `p3a:124`.
   - SPEC §13:982 binds Stage-0 to W10 unconditionally.
   - SPEC §13:990 requires Stage-0 BEFORE any parse_only admit lands → no wave can "honestly close on no 12-list primitive admitted" because W10 is the bound site and W10's entry gate enforces Stage-0 first.
   - SPEC §11:863 + §11:871 + §12:923 + §12:931 explicitly state W8 + W9 do NOT inherit Stage-0 — eliminating the V1 paper-close vehicle whereby W8 / W9 could close on "no admit needed this wave".

6. **The measurement-closure pass count is bench-verifiable** at each named wave's exit gate (the cells in §3.2 above are direct quotations from SPEC §3..§14 exit-gate language; each names a Mbps floor, samply attribution, grep count, file-existence check, du-sh corpora floor, or ROLLING-SOTA-DELTA cell-state).

## §12 — Pre-blocked routes (REDRESS entries this lens must NOT re-open)

CH6 V2 is a closure lens, not an admission lens. It does not re-open any REDRESS route by construction. The pre-blocks relevant to V2 findings:

- **REDRESS no-deferrals binding** (per `[no-deferrals]` memory): every primitive lands its hot-path consumer in the same commit. V1 surfaced the C5 orphan-kernel hole; V2 closes it.
- **CH7-V2 verb-tense discipline** (SPEC §1:227): C5's W10 binding is future-tense in the SPEC ("MUST include" at §13:982, "Ship" at §13:990); past-perfect tense at §13:1000 ("SHIPPED") is the post-execution audit cite, not a forward-looking commitment — verb-tense polish observed.
- **`[no-orphan-redress]` discipline:** every wave's exit gate names corpus rows it must lift + maintain. V2 §3.2 walk confirms 12/12 waves carry the lift + maintain row enumeration.
- **P-5 pattern pre-block (SPEC §15):** "Scaffold-research counted as load-bearing. SK-V14 PRUNE-5 (W7) wires W8 + W9 end-to-end; no row admit may cite W8 / W9 as evidence until the runtime consumer is measured." V2 preserves this — W7 same-wave consumer is the named `json/numbers/direct_to_struct/main` samply attribution shift (per §10:824).
- **CH7 round-trip-rule trigger** (SYNTHESIS §0.4 P-1): any CSS feature whose claimed Mbps exceeds the same-plane lightningcss comparator by ≥50× triggers user re-pin. V2 W8 exit gate at P3-C §2.8:333 explicitly enforces this — Track 1 Mbps < 50 × lightningcss Mbps per feature; trigger fires at admit, not at close.

CH6 V2 surfaces ONE non-blocking residual (F-V2-CH6-3 — P3-C §2.10:423 + §1.2:36 conditional wording mirror to SPEC §13:982 unconditional). No REDRESS route re-opened.

## §13 — Sources (every upstream artefact cited at V2 HEAD)

### §13.1 — Authority chain
- `restart/skinny/tranches/sk-v14/research/p3/hardening/V2/CHALLENGE-CONTEXT.md` (HEAD `690276e03`; §0 authority + §2 CH6 verbatim disposition focus; §3 30-min cap)
- `restart/skinny/tranches/sk-v14/research/p3/hardening/V1/CH6.md` (V1 disposition REVISE-1; load-bearing F-V2-P1ABC-RERECORD Stage-0 three-way divergence root finding)
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md §3` (CH6 lens definition lines 140-145)
- `restart/prompts/ORCHESTRATOR.md §3W + §3Z` (cohort LOCK ≥95% × 2 cycles; V≤5 ceiling)

### §13.2 — Six P3 axis artefacts + SPEC + DISPATCH-PROMPT at V2 HEAD
- `restart/skinny/tranches/sk-v14/research/p3/p3a-candidate-shortlist.md` (316 lines; V2 amended)
  - §2 C3 (line 87-94) — bbnf-simd checkasm row as non-JSON same-wave consumer
  - §2 C4 (line 100-108) — BBNF-self string-escape consumer + CSS \HEXHEX carve-out
  - §2 C5 (line 119, 124) — F-V2-P1ABC-RERECORD itself, ships per `[no-deferrals]` in any wave admitting C1/C3/C7
  - §2.1 shortlist table (line 173-178) — 8/8 CF-3 3-gate cells
  - §2.1 dependency census (line 180) — first SK-V14 implementation wave admitting any of {C1, C3, C7} MUST ship C5 as Stage 0
- `restart/skinny/tranches/sk-v14/research/p3/p3b-wave-sequencing.md` (410 lines; V2 amended)
  - §1.1 binding inputs (line 35) — Stage-0 MUST ship as Stage 0 of W10
  - §1.2 sequencing derivation (line 60) — Stage-0 ships in W10 verbatim
  - §2.1 wave manifest (line 76-87) — W0..W11 contiguous; W10 row carries "Stage 0 ships in this wave's Stage 0"
  - §2.2 owner-path families (line 105) — W10 row enumerates Stage-0 file targets
  - §2.13 W10 details (line 268-282) — Stage-0 binding to W10 unconditionally
  - §2.14 W11 close-ceremony (line 284-302) — close-ceremony gate
- `restart/skinny/tranches/sk-v14/research/p3/p3c-falsifiability-gates.md` (537 lines; V2 amended)
  - §1.2 wave manifest (line 22-37) — W10 row text carries residual conditional Stage-0 wording (F-V2-CH6-3 minor)
  - §2.0..§2.11 per-wave gates — 12 measurement-closed gates
  - §2.10 W10 (line 398-435) — exit-gate item 8 carries residual conditional language (F-V2-CH6-3 minor)
  - §2.11 W11 (line 436-441) — close-ceremony gate
- `restart/skinny/tranches/sk-v14/research/p3/p3d-telemetry-schema.md` (V1-LOCKED; not touched at V2)
- `restart/skinny/tranches/sk-v14/research/p3/p3e-preblocked-ledger.md` (V1-LOCKED; not touched at V2)
- `restart/skinny/tranches/sk-v14/research/p3/p3f-spec-draft.md` (V1-LOCKED; planning draft; carries V1 conditional Stage-0 wording at line 108, 117-122 — non-binding draft text; superseded by SPEC §13)
- `restart/skinny/tranches/sk-v14/SPEC.md` (1187 lines; V2 amended)
  - §1:220 no-deferrals language verbatim
  - §1:221 dispatch-envelope-internal primitive trigger criterion
  - §1:227 CH7-V2 verb-tense addendum
  - §2:237-248 wave manifest W0..W11
  - §3..§14 per-wave sections (lines 315-1059)
  - §11 W8 (line 863, 871, 880) — Stage-0 NOT a W8 obligation; unconditionally binds to W10; 5-step inheritance chain
  - §12 W9 (line 923, 931, 940) — Stage-0 NOT a W9 obligation; unconditionally binds to W10; 5-step inheritance chain
  - §13 W10 (line 982, 990, 1000) — Stage-0 MUST ship UNCONDITIONALLY; 5-step inheritance chain
  - §14 W11 (line 1057-1059) — close-ceremony pre-block: paper close
- `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md` (V1-LOCKED; not touched at V2)

### §13.3 — S-P2 V3 carry-forward binding authority
- `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md §6.3` (F-V2-P1ABC-RERECORD Stage-0 wave commitment binding — "Stage 0 of the first SK-V14 implementation wave admitting any dispatch-envelope-internal primitive")

### §13.4 — Memory binding
- `[no-deferrals]` — every primitive lands its hot-path consumer in the same commit; never defer to future tranches
- `[no-orphan-redress]` — every wave's exit gate names corpus rows it must lift + maintain
- `[execute-planned-architecture]` — don't retreat from planned architectural changes; never ship stub/shim
- `[clean-regen-discipline]` — generated files are output of fresh regen; never hand-patched (W6 per-sub-wave revert)
- `[reconcile-task-census]` — reconcile artefact-text against authority cites before user-facing report

## §14 — Disposition

**ACCEPT-rate: 95.0% line-item / 97.4% root-issue-collapsed.**

Both rates clear the §3Z ≥95% ACCEPT bar. V2 → V3 confirming cycle predicted; cohort LOCK expected at V3.

**ACCEPT dispositions (V2 closures of V1 REVISE-1):**

1. **F-V2-CH6-1 discharged** — SPEC `grep "UNLESS it admits one of the 12"` returns zero hits at V2 HEAD; W10 unconditional Stage-0 binding at SPEC §11/§12/§13 confirmed; 5-step inheritance chain verbatim repeated at three sites (lines 863, 923, 982) per `grep -c` count = 3.

2. **F-V2-CH6-2 discharged** — wave-numbering reconcile P3-B ↔ SPEC at 12/12 parity (W0..W11); P3-B §2.3..§2.14 subsections align with SPEC §3..§14; per-wave measurement-anchored exit gates preserved 12/12; gate content byte-identical except wave-id refresh per V2 fold authority.

3. **C5 orphan-kernel hole closed** — F-V2-P1ABC-RERECORD pinned to W10 Stage-0 UNCONDITIONALLY; SPEC §13:990 enforces "BEFORE any parse_only admit lands"; §13:1000 post-execution audit cite captures consumer manifest.

4. **C3 + C4 same-wave consumers verified** — `bbnf-simd` checkasm row for C3 (discharges Lock 14 v+1 non-JSON consumer inside admit wave); BBNF-self string-escape for C4 (CSS \HEXHEX carved out as separate-primitive measured-rejection); F-V2-CH2-1 + F-V2-CH2-2 confirmed per CH6 secondary check.

5. **All 12/12 revert protocols preserved**; 12/12 measurement-anchored exit gates; SPEC §1:220 + §1:227 + §14:1057 no-deferrals / paper-close / verb-tense discipline preserved.

6. **Three SPEC interaction observations resolved per intent**: W9 W1-only dependency (ACCEPT — intentional parallel-eligibility); W11 close-ceremony qualitatively different gate (ACCEPT — measurement is wave-disposition census + close-honesty checklist); W9 fused 34-row admit budget (ACCEPT — ≤450 LOC + ≤90-min cap).

**REVISE-2 disposition (non-blocking; V3 micro-fold candidate):**

- **F-V2-CH6-3 — P3-C wording mirror to SPEC unconditional binding.** Two P3-C cite-sites carry the old V1 conditional Stage-0 wording at `p3c-falsifiability-gates.md:36` (§1.2 wave manifest W10 row) + `p3c-falsifiability-gates.md:423` (§2.10 W10 exit-gate item 8). SPEC §13:982 is the binding artefact and reads UNCONDITIONALLY; P3-C should mirror this wording for textual consistency. The conditional residuals do NOT open a paper-close vehicle because (a) SPEC binds, (b) §2.10 IS the W10 wave-section so the conditional is harmless within that local scope. Recommend V3 micro-fold to replace both occurrences with SPEC's unconditional wording, eliminating textual asymmetry between P3-C and SPEC. Non-blocking; does not affect §3Z cohort LOCK trajectory.

**CH6 V2 disposition: ACCEPT (95.0% line-item / 97.4% root-issue-collapsed; load-bearing F-V2-CH6-1 + F-V2-CH6-2 discharged; one non-blocking V3 micro-fold flagged).**

## §15 — Output for aggregator

Path: `restart/skinny/tranches/sk-v14/research/p3/hardening/V2/CH6.md` (this file).

ACCEPT-rate: **95.0% line-item / 97.4% root-issue-collapsed** (clears §3Z ≥95% bar).

Cycle disposition: **ACCEPT** (V1's REVISE-1 root issue — F-V2-P1ABC-RERECORD Stage-0 three-way divergence and C5 orphan-kernel hole — fully closed at V2; one non-blocking REVISE-2 minor wording residual in P3-C §1.2 + §2.10 for V3 micro-fold; load-bearing F-V2-CH6-1 + F-V2-CH6-2 discharged with executable grep evidence).

Findings:
- (a) F-V2-CH6-1 final `grep "UNLESS it admits one of the 12"` returns ZERO hits at V2 HEAD across SPEC; the broader `grep "UNLESS"` also returns zero hits — the V1 conditional language is fully removed.
- (b) W10 unconditional Stage-0 binding confirmed at SPEC §11:863 + §12:923 + §13:982 with 7 site-anchored mentions of "UNCONDITIONALLY"/"unconditionally" naming W10 as the bound wave.
- (c) 5-step inheritance chain verbatim repeated at three sites per `grep -c` count = 3 (lines 863, 923, 982).
- (d) F-V2-CH6-2 wave-numbering reconcile preserves 12/12 measurement-based exit gates; P3-B §2.3..§2.14 subsection labels align with SPEC §3..§14 wave sections at full byte-identity except wave-id refresh.
- (e) C5 orphan-kernel hole closed; F-V2-P1ABC-RERECORD anchored to W10 Stage-0 with §13:990 "BEFORE any parse_only admit lands" entry-gate enforcement.
- (f) C3 same-wave consumer = bbnf-simd checkasm row for `byte_class_from_range_64` (discharges Lock 14 v+1 non-JSON consumer in same wave); C4 same-shape consumer = BBNF-self string-escape with CSS \HEXHEX carved out as separate-primitive measured-rejection.
- (g) 8/8 shortlist candidates carry CF-3 3-gate cell with same-wave consumer NAMED at V2.
- (h) 12/12 wave revert protocols preserved at SPEC §3..§14; W11 close-ceremony alternate form ("no source revert by default; reopen the producing wave or mark close blocked") preserved at line 1064.
- (i) SPEC §1:220 no-deferrals language + §1:227 CH7-V2 verb-tense addendum + §14:1057 paper-close pre-block preserved verbatim.
- (j) Three SPEC interaction observations (W9 W1-only dependency, W11 ceremony gate category, W9 fused 34-row admit budget) resolved per intent — all ACCEPT.
- (k) Residual F-V2-CH6-3 — P3-C §1.2:36 wave manifest + §2.10:423 exit-gate item 8 retain V1 conditional Stage-0 wording; non-blocking V3 micro-fold recommended for textual consistency with SPEC §13:982 unconditional binding.
- (l) V1 → V2 ACCEPT-rate movement: 73.7% → 95.0% line-item (+21.3 pp); 89.5% → 97.4% root-issue-collapsed (+7.9 pp). Cohort LOCK at V3 expected per §3Z trajectory.
