# CH3 REGRESSION — SK-V9 S-P3 Synthesis-Plan, Cycle V1

Lens: CH3 REGRESSION (`ORCHESTRATOR.md` §3W). Pass: S-P3 Synthesis-Plan.
Cycle: V1. Date: 2026-05-18.
Target: the seven S-P3 P3 artefacts at
`restart/skinny/tranches/sk-v9/research/p3/`, with the pre-blocked
ledger P3-E (`skv9-p3-E-preblocked-ledger.md`) as the primary scrutiny
surface.
Authority cross-checked: `skinny/REDRESS.md` (entries 1–93),
`restart/skinny/tranches/sk-v9/HANDOFF.md` §5, `restart/prompts/
ORCHESTRATOR.md` §3W, `skinny/RESULTS.md` `SK-V9-open` 38-row report.

---

## §1 — Method

CH3 verifies that no SK-V9 wave reopens a `skinny/REDRESS.md` route
without a cited entry and a material differential bound to a same-row
falsification gate. The S-P3 cohort dedicates one artefact — P3-E — to
this exact obligation. CH3's procedure:

1. Read all seven P3 artefacts plus the three authority surfaces.
2. Verify P3-E's per-wave pre-block list (§2) is complete against the
   six waves it covers and that each REDRESS-adjacent wave carries a
   §3 material differential.
3. Cross-check the five §3 differentials against the verbatim REDRESS
   text — union vs 92/50–72, codec vs 82, EOR3 vs 88, string-block vs
   83, CTZ vs 89.
4. Verify P3-F's per-wave "pre-blocked routes" sections match P3-E.
5. Verify P3-B's wave manifest sequences no wave that silently reopens
   a REDRESS route.
6. Verify the 4 typed-GO + 3 direct-GO rows are protected by P3-C's
   maintain envelope, and the W10b six-row WIN block is bound to the
   substrate + ASM waves.
7. Verify the SUPERSEDED list (35, 36, 37, 38, 46, 49, 70) and the
   "supersession ≠ re-admission" caveat.

Concrete cross-checks performed against `skinny/REDRESS.md`: entry 82
(SK-V7 W4, line 2285), entry 83 (SK-V7 W5, line 2318), entry 88 (SK-V7
W10, line 2508), entry 89 (SK-V7 W10b, line 2542), entries 91/92/93
(SK-V8 W2/W3/W4, lines 2620/2661/2692), the SK-V5 numbered entries
35/36/37/38/46/49/50 (lines 440/460/480/500/633/685/715), and the
typed-GO / direct-GO rows in `skinny/RESULTS.md`.

The disposition codes: **HOLD** — the artefact claim is correct and
load-bearing; **DEFECT** — a concrete error a V2 fold must repair;
**REVISE** — the claim is sound but under-specified or mis-bound and a
V2 fold must tighten it.

---

## §2 — Disposition table

| # | Scope | CH3 finding | Disp. |
|---|---|---|---|
| 1 | P3-E §3.2 — W-UE/W-RG vs REDRESS 92 | REDRESS 92 (`REDRESS.md:2687-2690`) routed precursor verbatim: "define the retained class/event grammar including numbers/literals and string quote ownership, prove the retained `ValueRef` cursor contract over that grammar, and only then reopen a measured structural-heavy parse row wave." P3-E §3.2 reproduces this faithfully; the W-RG-proof-before-W-UE-union ordering discharges the precursor. Material differential present + falsifiable. | HOLD |
| 2 | P3-E §2.4 — W-UE blanket pre-block vs REDRESS 92 sidecar list | REDRESS 92 (`:2673-2676`) enumerates the rejected shapes: REDRESS 51/53 cursor routes, sidecar producers, parser-owned structural cursors/facts, `tape_vs_tape` as production consumer, `UnionTape`, new `BackendShape`, new BIR, new directive, public substrate API, Tier B string-boundary/quote-backslash/parity. P3-E §2.4 reproduces every item with a per-item structural falsifier. Complete. | HOLD |
| 3 | P3-E §3.4 — W-UC vs REDRESS 82 (codec) | REDRESS 82 (`:2300-2310`) failure: `unicode_escapes/parse_only` FAIL at 82.1% of sonic, `y_string_unicode/parse_only` FAIL at 49.9%, `y_string_unicode/direct` Track 2 regressed 6.6%. P3-E §3.4 cites "82.1%" and "49.9%" verbatim. Differential (x4-batcher already wired at `lib.rs:402`; single-quartet binding fires only on x4 pre-filter reject; `parse_only`-only gate) is concrete and falsifiable. Confirmed `unescape_uxxxx_x4_neon` is live at `parse-that-regex/src/lib.rs:402`. | HOLD |
| 4 | P3-E §3.3 — W-AS vs REDRESS 88 (EOR3) | REDRESS 88 (`:2510-2540`): PMULL `bitmap_prefix_xor_64` rejected; failure mode = retire latency on escape-heavy/narrow rows (`unicode_escapes` −12.66%/−15.52%, `numbers` −10.04%). P3-E §3.3 differential — EOR3 is a 3-input vector XOR, no multiply, accelerates the *kept* scalar ladder, Lock-16 `FEAT_SHA3`-gated with scalar unconditional fallback. The differential is structural and PMULL stays rejected as default. The absolute-cycle-count caveat ("monotonic ordering EOR3 < PMULL is the load-bearing claim") is honest. | HOLD |
| 5 | P3-E §3.5 — W-UC vs REDRESS 83 (string-block) | REDRESS 83 (`:2320-2356`): StringBlock16 tiny probe rejected; zero of six rows crossed threshold, all six regressed >3%; failure mode = movemask shape too expensive for the *already-tiny* generated retained quote-pair probe. P3-E §3.5 differential — different call site (`match_string_at_quote_trusted_utf8` at `lib.rs:162`, the *full* path, vs the tiny `match_tiny_plain_string_with_cap::<16>`), 32-byte successor not wrapper, existing consumer. Confirmed `match_string_at_quote_trusted_utf8` at `lib.rs:162`. Gate binds the *combined producer+consumer* path, not the block-scan microbench — correct, since the µop-neutral-per-byte finding is the binding risk. | HOLD |
| 6 | P3-E §3.6 — W-AS vs REDRESS 89 (CTZ) | REDRESS 89 (`:2573-2579`): CSSC CTZ bulk consumer rejected; six rows dropped >2%. P3-E §3.6 differential — different call site (string-mask consumer's per-mask first-set extract vs the structural-scan bulk-emit pipeline), LOSS-rows-targeted vs WIN-rows-regressed. P3-E correctly grades this **HIGH risk** ("structurally adjacent to the route that produced this exact six-row regression") and binds it to the W10b six-row maintain gate as a hard precondition. Honest. | HOLD |
| 7 | P3-E §5 — W10b six-row WIN block fidelity | P3-E §5 table reproduces REDRESS 89 (`:2575-2579`) exactly: `canada` T1 −3.11%/T2 −4.14%; `citm_catalog` T1 −7.36%; `instruments` T1 −3.96%; `marine_ik` T1 −5.68%; `mesh` T1 −8.07%/T2 −7.46%; `numbers` T1 −6.44%. Every figure verified verbatim. | HOLD |
| 8 | P3-E §5 — W10b gate binding to substrate + ASM waves | P3-E §5 binds the no-regression maintain gate to W-AS (EOR3, CTZ) and W-UC (string-block widening), and *also* constrains W-UE (the union substrate keeps the SIMD producer unchanged but the consumed-by-move structural index "must not perturb the structural-scan-heavy WIN rows"). P3-C §2 echoes this: W3/W4/W5 all carry the six-row block as the binding maintain gate. Coverage is complete across every parse-loop-touching wave. | HOLD |
| 9 | P3-E §6 — SUPERSEDED list membership | All seven entries (35, 36, 37, 38, 46, 49, 70) verified present in `skinny/REDRESS.md`: 35 `:440`, 36 `:460`, 37 `:480`, 38 `:500`, 46 `:633`, 49 `:685`, 70 within the SK-V6 W3 Candidate-12 section. Each entry header matches P3-E §6's one-line summary. | HOLD |
| 10 | P3-E §6 — superseder chains | 35 → 40/48/71/81; 36/37 → 85/86 (Lock 14 Phase A–D); 38 → SK-V6/V7 crate restructure; 46 → 71/81; 49 → 66; 70 → 71. Items 71 (`:1944`), 81 (`:2252`), 85 (`:2399`), 86 (`:2431`) verified present. The chains are coherent: each superseder is a later monotonic admit (or, for 49, a later *reject* — 66 — that closes the surface). | HOLD |
| 11 | P3-E §6 — "supersession ≠ re-admission" caveat | The caveat is explicitly stated and load-bearing: SUPERSEDED means the entry is not a *standalone* pre-block, not that the rejected route reopens. REDRESS 49's source-hook surface stays closed by 66; REDRESS 70's first-attempt route stays rejected (71 is a different host/API route). This is the correct CH3 posture — without the caveat, "SUPERSEDED" could be misread as license to reopen. Holds. | HOLD |
| 12 | P3-E §6 — entry 49 disposition nuance | Entry 49 (`:685`) header reads "generated source hooks are ADMITTED" — i.e. 49 was itself an *admit*, not a reject. P3-E §6 frames it as "Generated source-hook string ADMIT (direct)" superseded by 66 (a *rejection*). The framing is correct (an admit whose forward-producer surface is later closed by a reject), and §6 spells it out — "the surface exists but the route is closed." No defect; the nuance is handled. | HOLD |
| 13 | P3-E §4 hard pre-blocks — REDRESS 93 fidelity | REDRESS 93 (`:2724-2727`) verbatim: "Do not reopen scalar parent folding under another name unless a later wave first supplies a W4/V9-aware checked gate, full-table maintain measurement, and an independent Track 2 digest-arithmetic backstop." P3-E §4 item 4 reproduces all three conditions. Hard pre-block correctly stated. | HOLD |
| 14 | P3-E §4 — hard pre-block completeness vs HANDOFF §5 | HANDOFF §5 lists eight pre-blocked routes; P3-E §4 reproduces all eight (items 1–8) and adds five class umbrellas (items 9–13) sourced from P1-V3-F §3.2. The umbrella additions are legitimate — HANDOFF §5 itself says "the full prior pre-block ledger … is binding by reference." Superset, not contradiction. | HOLD |
| 15 | P3-E §1 — wave-letter vs numeric-manifest mapping | P3-E §1 uses lettered waves (W0, W-AC, W-RG, W-UE, W-UC, W-AS) and states "P3-B owns the canonical W0…W{n} numbering." But W-UC bundles codec+string-block and W-AS bundles EOR3+CTZ — and the three sibling artefacts each split those bundles differently (see #16). P3-E's lettered scheme cannot be deterministically projected onto any one numeric manifest. This is the root coupling defect of the cohort. | DEFECT |
| 16 | P3-B vs P3-C vs P3-F — wave manifest divergence | Three artefacts give three manifests. **P3-B** (`§2`): W0–W5; W4 = "aarch64 ASM consumers — unicode codec + string-block widening" (all paired); W5 = close. **P3-C** (`§1.4`): W0–W5; W4 = codec + string-block paired; W5 = aarch64 ASM kernels (EOR3, CSSC CTZ, structural-bitmap). **P3-F SPEC + dispatch** (`Section 2` / manifest): W0–W6; W4 = string-block widening; W5 = codec; W6 = close. The codec, the string-block widening, and the ASM kernels land in a *different wave number in every artefact*. This is a CH3 defect: the per-wave pre-block binding cannot be verified when the wave boundaries themselves disagree. | DEFECT |
| 17 | P3-F §4–§8 — per-wave pre-blocked sections vs P3-E ledger | P3-F SPEC §4 (W1) cites REDRESS 91; §5 (W2) cites 92 + 60–72; §6 (W3) cites 92 + the sidecar list; §7 (W4 string-block) cites 83 + 73; §8 (W5 codec) cites 82 + 88. The *citations* are individually correct, but because P3-F's W4=string-block / W5=codec while P3-E §2.5 bundles both into one wave W-UC, the SPEC's §7 carries the REDRESS 83 differential and §8 carries the REDRESS 82 differential **split across two waves** — yet P3-E §3.4's honest verdict (the codec alone closes zero rows; admission is the §6.4 same-wave conditional, codec paired with the string-scanner widening, "a codec-only orphan is itself pre-blocked"). P3-F §8 (codec as a standalone W5 *after* string-block W4) does pair them sequentially but the SPEC §8 exit gate still admits codec rows in W5 — this is exactly the codec-only-orphan shape P3-E §3.4 pre-blocks. The SPEC's per-wave split contradicts P3-E's same-wave-conditional rule. | DEFECT |
| 18 | P3-F §8 — codec admission basis vs P3-E §3.4 pre-block | P3-E §3.4 verdict: "the codec ships paired with the string-block widening … in W-UC, never alone. A codec-only orphan is a REDRESS-82-style orphan and is itself pre-blocked." P3-F SPEC §8 makes W5 the codec wave, *separate* from W4 string-block, with its own `G-W5-CODEC` exit gate admitting `unicode_escapes`/`y_string_unicode`/`unicode_mixed` per-row. Although W5's entry gate is "W4 closed," the codec still carries its own row-admission gate in a wave with no string-scanner edit — re-creating the REDRESS-82 orphan shape P3-E explicitly pre-blocks. P3-C §4.3 ("the codec wave would close zero rows … a paper-close under CH6") independently condemns the P3-F split. | DEFECT |
| 19 | P3-C §1.4 / §4 — codec+string-block one-wave rule | P3-C is correct and aligned with P3-E §3.4: it manifests codec + string-block as one wave (W4) and §4.3 proves the pairing is the *only* non-vacuous sequencing. P3-C is the artefact that honours the P3-E pre-block; P3-F is the artefact that breaks it. The V2 fold must adopt the P3-C manifest. | HOLD |
| 20 | P3-B §2 — does the manifest silently reopen a REDRESS route? | P3-B's W1–W5 sequence carries no wave that reopens a route absent a citation: W1 (P2-C) cites 91; W2 (P2-B) cites 92; W3 (P2-A) cites 92 + sidecar list; W4 (P2-D+P2-E) cites 82/83/88/89. The topological order (proof W2 → union W3 → consumers W4) honours the REDRESS 92 hard pre-block "W-UE cannot precede W-RG." No silent reopen in P3-B. The defect in P3-B is only the manifest *disagreement* with P3-C/P3-F (#16), not a reopen. | HOLD |
| 21 | P3-B §4 — SHA3 EOR3 authority vs REDRESS 88 | P3-B §4 sequences the SHA3 EOR3 slice as a `FEAT_SHA3`-conditional branch with a scalar fallback, explicitly noting that an *unconditional* SHA3 default "would be a pre-blocked route (HANDOFF §5: PMULL prefix-XOR … as default hot paths) and would require Omega." The conditional framing keeps W-AS under SK-V9 authority and honours REDRESS 88. Correct. | HOLD |
| 22 | P3-C §2 W1 — 4 typed-GO rows protected | P3-C §2 W1 maintain envelope (a): the four typed-GO rows — `twitter`, `update_center`, `mesh`, `marine_ik` `real_typed_struct A / GO` — each must hold `≥ sonic_strict / 1.10`. Verified against `skinny/RESULTS.md`: those four rows carry `real_typed_struct A / GO` (twitter +0.7%, update_center −4.5%, mesh +4.6%, marine_ik +25.2%). The four-row set is exact. | HOLD |
| 23 | P3-C §2 W1 — 2 direct rows held | P3-C §2 W1 maintain envelope (b): `apache_builds/direct_to_struct` (N-direct/NO-GO) and `citm_catalog/direct_to_struct` (A/GO) hold their `SK-V9-open` verdicts. Verified in `RESULTS.md` (`apache_builds/direct` N-direct/NO-GO; `citm_catalog/direct` A/GO). Correct. | HOLD |
| 24 | P3-C §2 — 3 direct-GO rows protected | P3-C §5 source 9 names the three direct-GO rows: `citm_catalog`, `marine_ik`, `unicode_basic` `direct_to_struct A / GO`. Verified against `RESULTS.md` — exactly those three rows carry `direct_to_struct A / GO`. P3-C §2 W5 maintain envelope requires "the four typed-GO + three direct-GO rows … show no delta beyond noise" on the ASM-kernel wave. The 3 direct-GO rows are protected. | HOLD |
| 25 | P3-C §2 — direct-GO protection on W4 | P3-C §2 W4 maintain envelope names the three *unicode* direct rows (`unicode_escapes/direct`, `y_string_unicode/direct`, `unicode_mixed/direct`) as REDRESS-82 no-regression guards — but does NOT restate the three direct-GO rows (`citm_catalog`/`marine_ik`/`unicode_basic`) for W4. W4 (codec + string-block) touches `match_string_at_quote_trusted_utf8`, a path the typed/direct projection also reaches. The 3 direct-GO rows should be in W4's maintain envelope too, not only W5's. Under-specified. | REVISE |
| 26 | P3-C §2 W3 — typed/direct no-leak clause | P3-C §2 W3 maintain envelope: "Track 2 / `path!` / direct-to-struct / SinkOnly rows show no delta beyond noise — the class column touches only retained-view consumers." This protects the typed-GO and direct-GO rows on the substrate wave by the no-cross-substrate-leak clause (P2-A §4.4 #4). Correct, though it would be tighter to name the seven GO rows explicitly as P3-C §2 W1 does. | REVISE |
| 27 | P3-E §2.3 — W-RG vs REDRESS 71 orthogonality | P3-E §2.3 states REDRESS 71 (the *admitted* host/API typed-DirectBuild route, `:1944`) is orthogonal to W-RG: the proof sits on the `OffsetTape` retained lowerer, 71 on the `SinkOnly` direct lowerer. Verified — 71 is an admit, not a reject, so it is not a pre-block; P3-E correctly treats it as orthogonal rather than a route to honour. | HOLD |
| 28 | P3-E §4 item 13 — REDRESS 28+33 NEON tiny-string | P3-E §4 item 13 keeps `match_tiny_plain_string` as a Class-A retained-G fix permanently pre-blocked (REDRESS 28+33). P3-E §2.6 W-AS confirms W-AS does not wire Class-A NEON `match_tiny_plain_string` into a field-name match-arm chain. Note: P3-C §2 W5 hot-leaf falsifier mentions `match_tiny_plain_string` aggregate self-time as a *measurement* target on W4 — that is a self-time observation, not a reopen of the REDRESS-33 rejected wiring. No conflict. | HOLD |
| 29 | P3-F integration note — siblings "not present" | P3-F's header states "sibling P3 artefacts P3-A … P3-E were not present at `research/p3/` when this draft was authored" and marks `[INTEGRATE P3-x]` placeholders. This is the structural cause of #16/#17/#18: P3-F was drafted blind to P3-E and chose its own (7-wave) manifest. The `[INTEGRATE P3-E]` markers in P3-F §4–§8 are unresolved — the SPEC is not yet reconciled with the ledger. A V2 fold must perform the integration, not merely leave the marker. | DEFECT |

---

## §3 — Aggregate verdict

P3-E itself — the dedicated pre-block ledger — is **substantially
correct**. Every one of the five material differentials (union vs
92/50–72, codec vs 82, EOR3 vs 88, string-block vs 83, CTZ vs 89) is
cross-checked verbatim against `skinny/REDRESS.md` and holds; the
differentials are concrete, falsifiable, and bound to same-row gates.
The W10b six-row WIN block is reproduced exactly from REDRESS 89 and is
correctly bound to every parse-loop-touching wave (W-AS, W-UC, and the
constraining clause on W-UE). The SUPERSEDED list (35, 36, 37, 38, 46,
49, 70) is verified entry-by-entry, the superseder chains are coherent,
and the load-bearing "supersession ≠ re-admission" caveat is explicit
and correct. The four hard-pre-block obligations (REDRESS 91/92/93,
sidecar class) and the five class umbrellas are complete against
HANDOFF §5. No SK-V9 wave, as P3-E describes it, silently reopens a
rejected route.

The cohort, however, does **not converge**. Four DEFECT dispositions
(#15, #16, #17, #18, #29) trace to a single root: **the seven P3
artefacts carry three mutually inconsistent wave manifests.** P3-E uses
lettered waves and delegates numbering to P3-B; P3-B gives W0–W5 with
ASM+codec+string-block fused into W4; P3-C gives W0–W5 with codec+
string-block in W4 and ASM kernels split into W5; P3-F (SPEC + dispatch)
gives W0–W6 with string-block in W4 and codec in W5. The codec, the
string-block widening, and the ASM kernels each land in a different
wave number in every artefact.

For CH3 this is not cosmetic. P3-E §3.4's honest verdict pre-blocks a
**codec-only orphan wave** as a REDRESS-82-style orphan — and P3-F's
SPEC §8 manifests exactly that: a standalone W5 codec wave with its own
row-admission gate, separated from the W4 string-block wave. The SPEC,
as drafted, reopens the REDRESS-82 orphan shape that P3-E and P3-C both
pre-block. P3-C is the artefact that honours the ledger (codec + string-
block as one inseparable wave, §4.3 proving the pairing is the only
non-vacuous sequencing); P3-F is the artefact that breaks it; P3-E
correctly states the rule but does not pin the numbering that would
have prevented the drift.

Verdict: **REVISE.** P3-E's ledger content is accepted. The cohort
fails convergence on the manifest inconsistency. The V2 fold must
unify the three manifests onto the P3-C shape (codec + string-block
paired in one wave), then re-bind P3-E's lettered pre-blocks and
P3-F's SPEC §4–§9 per-wave pre-block sections to that single canonical
numbering. CH3 ACCEPT-rate this cycle: 24/29 HOLD, 2/29 REVISE, 5/29
DEFECT — below the §3Z 95% threshold; the pass does not advance.

---

## §4 — REDRESS reopens requiring V2 fold

The following must fold into the S-P3 V2 dispatch. None is a defect in
P3-E's ledger *content*; all are wave-binding inconsistencies that, if
shipped, would let a wave reopen a route P3-E correctly pre-blocked.

1. **Unify the wave manifest (DEFECT #15, #16, #29).** P3-B, P3-C, and
   P3-F must share one numbering. Adopt the **P3-C shape**: W0
   telemetry-lock (closed); W1 Apache/CITM (P2-C); W2 retained-grammar
   proof (P2-B); W3 union event-model (P2-A); W4 codec + string-block
   widening **paired** (P2-E + P2-D §4); W5 aarch64 ASM kernels (P2-D
   §5 — EOR3, CSSC CTZ, structural-bitmap); W6 close. P3-E §1's
   lettered waves then map deterministically: W-AC→W1, W-RG→W2,
   W-UE→W3, W-UC→W4, W-AS→W5. Re-author P3-B §2 and the P3-F SPEC
   §2 manifest to this single table.

2. **Repair the P3-F SPEC codec-orphan (DEFECT #17, #18).** P3-F SPEC
   §7/§8 split string-block (W4) and codec (W5) into separate waves
   with separate row-admission gates. This reopens the REDRESS-82
   orphan shape P3-E §3.4 explicitly pre-blocks ("a codec-only orphan
   is itself pre-blocked") and P3-C §4.3 independently condemns. The
   V2 SPEC must merge them into one wave (W4) with the conditional-
   admission rule of P3-C §4.2 as the single exit gate — the codec
   admits as a *primitive with a measured contribution*; the unicode
   *rows* admit only on the combined codec + string-block measured
   Mbps. Resolve the unresolved `[INTEGRATE P3-E]` markers in P3-F
   §4–§8 in the V2 fold rather than leaving them.

3. **Extend the W4 maintain envelope to the 3 direct-GO rows (REVISE
   #25).** P3-C §2 W4 names only the three *unicode* direct rows as
   REDRESS-82 no-regression guards; it omits the three direct-GO rows
   (`citm_catalog`, `marine_ik`, `unicode_basic` `direct_to_struct
   A / GO`). W4 rewires `match_string_at_quote_trusted_utf8`, a path
   the direct projection also reaches. The V2 P3-C must add the three
   direct-GO rows to W4's maintain envelope, as it already does for W5.

4. **Name the 7 GO rows explicitly in the W3 envelope (REVISE #26).**
   P3-C §2 W3 protects the typed/direct rows only by the generic
   "no delta beyond noise" no-leak clause. For parity with the W1
   envelope (which names the four typed-GO rows explicitly) and to
   make the gate mechanically checkable, the V2 P3-C W3 envelope must
   name the four typed-GO + three direct-GO rows explicitly.

No reopen survives into a dispatched wave provided the V2 fold lands
items 1–2; items 3–4 tighten the maintain envelope. P3-E's pre-block
ledger content carries forward unchanged into V2 — it is the manifest
and the SPEC that must be reconciled to it.
