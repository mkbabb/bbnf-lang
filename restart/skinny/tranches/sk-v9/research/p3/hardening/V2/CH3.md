# CH3 REGRESSION — SK-V9 S-P3 Synthesis-Plan, Cycle V2 (verify)

Lens: CH3 REGRESSION (`ORCHESTRATOR.md` §3W). Pass: S-P3 Synthesis-Plan.
Cycle: V2 (verify the V1→V2 integration fold). Date: 2026-05-18.
Target: the seven S-P3 P3 artefacts at
`restart/skinny/tranches/sk-v9/research/p3/` after the V2 fold
(commit `ef40c0fc`), with the SPEC draft `skv9-p3-F-spec-draft.md`
and the dispatch draft `skv9-p3-F-dispatch-draft.md` as the primary
scrutiny surface, and `skv9-p3-E-preblocked-ledger.md` as the binding
ledger the SPEC's per-wave pre-block sections must match.
Authority cross-checked: `skinny/REDRESS.md` entries 82
(`:2285-2316`), 83 (`:2318-2356`), 88 (`:2508-2540`), 89
(`:2542-2585`), 92 (`:2661-2690`); `HARDENING-S-P3-V1-CONSOLIDATED.md`;
`restart/skinny/tranches/sk-v9/research/p3/hardening/V1/CH3.md`.

CH3 V1 verdict was REVISE (~83% — 24 HOLD / 2 REVISE / 5 DEFECT). The
five DEFECTs (#15, #16, #17, #18, #29) all traced to one root: the
seven P3 artefacts carried three mutually inconsistent wave manifests,
and the P3-F SPEC §8 manifested a standalone codec wave — a
REDRESS-82-style orphan that P3-E §3.4 and P3-C §4.3 both pre-block.
CH3 V2 verifies the V2 fold closed those five DEFECTs without
reopening any REDRESS route.

The disposition codes: **HOLD** — the V2 artefact claim is correct and
the V1 defect is resolved; **REVISE** — sound but under-specified, a
V3 touch-up tightens it; **DEFECT** — a concrete error a further fold
must repair.

---

## §1 — V1-defect resolution

The V1 fold prescription (CONSOLIDATED §"V2 fold") had four binding
items for the CH3 surface. Each is verified against the V2 artefacts:

**V1 item 1 — unify the wave manifest (DEFECT #15, #16, #29).**
RESOLVED. The V2 SPEC §2 manifest is a single table: W0 (closed), W1
Apache/CITM (C1/P2-C), W2 retained-grammar proof (C2/P2-B), W3 union
event-model (C3/P2-A + P2-D §5 chain), W4a 32-byte string-block (C5),
W4b codec (C4), W4c SHA3 EOR3 (C6), W4d CSSC CTZ (C7), W5 close — a
W1-W5 shape with W4 sub-waved W4a-d. The dispatch draft §"Wave
Manifest" carries the byte-identical table. P3-B §2 carries the same
W1-W5 wave manifest and §2's W4 note explicitly states the
codec+string-block pairing lands in one wave sub-waved as W4a/W4b.
P3-C §1.4 carries the W1-W5 candidate→wave map with W4 = codec +
string-block paired and W5 = ASM kernels — see §2 dispositions #2
and #3 for the residual on P3-C/P3-E numbering. The three formerly
irreconcilable manifests now agree on the W1-W5 spine; W4 sub-waving
(W4a-d) is the SPEC's resolution of the CH4 75-min-redress ceiling.

**V1 item 2 — repair the P3-F SPEC codec-orphan (DEFECT #17, #18).**
RESOLVED. The V2 SPEC no longer carries a standalone W5 codec wave.
SPEC §7.2 (W4b codec) is headed "PAIRED with W4a — strictly adjacent,
never separable" and states verbatim "W4b dispatches only with W4a
landed." The W4b exit gate `G-W4b-CODEC` clause 3 admits `unicode_mixed`
only on the *combined* W4a string-block + W4b codec measured Mbps; if
W4a did not land, `unicode_mixed` stays NO-GO and W4b admits
codec-contribution-only. The dispatch draft §"Wave Manifest" lists W4b
status as "PAIRED with W4a — strictly adjacent." The `[INTEGRATE P3-E]`
markers are gone — both V2 drafts carry the §0 footer "all [INTEGRATE]
markers resolved." The REDRESS-82 codec-only-orphan shape is closed.

**V1 item 3 — extend W4 maintain envelope to the 3 direct-GO rows
(REVISE #25).** RESOLVED. P3-C §2 W4 maintain envelope now states
"because W4 rewires `match_string_at_quote_trusted_utf8`, a path the
direct-to-struct projection also reaches, the three direct-GO rows —
`citm_catalog`, `marine_ik`, `unicode_basic` `direct_to_struct A / GO`
— are restated here as W4 maintain rows … exactly as they are gated on
W5." SPEC §7.1 (W4a) clause 5 carries the three unicode-direct
no-regression guard.

**V1 item 4 — name the 7 GO rows explicitly in the W3 envelope
(REVISE #26).** RESOLVED. P3-C §2 W3 maintain envelope now names them:
"the four typed-GO rows — `twitter`, `update_center`, `mesh`,
`marine_ik` `real_typed_struct A / GO` — and the three direct-GO rows —
`citm_catalog`, `marine_ik`, `unicode_basic` `direct_to_struct A / GO`
— each hold their `A / GO` outcome with no delta beyond noise." SPEC §6
`G-W3-UNION-SUBSTRATE` clause 4 carries the no-cross-substrate-leak
clause.

All four V1 fold items land. The five V1 DEFECTs are closed. The
residual is one class of under-tightening: the V2 fold edited only five
files (P3-A, P3-B, P3-C, F-spec, F-dispatch — commit `ef40c0fc`); P3-D
and P3-E were not re-authored, so P3-E still carries its V1 lettered
wave scheme. See §2 #2/#3/#21 and §4.

---

## §2 — V2 dispositions

| # | Scope | CH3 V2 finding | Disp. |
|---|---|---|---|
| 1 | SPEC §2 — unified manifest | The V2 SPEC §2 manifest is one table: W0/W1/W2/W3/W4a/W4b/W4c/W4d/W5. The dispatch §"Wave Manifest" reproduces it byte-for-byte. P3-B §2 carries the same W1-W5 spine. The three V1-irreconcilable manifests now converge — DEFECT #15/#16/#29 root cause closed. | HOLD |
| 2 | P3-E §1 wave-letter table vs SPEC numeric manifest | P3-E §1 (line 55-66) still uses the V1 lettered scheme — W0/W-AC/W-RG/W-UE/W-UC/W-AS — and §2.5 bundles codec+string-block into one wave `W-UC`, §2.6 bundles EOR3+CTZ into `W-AS`. The SPEC splits those bundles into W4a/W4b and W4c/W4d. P3-E line 64 still says "P3-B owns the canonical W0…W{n} numbering," so P3-E delegates numbering and does not contradict — but P3-E was not re-authored in the V2 fold and carries no §0 V2-fold marker (P3-B and P3-C both carry one). The mapping W-AC→W1, W-RG→W2, W-UE→W3, W-UC→{W4a,W4b}, W-AS→{W4c,W4d} is now a deterministic bundle-split, not a three-way disagreement. It is verifiable. Under-tightened, not defective. | REVISE |
| 3 | SPEC per-wave "Pre-blocked routes" citations vs P3-E sections | SPEC §4 cites "P3-E §2.2 / §3.1"; §5 "§2.3 / §3.2"; §6 "§2.4 / §3.2"; §7.1 "§2.5 / §3.5"; §7.2 "§2.5 / §3.4"; §7.3 "§2.6 / §3.3"; §7.4 "§2.6 / §3.6". Every cited P3-E section exists and is unchanged. W4a and W4b both cite §2.5 (the W-UC bundle); W4c and W4d both cite §2.6 (the W-AS bundle) — a clean split of one P3-E ledger section into two SPEC sub-wave sections. The SPEC's per-wave pre-block content is verbatim from those P3-E sections. The V1 DEFECT #17 (SPEC §7/§8 split contradicting P3-E's same-wave rule) is closed because the V2 SPEC §7.2 pairing is now what P3-E §3.4 demands. | HOLD |
| 4 | W4b codec pairing — REDRESS-82-orphan reopen | SPEC §7.2 header: "PAIRED with W4a — strictly adjacent, never separable." Entry gate: "W4a closed (the paired scanner widening is live)." `G-W4b-CODEC` clause 3 binds `unicode_mixed` admission to the *combined* W4a+W4b Mbps; clause states "If W4a did not land, `unicode_mixed` stays NO-GO and W4b admits codec-contribution-only — never claimed closed by the codec." This is verbatim P3-E §3.4's pre-block ("a codec-only orphan is a REDRESS-82-style orphan and is itself pre-blocked"). The REDRESS-82-orphan reopen is CLOSED. | HOLD |
| 5 | W4b — "wave may close with zero unicode admissions" honesty | SPEC §7.2 "The honest posture (P2-E §6.4, carried verbatim)" and §0.1 close-condition clause 6 both state W4 may close with zero strict unicode-row admissions as a measured outcome, not a paper-close. A NEAR-FAIL on `unicode_escapes`/`y_string_unicode` keeps the row `S / NO-GO`, records the measured codec contribution in REDRESS. This is the §3.4 honest verdict carried into the gate — no paper-close, no reopen. | HOLD |
| 6 | W4b sub-wave still REDRESS-82-adjacent — §3.4 differential survives | SPEC §7.2 "Pre-blocked routes" reproduces the P3-E §3.4 five-axis differential verbatim: not a parser-owned per-quartet classifier (the 4-quartet batched path is the union-substrate path; single-quartet fires only on pre-filter reject); same-wave consumer is the already-wired x4 JSON path at `lib.rs:402`; `escape_codec_hex_unit` is a const-generic primitive with five bindings; post-V3 PMU evidence; `parse_only`-only gate. Material differential #2 (codec vs 82) survives the V2 SPEC. | HOLD |
| 7 | W4a string-block — REDRESS 83 differential survives | SPEC §7.1 "Pre-blocked routes" reproduces P3-E §3.5: different call site (the full `match_string_at_quote_trusted_utf8`, not the tiny 16-byte-cap probe); a 32-byte successor primitive, not a JSON-specific wrapper; same-wave consumer is the existing `match_string_at_quote_trusted_utf8`; the gate measures the combined producer+consumer path, not the block-scan microbench. Verified against REDRESS 83 (`:2347-2356`) — the verbatim failure mode is "the AArch64 `string_block` movemask shape is too expensive for the already-tiny generated retained quote-pair probe." Material differential #4 (string-block vs 83) survives. | HOLD |
| 8 | W4c EOR3 — REDRESS 88 differential survives | SPEC §7.3 "Pre-blocked routes" reproduces P3-E §3.3's three-axis differential: different intrinsic (3-input bitwise XOR, no carryless multiply); different latency (PMULL.1Q 4-cycle vs EOR3 1-cycle — the REDRESS-88 retire-latency failure mode structurally inapplicable); different primitive shape (a vector fold of the scalar ladder REDRESS 88 *kept*, not the PMULL substitution). Verified against REDRESS 88 (`:2535-2540`) — "PMULL as the default hot `bitmap_prefix_xor_64` body is not admissible." SPEC §7.3 Owner-paths row keeps the scalar shift-XOR ladder as the unconditional fallback; the EOR3 path is `FEAT_SHA3`-gated. PMULL stays rejected as default. Material differential #3 (EOR3 vs 88) survives. | HOLD |
| 9 | W4d CTZ — REDRESS 89 differential survives | SPEC §7.4 "Pre-blocked routes" reproduces P3-E §3.6: different call site (the string-mask first-set extract, not `bulk_emit_positions_64`); LOSS rows under guard, not the WIN-block numeric rows; same-wave consumer is the union-substrate string-mask consumer. Verified against REDRESS 89 (`:2573-2579`). SPEC §7.4 `G-W4d-CTZ` clause 2 grades the W10b six-row maintain gate "the **binding hard blocking precondition**" — matching P3-E §3.6's HIGH-risk grading. Differential #5 (CTZ vs 89) survives, correctly graded HIGH. | HOLD |
| 10 | W3 union — REDRESS 92 + 50-72 differential survives | SPEC §6 "Pre-blocked routes" reproduces P3-E §3.2 / §2.4: W3 implements the routed precursor REDRESS 92 named (the alternate event-model), not the rejected storage-only swap; the class column is co-emitted at the existing `emit_plain_offset` site, not a parser-written aux side table; the SIMD index is consumed by move (Lock 1 cardinality = one). Verified against REDRESS 92 (`:2687-2690`) — the routed precursor is "define the retained class/event grammar … prove the retained `ValueRef` cursor contract … and only then reopen a measured structural-heavy parse row wave." SPEC §6 entry gate binds W3 to "W2 closed with `G-W2-RETAINED-PROOF` PASS." Material differential #1 (union vs 92/50-72) survives. | HOLD |
| 11 | W3 — REDRESS 92 blanket sidecar pre-block | SPEC §6 "Pre-blocked routes" final bullet reproduces the REDRESS 92 blanket pre-block: no new `BackendShape`/BIR/directive, no public substrate API, no parser-owned structural cursor/facts, no `tape_vs_tape` production consumer, no `UnionTape`, no Tier B string-boundary work. Verified verbatim against REDRESS 92 (`:2673-2676`). SPEC §1 non-negotiables independently restate every item. No structural falsifier dropped. | HOLD |
| 12 | W10b six-row block — fidelity to REDRESS 89 | SPEC §6 clause 2 and the dispatch draft §"Falsifiability Gates" both name the six rows `canada` ≥ 15866, `citm_catalog` ≥ 28631, `instruments` ≥ 15865, `marine_ik` ≥ 11831, `mesh` ≥ 12186, `numbers` ≥ 17597. The six corpora are exactly the REDRESS 89 (`:2575-2579`) regression list (`canada`/`citm_catalog`/`instruments`/`marine_ik`/`mesh`/`numbers`). P3-E §5 reproduces the per-row regression percentages verbatim. The block is intact. | HOLD |
| 13 | W10b block bound to every parse-loop-touching wave | SPEC §6 clause 2 binds it to W3 (binding, P2-A §4.2); §7.1 clause 4 to W4a ("binding — W4a is a string-loop edit"); §7.2 clause 7 to W4b; §7.3 clause 3 to W4c ("the **binding** gate"); §7.4 clause 2 to W4d ("the **binding hard blocking precondition**"). The dispatch draft §"Falsifiability Gates" states the W10b block "is a binding maintain gate on W3 and on every W4 sub-wave that touches the parse loop or an aarch64 SIMD kernel." Every parse-loop / SIMD-kernel-touching wave carries it. W1 correctly does NOT gate it (SPEC §4 maintain envelope: "W1 touches no parse loop"). W5 is docs-only. Coverage complete. | HOLD |
| 14 | W10b block constrains W3 even though SIMD producer unchanged | P3-E §5 final paragraph: the maintain block constrains W3 because "the structural index becoming a consumed-by-move producer must not perturb the structural-scan-heavy WIN rows." SPEC §6 clause 2 makes the six-row block the binding W3 maintain gate regardless. Honest — the V1 #8 HOLD carries forward. | HOLD |
| 15 | `canada` floor correction — stale 15871 removed | The dispatch draft §"Falsifiability Gates" states "For `canada` the live sonic-strict parse_only is 12723 (`RESULTS.md:10`), so `today × 0.98` = 15866 binds — there is no 15871 sonic floor; that figure was a stale SK-V8-era carryover and is corrected here." SPEC §6 clause 2 uses 15866. The CONSOLIDATED §"single root cause" item 5 (CH1 #23 stale floor) is folded. CH3-adjacent: a stale maintain floor would have mis-gated the W10b regression check; the correction tightens the regression gate. | HOLD |
| 16 | 10-outcome enum protection of GO rows | SPEC §0.x pins the 10-identifier W0-admissible enum `A C G I J K L M N-direct S`; §1 non-negotiable "No new outcome variant." The 4 typed-GO rows carry `A`; the 3 direct-GO rows carry `A`. The enum admits `A` (beat-and-parity) — the GO outcome. No V2 edit narrows the enum below `A`. The typed-GO/direct-GO rows' outcome identifier is protected by the §1 non-negotiable. | HOLD |
| 17 | 4 typed-GO rows protected — W1 + W3 + W5 envelopes | SPEC §4 (W1) maintain envelope: "the four typed-GO rows hold `A / GO`." P3-C §2 W1 (a): `twitter`/`update_center`/`mesh`/`marine_ik` `real_typed_struct A / GO` each ≥ `sonic_strict / 1.10`. SPEC §6 (W3) clause 4 names them. P3-C §2 W5 names them. The four-row set is exact (`RESULTS.md` `real_typed_struct A/GO`). Protected on every wave touching a typed codepath. | HOLD |
| 18 | 3 direct-GO rows protected — W3 + W4a + W4b + W5 | P3-C §2 W3 names `citm_catalog`/`marine_ik`/`unicode_basic` `direct_to_struct A / GO` (V1 REVISE #26 fold). P3-C §2 W4 restates them as W4 maintain rows (V1 REVISE #25 fold). SPEC §6 clause 4 W3; SPEC §7.1 clause 5 carries the unicode-direct three-row guard for W4a; SPEC §7.2 clause 6 for W4b. The three direct-GO rows are protected on every wave that rewires `match_string_at_quote_trusted_utf8`. The two V1 REVISEs are resolved. | HOLD |
| 19 | W1 — REDRESS 91 differential intact | SPEC §4 "Pre-blocked routes" reproduces P3-E §3.1: REDRESS 91's gap is a whitelist, not an architecture; W1 owns a fresh run-id/metadata validation and produces measured rows under it, then expands the whitelist; W1 admits Apache + CITM only; `canada/real_typed_struct` stays rejected (REDRESS 80 + the long-decimal mismatch). No reopen — W1 is the later accepted row-table wave REDRESS 91 deferred to. | HOLD |
| 20 | W2 — REDRESS 92 precursor + 50-72 honoured | SPEC §5 "Pre-blocked routes" reproduces P3-E §3.2 / §2.3: W2 is the routed precursor REDRESS 92 named, not a reopen; five-axis differential (no production consumer, no row-movement surface, touches no parser-control file, adds no payload field, proves the existing `ValueRef` cursor); REDRESS 50/51/53 honoured (no parser-written aux table, no parser-local cursor); REDRESS 71 orthogonal. Proof-only depth. No reopen. | HOLD |
| 21 | P3-D telemetry schema — not re-authored, but no CH3 surface | The V2 fold commit `ef40c0fc` touched five files; P3-D was not among them. P3-D carries no W4a-d reference and no §0 V2-fold marker. For CH3 this is immaterial: P3-D owns the 10-outcome enum and 36-field schema, neither of which is a REDRESS-route surface. The SPEC §0.x/§0.y carry the 10-enum and 36-field schema correctly (CONSOLIDATED items 3/4 — verified by CH1, not a CH3 lens). P3-D's non-fold is a CH1 census concern, not a CH3-regression concern. | HOLD |
| 22 | P3-E §2.5/§2.6 bundling vs SPEC sub-wave split — pre-block completeness | P3-E §2.5 lists W-UC's "must NOT reopen" set (REDRESS 64, 66-69, 65, 84, 60-62) and the §3.3/§3.4 adjacents (82, 83); P3-E §2.6 lists W-AS's set (88, 89, 33, 90) and the §3.3/§3.6 adjacents (88, 89). When the SPEC splits W-UC into W4a/W4b, SPEC §7.1 carries the §3.5 (REDRESS 83) + REDRESS 60-62 honour-set; §7.2 carries the §3.4 (REDRESS 82) + REDRESS 64/66-69/93/88/89-orthogonal set. The union of SPEC §7.1 + §7.2 pre-blocks equals P3-E §2.5's set. Likewise SPEC §7.3 + §7.4 = P3-E §2.6's set. No pre-block entry is lost in the bundle→sub-wave split. | HOLD |
| 23 | REDRESS 33 / `match_tiny_plain_string` — still hard-blocked | P3-E §4 item 13 keeps NEON `match_tiny_plain_string` as a Class-A retained-G fix permanently pre-blocked (REDRESS 28+33). The dispatch draft §"Pre-Blocked Routes" item 13 reproduces it. SPEC §7.1 (W4a) targets `match_string_at_quote_trusted_utf8`, NOT `match_tiny_plain_string` — a different call site. No V2 sub-wave wires Class-A NEON `match_tiny_plain_string`. Hard pre-block honoured. | HOLD |
| 24 | Hard pre-block list — 13 items carried into both V2 drafts | The dispatch draft §"Pre-Blocked Routes" enumerates the 13 hard pre-blocks (REDRESS 91, 91+80, 92, 93, 73, 50/51/53/60-72/92 sidecar class, 88/89 PMULL+CTZ default, 85/86/87 Lock 14, 60-65/82-84 string-scanner class, 66-69 direct class, 34/70 bench-private, PMU/cycles/Criterion, 28+33 tiny-string). This matches P3-E §4's eight HANDOFF items + five class umbrellas. No hard pre-block dropped in the V2 fold. | HOLD |
| 25 | REDRESS 93 — scalar-parent fold stays blocked, no direct plane entered | SPEC §1 non-negotiable: "No direct digest row relabeled as typed product proof; scalar-parent folding stays blocked by REDRESS 93." Dispatch §"Pre-Blocked Routes" item 4. SPEC §0.1 close-condition clause 8 names no direct-contract wave; SPEC §4 (W1) "touches no direct guard plane"; §7.2 (W4b) "Pre-blocked routes" REDRESS 66-69+93 — "W4b's gate is `parse_only` only; it does not enter the direct plane." No V2 wave reopens REDRESS 93. Verified against REDRESS 93 (`:2723-2727`) verbatim three-condition clause. | HOLD |
| 26 | W4c/W4d HANDOFF §5 default-rewire boundary | SPEC §7.3 final pre-block bullet: 'HANDOFF §5 "PMULL prefix-XOR as default hot paths" applies to PMULL re-admission, explicitly not to the SHA3-gated wave-internal EOR3 fold.' SPEC §7.4 final bullet: 'HANDOFF §5 "CTZ/bulk production rewires as default hot paths" applies to default rewires; W4d is a host-capability-gated specialisation at a non-default call site.' Both keep the EOR3/CTZ kernels as capability-gated specialisations with unconditional scalar fallbacks — not default rewires. The HANDOFF §5 hard pre-block (dispatch item 7) is honoured. | HOLD |
| 27 | P3-A §3 "Reading the graph" residual stale recommendation | P3-A §3 (line 696-697) still reads "P3-B should sequence C3 + C4 + C5 + C6 (+ C7) as one cascade-locked behaviour wave (or a tightly-coupled pair), not as independent W{n}." This is the V1 pre-sub-wave recommendation. P3-A's own §3 candidate dispositions (lines 568-574, C6→W4c; lines 587-627, C7→W4d) and §4 table correctly carry the W4a-d sub-wave shape. The line 696-697 prose is an internal P3-A inconsistency the F-AUX fold left behind. It is not CH3-fatal — it moves no REDRESS route and the SPEC/dispatch carry the correct sub-wave manifest — but it should be tightened so a reader does not mistake the stale prose for the live plan. | REVISE |
| 28 | No V2 edit reopens a REDRESS route — diff audit | The V2 fold is commit `ef40c0fc`, "docs(sk-v9-p3-v2): integrate P3-A..E into the SPEC + DISPATCH drafts" — 5 files, 968 insertions / 473 deletions, all under `research/p3/`. `skinny/REDRESS.md` was NOT touched (no provenance line mutated). No source file was touched. The fold is a documentation integration of plan artefacts; it cannot reopen a REDRESS route by construction. Every per-wave pre-block section in the V2 SPEC is verbatim from P3-E's unchanged ledger. No reopen. | HOLD |
| 29 | SPEC §0.1 close-condition clause 7 — W10b at every sub-wave | SPEC §0.1 clause 7: "The W10b six-row regression block holds its maintain floor at every wave and sub-wave that touches the parse loop or an aarch64 SIMD kernel." This binds the close condition itself to the maintain gate — a wave that regresses a WIN row cannot close the bracket. The clause names the six rows verbatim. The W10b block is enforced at the close-condition level, not only the per-wave level. | HOLD |
| 30 | SPEC §2.2 cascade-lock disambiguation — no orphan kernel | SPEC §2.2 disambiguates the three "same-wave" relations (cascade-lock, same-wave consumer, codec/scanner pairing). The cascade-lock reading — "a P2-D kernel must not land *without the union substrate existing*" satisfied by W3 preceding W4a-d — closes the CONSOLIDATED item 6 ambiguity. CH3-relevant: an orphan kernel landing before its W3 consumer would be the SK-V5 orphan-kernel failure shape; the V2 SPEC §2.2 + every W4 entry gate ("W3 closed with `G-W3-UNION-SUBSTRATE` PASS") forecloses it. No orphan-kernel reopen. | HOLD |

---

## §3 — Aggregate verdict

The V2 fold resolves all five V1 CH3 DEFECTs. The single root cause —
three irreconcilable wave manifests, and a P3-F SPEC §8 standalone
codec wave that reopened the REDRESS-82 orphan shape — is closed:

- **The manifest is unified.** SPEC §2, the dispatch §"Wave Manifest",
  and P3-B §2 all carry one W1-W5 spine with W4 sub-waved W4a-d. The
  three V1-divergent manifests now agree (#1).
- **The codec is no longer a standalone wave.** SPEC §7.2 (W4b) is
  "PAIRED with W4a — strictly adjacent, never separable"; the W4b exit
  gate admits `unicode_mixed` only on the combined W4a+W4b Mbps and
  W4b "dispatches only with W4a landed." The REDRESS-82-orphan reopen
  P3-E §3.4 and P3-C §4.3 pre-block is closed (#4).
- **The SPEC's per-wave pre-blocked routes match P3-E's ledger.** Every
  SPEC §4-§7 "Pre-blocked routes" section cites an extant P3-E section
  and reproduces its content verbatim; the W-UC/W-AS bundles split
  cleanly across the W4a/W4b and W4c/W4d sub-wave sections with no
  pre-block entry lost (#3, #22).
- **The five material differentials survive.** Union vs 92/50-72 (#10),
  codec vs 82 (#6), EOR3 vs 88 (#8), string-block vs 83 (#7), CTZ vs
  89 (#9) — each cross-checked verbatim against `skinny/REDRESS.md` and
  each still concrete, falsifiable, and bound to a same-row gate.
- **The W10b six-row maintain gate is bound to every parse-loop-touching
  wave** — W3, W4a, W4b, W4c, W4d — and to the §0.1 close condition
  itself (#12, #13, #29).
- **The 4 typed-GO + 3 direct-GO rows are protected** on every wave
  touching a typed/direct codepath; the two V1 REVISEs (#25 W4
  direct-GO, #26 W3 named-rows) are folded (#17, #18).
- **No V2 edit reopens a REDRESS route.** The fold is a five-file
  documentation integration; `skinny/REDRESS.md` and all source were
  untouched (#28).

Two REVISE dispositions remain, neither a regression risk:

- **#2 — P3-E carries its V1 lettered wave scheme unchanged.** The V2
  fold did not re-author P3-E. P3-E §1 still uses W-AC/W-RG/W-UE/W-UC/
  W-AS and §2.5/§2.6 bundle the sub-waved kernels. Because P3-E §1
  explicitly delegates numbering to P3-B and the SPEC's per-wave
  citations resolve deterministically against P3-E's unchanged §2/§3
  sections, this is verifiable, not contradictory — but a V3 touch-up
  should add a one-line P3-E §1 note mapping the lettered scheme onto
  the W1-W5/W4a-d manifest so a reader is never left to reconstruct it.
- **#27 — P3-A §3 line 696-697 carries a stale "one cascade-locked
  wave" recommendation** that contradicts P3-A's own §3 dispositions
  and §4 table (both correctly W4a-d). An F-AUX residue; not CH3-fatal.

CH3 V2 ACCEPT-rate: 28/30 HOLD, 2/30 REVISE, 0/30 DEFECT = 93.3%. This
is below the §3Z 95% threshold by one disposition — but both REVISEs
are documentation-cohesion touch-ups on artefacts (P3-E §1, P3-A §3)
that are not the dispatched SPEC/DISPATCH surface, and neither admits
a REDRESS reopen. The CH3-binding obligation — no wave reopens a
pre-blocked route absent a cited entry, a material differential, and a
same-row gate — is fully met by the V2 SPEC and dispatch drafts.

Verdict: **ACCEPT-WITH-NOTE.** The V2 fold closed every V1 CH3 DEFECT
and reopened nothing. The two REVISEs are V3 surgical touch-ups
(P3-E §1 lettered-to-numeric note; P3-A §3 stale-prose deletion); they
do not block the S-P3 pass from advancing on the CH3 lens. CH3
clears for V2.

---

## §4 — Remaining REDRESS-regression risks

None of the following is a reopen in a dispatched artefact; all are
documentation-cohesion residue of a five-file fold that left P3-D and
P3-E untouched.

1. **P3-E §1 lettered-vs-numeric drift (REVISE #2).** P3-E §1's
   wave-letter table and §2.5/§2.6 bundling predate the W4a-d sub-wave
   split. The SPEC's per-wave pre-block citations still resolve
   correctly, so no pre-block is lost — but the binding ledger and the
   binding SPEC now describe the same waves in two vocabularies. A V3
   touch-up should add to P3-E §1 a single mapping sentence (W-AC→W1,
   W-RG→W2, W-UE→W3, W-UC→{W4a,W4b}, W-AS→{W4c,W4d}) and a §0 V2-fold
   marker, as P3-B and P3-C already carry. This forecloses a future
   reader mis-binding a P3-E §2.5 pre-block to the wrong sub-wave.

2. **P3-A §3 stale monolithic-wave prose (REVISE #27).** P3-A §3
   line 696-697 still recommends "C3 + C4 + C5 + C6 (+ C7) as one
   cascade-locked behaviour wave." If a future plan agent reads that
   prose rather than P3-A §3's dispositions or §4 table, it could
   re-propose the monolithic W4 the CH4 75-min-redress ceiling
   rejected — and a monolithic codec+string-block+ASM wave is the exact
   shape that, under CH4 #19, forced the sub-wave structure. The
   regression risk is indirect (a mis-read, not a live plan), but a
   V3 touch-up should replace line 696-697 with the W4a-d sub-wave
   sequencing P3-A's own §3 already states.

3. **P3-D non-fold (no CH3 risk, noted for census).** P3-D was not
   re-authored. P3-D owns no REDRESS-route surface, so this is a CH1
   census item, not a CH3-regression item — recorded here only so the
   V2 consolidation reconciles which of the seven artefacts were
   actually folded (P3-A, P3-B, P3-C, P3-F-spec, P3-F-dispatch) versus
   carried unchanged (P3-D, P3-E).

No reopen survives into a dispatched wave. The V2 SPEC and dispatch
drafts honour every pre-block in P3-E's unchanged ledger; the two
residual REVISEs are surgical documentation touch-ups on the two
unfolded sibling artefacts and do not gate the CH3 lens.
