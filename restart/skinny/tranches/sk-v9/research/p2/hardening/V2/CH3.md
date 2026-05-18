# SK-V9 S-P2 V2 — CH3 REGRESSION (REDRESS-Reopen Audit, V2 verify)

Pass: S-P2 Research. Cycle: V2. Lens: CH3 (`restart/prompts/ORCHESTRATOR.md`
§3W). Cohort: S-P2 V2 fold, six artefacts (`skv9-p2-A` … `skv9-p2-F`,
all `mtime 2026-05-18 17:42–17:47`, newer than `V1/CH3.md`). Fold authority:
`HARDENING-S-P2-V1-CONSOLIDATED.md` F3 (P2-F synthesis overreach) + F6 (CH3
REVISEs). REDRESS authority surface: `skinny/REDRESS.md` entries 28, 33,
50–55, 60–72, 82, 83, 84, 88, 89, 90, 92. HANDOFF §5 pre-block list.

V1 CH3 verdict: **67.4% ACCEPT (29/43)** — 3 REJECT (all P2-F: F-2, F-3,
F-6), 11 REVISE. This V2 pass verifies whether the F3 + F6 fold resolves
all 14 named V2-fold items (`V1/CH3.md` §4 table) without opening new
REDRESS-reopen routes.

## §1 — V1-REJECT + V1-REVISE resolution

### §1.1 — The 3 V1 REJECTs (all P2-F)

**V1 REJECT F-2 — P2-F §7.2 DirectBuild emit-site clause.** V1 found §7.2
wired the fused `\uXXXX` codec at "the DirectBuild field-fact emit site
for unicode-bearing typed structs" — REDRESS 66–69 territory (DirectBuild
semantic string field facts, parser-owned decoded scratch, byte-output
`unescape_json_string`). **RESOLVED.** V2 §7.2 reads: "The
`parse-that-regex` unescape hot path only, per P2-E §4 — the codec wires
into the existing `unescape_four_unicode_escapes` consumer site. It does
*not* extend to a DirectBuild field-fact emit site: that expansion is
REDRESS 66-69 territory … and is not opened by this synthesis." The
DirectBuild emit-site clause is stripped verbatim; the §0 fold note
records the strip. P2-E §4.3 disclaimer (no JSON-specific allocator
strategy in the codec path) is now consistent with P2-F. **No residual
REDRESS 66–69 exposure.**

**V1 REJECT F-3 — P2-F §7.3 admission 1 (REDRESS 33) + admission 2
(REDRESS 89).** V1 found admission 1 (`match_tiny_plain_string` cost-fact
+ NEON wiring at the field-name match-arm chain) was the REDRESS 33
rejected shape verbatim, and admission 2 broadened P2-D §4.4's narrow
string-mask consumer into "retained-parse next-structural seek" — REDRESS
89 bulk-consumer territory. **RESOLVED.** V2 §7.3 is retitled "P2-D ASM
kernel opportunities (admission shapes deferred)" and authors *no*
admission shapes: "A Class A `match_tiny_plain_string` cost-fact wired at
the field-name match arm chain is the REDRESS 33 rejected shape, and
broadening the P2-D §4.4 narrow string-mask consumer into a bulk consumer
is REDRESS 89 territory. **Admission shapes for P2-D ASM kernels are
authored by S-P3 with explicit REDRESS material-differential gates.**"
Both admission 1 and admission 2 are deleted, not reworded — the §0 fold
note confirms the strip. **No residual REDRESS 33/89 exposure.**

**V1 REJECT F-6 — P2-F §3 "Room to widen the lead."** V1 found §3
proposed per-field NEON tiny-string equality at the DirectBuild dispatch
+ direct `\uXXXX` fusion into the field-fact emit, silently reopening
REDRESS 33 + 66–69 and risking the 4 typed-GO rows. **RESOLVED.** V2 §3
final paragraph ("The lead is REDRESS-bound territory") names the
structural lead as a *finding only* and states explicitly: "wiring Class
A NEON `match_tiny_plain_string` at the DirectBuild field-name match arm
chain is the REDRESS 33 rejected shape, and fusing `\uXXXX` decode into
the field-fact emit is REDRESS 66-69 territory … This synthesis names the
structural lead as a finding only; the question of whether — and under
what fresh-evidence framing — the lead can be widened belongs to S-P3
with explicit material-differential gates." The proposals are walked back
to a finding; no widening intervention is authored. **No residual
typed-GO regression route from P2-F.**

### §1.2 — The 11 V1 REVISEs (V1/CH3.md §4 folds 1–14, excl. the 3
REJECT-origin items 9/10/13)

| V1 fold | Origin | Required action | V2 status |
|---:|---|---|---|
| 1 | A-2 | Tighten `StructuralIndex` falsifier (lifetime ≤ one `parse`, never `Send`, never named publicly) | **FOLDED.** P2-A §2 lines 194–205: "`StructuralIndex` lifetime is explicit and bounded … never `Send` across; it carries no `'static` lifetime and is never named on a [tape]." Grep falsifier `rg -n 'StructuralIndex' skinny/crates/runtime/src/` added. |
| 2 | A-4 | Prove `OffsetTape + classes` is the same `BackendShape` variant, not a sixth, via code-level argument against `derive_backend_shape` | **FOLDED.** P2-A §2 lines 234–253: the five-variant enum is "unchanged at the variant level"; the class column is "a representation refinement of the `OffsetTape` variant only"; falsifier `rg -n 'enum BackendShape' bbnf-codegen/src/` returns one match before and after, diff byte-identical. Code-level argument present. |
| 3 | A-8 | Class column opt-in per-grammar OR explicit acknowledgement all five tapes grow | **FOLDED.** P2-A §2 lines 255–267: storage cost materialises "only for grammars whose `derive_backend_shape` assigns `OffsetTape`"; generic `runtime/src/tape/` crate does NOT allocate the column; per-grammar codegen template constructs it; grammars opting out "emit a `parser.rs` that never references `classes`." Per-grammar opt-in is the chosen resolution. |
| 4 | B-5 | `rg` check binding: no `_witness` directory contains `scan.rs`/`parser.rs`/`generated.rs`/`view.rs` | **FOLDED** (P2-B §0 + body — see §2 B-row dispositions). |
| 5 | D-1 | No-regression gate on six W10b-rejection rows for the EOR3 candidate | **PARTIALLY FOLDED — see §2 D-1.** The §5.3.1 EOR3 slice is now Lock-16 host-cap-gated with scalar fallback; the §4.4 CSSC CTZ slice carries the explicit six-row gate. The EOR3 slice itself does *not* name the six-row block in its own falsification gate text. |
| 6 | D-4 | Bind P2-D §3 codec admission to "consumer is the union-substrate, not the parser-owned materialiser" | **FOLDED.** P2-D §3.5 "Same-wave consumer binding (CH3 / no-orphan)": codec broadening "blocks on P2-A landing in the same wave OR fails CH5"; absent P2-A "the codec broadening ships as a primitive without its production consumer — a REDRESS-82-style orphan — and must be held back." |
| 7 | D-10 | EOR3 fold honours HANDOFF §5 procedure (fresh evidence, owner paths, six-row gate, REDRESS 88+89 citation, CH3 acceptance) | **PARTIALLY FOLDED — see §2 D-10.** Owner path named (`bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs`), REDRESS 88 cited with three-axis differential, Lock-16 gate added, scalar fallback unconditional. Six-row no-regression gate is *not* explicitly named for the EOR3 slice. |
| 8 | E-1 | Tighten consumer differential; require `unescape_uxxxx_neon` checkasm pre-wave; flag 94.5%-of-threshold as conditional-admission rule | **FOLDED.** P2-E §0: §5 tightened (REDRESS 82 = parser-owned per-quartet classifier vs P2-E full hex-decoder primitive class); §6.2.1 (P2-D) assigns `checkasm_unescape_uxxxx.rs` ownership to the broadening wave; §6.4 admission rule states 0.70 slack before projection; rederived verdicts are honest (zero of four rows admit on codec alone). |
| 11 | F-3 adm.2 | Align with P2-D §4.4 narrow scope OR carry REDRESS 89 differential | **FOLDED** via F-3 strip (§1.1). P2-D §4.4 retains the narrow string-mask scope independently. |
| 12 | F-4 | Rewrite §7.4 to reflect actual REDRESS-adjacency | **FOLDED.** §7.4 reframed as a pure dependency graph (`I ← P2-A ← P2-B`; `II ← P2-E`, secondary `II ← P2-A`; `III ← P2-D ← P2-A`); no cost set, no wave sequence, no cumulative projection. |
| 14 | F-7 | §5.2 "pattern not yet used" must cite REDRESS 33, state cost-fact differential, name same-row guards | **PARTIALLY FOLDED — see §2 F-7.** §5.2 is unchanged prose; §7.3 carries the REDRESS 33 deferral. The §5.2 architecture-lesson framing still says "Pattern not yet used … Expected impact: lifts twitter, update_center, apache_builds, github_events" without a REDRESS 33 citation in §5.2 itself. |

## §2 — V2 dispositions (≥25)

Disposition columns: *Item* / *V1 verdict* / *V2 disposition* / *V2 verdict*.
V2 verdicts: **CONFIRMED** (V1 ACCEPT holds), **RESOLVED** (V1 REVISE/REJECT
folded), **RESIDUAL-REVISE** (fold incomplete, non-blocking), **REGRESSION**
(fold opened a new route).

### §2.1 — P2-A

| Item | V1 | V2 disposition | V2 verdict |
|---|---|---|---|
| A-1 | ACCEPT | Class column co-emitted at the same emit site (now `emit_event_offset`); unchanged. | **CONFIRMED.** |
| A-2 | REVISE | `StructuralIndex` falsifier tightened: lifetime ≤ one `parse(input)`, never `Send`, never `'static`, never named publicly; `rg` falsifier added (§2 lines 194–205). REDRESS 51 differential is now testable. | **RESOLVED.** |
| A-3 | ACCEPT | `consume_structural` deleted (§5.3 slice A.3 removal half, −50 regen LOC); no second mask. | **CONFIRMED.** |
| A-4 | REVISE | `BackendShape` proven unchanged at variant level — class column is an interior representation refinement of `OffsetTape`'s runtime struct; `derive_backend_shape` decision tree gains no branch; byte-identical `enum` line. HANDOFF §5 "no new `BackendShape`" honoured. | **RESOLVED.** |
| A-5 | ACCEPT | Class column does not invoke/modify the Class A string-scanner pair. | **CONFIRMED.** |
| A-6 | ACCEPT | Unicode escape codec delegated to P2-E. | **CONFIRMED.** |
| A-7 | ACCEPT | SIMD producer unchanged at Layer-1 vocabulary. | **CONFIRMED.** |
| A-8 | REVISE | Class column made per-grammar opt-in: only `OffsetTape`-routed grammars pay `+1 byte/cursor`; generic `tape/` crate does not allocate; opting-out grammars emit `parser.rs` with no `classes` reference. §5 per-slice LOC + revert added. | **RESOLVED.** |
| A-9 | ACCEPT | Consumer is `JsonNodeKind::at_cursor` (production view, slice A.4), not `tape_vs_tape`. | **CONFIRMED.** |

### §2.2 — P2-B

| Item | V1 | V2 disposition | V2 verdict |
|---|---|---|---|
| B-1 | ACCEPT | Compile-time `EventGrammar` trait + `ValueRef` proof, no production consumer, `#[cfg(any(test, feature = "proof"))]` gated. | **CONFIRMED.** |
| B-2 | ACCEPT | REDRESS 71 (SinkOnly direct lowerer) orthogonal to the `OffsetTape` retained class/event contract. | **CONFIRMED.** |
| B-3 | ACCEPT | Proof is the response to REDRESS 92's "define + prove, reopen out of scope" routing sentence. | **CONFIRMED.** |
| B-4 | ACCEPT | Tier B substrate-API expansion owned by SC-3 §5.2, not the proof. | **CONFIRMED.** |
| B-5 | REVISE | `_witness` directory binding: P2-B §0 names `AnyGrammar` empty-grammar declaration + the `_witness`-directory rule. The fold adds the binding that `runtime/src/grammars/<G>_witness/` exists only for `EventGrammar` proof witnesses; the `rg` admission check (no `scan.rs`/`parser.rs`/`generated.rs`/`view.rs` in any `_witness` dir) is named as a pre-commit + per-wave gate. | **RESOLVED.** |
| B-6 | ACCEPT | Same-wave-consumer rule binds substrates, not compile-only contracts. | **CONFIRMED.** |

### §2.3 — P2-C

| Item | V1 | V2 disposition | V2 verdict |
|---|---|---|---|
| C-1 | ACCEPT | REDRESS 91 source/product → measured admission; row-table + gate ownership only. V2 §0 footer adds per-slice LOC break-out (F4). | **CONFIRMED.** |
| C-2 | ACCEPT | No retained-parse surface; existing generated DirectBuild typed path untouched. | **CONFIRMED.** |
| C-3 | ACCEPT | Not a structural-heavy parse wave; REDRESS 92 not reopened. | **CONFIRMED.** |
| C-4 | ACCEPT | Direct rows hold SK-V9-open verdicts; REDRESS 93 not reopened. | **CONFIRMED.** |
| C-5 | ACCEPT | Track 2/oracle = `serde_json`; strict parity across {generated, serde, sonic}. | **CONFIRMED.** |
| C-6 | ACCEPT | Four typed-GO rows explicitly guarded ("no regression below sonic × 1.10⁻¹"); §4.3 falsifiability gate. | **CONFIRMED — load-bearing for §3 (typed-GO protection).** |

### §2.4 — P2-D

| Item | V1 | V2 disposition | V2 verdict |
|---|---|---|---|
| D-1 | REVISE | §5.3.1 EOR3 fold reframed: SHA3 `veor3q_u8` is a Lock-16 host-cap-gated (`FEAT_SHA3`) *capability-conditional specialisation* of the scalar ladder, NOT a new default body; the scalar shift-XOR ladder remains the unconditional fallback. This is materially different from the V1 framing (a default-hot-path rewire). However, the §5.3.1 falsification posture does **not** name the W10b six-row block (`canada`, `citm_catalog`, `instruments`, `marine_ik`, `mesh`, `numbers`) in its own slice gate — the six-row gate is named only in §4.4 (CSSC CTZ). | **RESOLVED-with-RESIDUAL.** The Lock-16-gated-specialisation reframe is the material differential V1 demanded and it removes the "default rewire" objection; but V1 fold #5/#7 asked specifically for the six-row no-regression gate on the EOR3 candidate, and §5.3.1 does not carry it. Non-blocking (the slice blocks on P2-A and S-P3 authors the final cost-set), but should be named in S-P3 dispatch. |
| D-2 | ACCEPT | CSSC CTZ at the string-mask first-set extract; §4.4 retains the narrow scope + names the six-row WIN-block falsification gate as a hard blocking precondition. | **CONFIRMED — and strengthened.** |
| D-3 | ACCEPT | 32-byte string-block widening at the full-scan path (`match_string_at_quote_trusted_utf8`), not the tiny probe. §4.0 adds the grammar-neutral `scan_string_special_block_32` Lock-14 framing. | **CONFIRMED.** |
| D-4 | REVISE | §3.5 codec broadening now bound to the union-substrate consumer: "blocks on P2-A landing in the same wave OR fails CH5"; absent P2-A it is a "REDRESS-82-style orphan … held back." The differential is honestly reframed (see §3). | **RESOLVED.** |
| D-5 | ACCEPT | §5 structural-bitmap chain consumer is the union substrate, not the parser hot loop. | **CONFIRMED.** |
| D-6 | ACCEPT | §5 union substrate, no sidecar; tracks A-1/A-4/A-8 (all RESOLVED). | **CONFIRMED.** |
| D-7 | ACCEPT | §4 widens producer per-block, not the retained scanner. | **CONFIRMED.** |
| D-8 | ACCEPT | §3 x4 batched, stateless, not a retained validator. | **CONFIRMED.** |
| D-9 | ACCEPT | Direct source-hook field-folding not proposed; delegated to REDRESS 93 tranche. | **CONFIRMED.** |
| D-10 | REVISE | §5.3.1 EOR3 owner path named (`bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs`); REDRESS 88 cited with a three-axis differential (different intrinsic / latency / primitive shape); Lock-16 `FEAT_SHA3` gate + unconditional scalar fallback. The HANDOFF §5 "default hot path rewire" objection is dissolved because the EOR3 body is *not* the default — it is a host-cap-gated path. **However**, the explicit no-regression gate against the six W10b rows is still absent from §5.3.1. | **RESOLVED-with-RESIDUAL** (same residual as D-1; one defect, two rows). |

### §2.5 — P2-E

| Item | V1 | V2 disposition | V2 verdict |
|---|---|---|---|
| E-1 | REVISE | §5 consumer differential tightened: REDRESS 82 was a parser-owned per-quartet *classifier*; P2-E is a *primitive class — a full hex-decoder*. §6.1/§6.2 PMU rederived from the actual TSV (the V1 baseline was fabricated — load-bearing F2 fold). §6.2.1 assigns the `checkasm_unescape_uxxxx.rs` ownership to the broadening wave (pre-wave, not concurrent). §6.4 conditional-admission rule states 0.70 slack before projection. Rederived verdicts are honest: unicode_escapes NEAR-FAIL 94.5%, y_string_unicode NEAR-FAIL 94.8%, unicode_mixed FAIL 63.7% — zero of four rows admit on the codec alone. | **RESOLVED.** The honest "zero rows admit on codec alone" posture is the correct anti-paper-close outcome; the 94.5%-of-threshold risk is now an explicit conditional-admission rule, not a buried optimism. |
| E-2 | ACCEPT | Codec on parse_only + retained-tape plane; no direct-string materialiser touch. | **CONFIRMED.** |
| E-3 | ACCEPT | Codegen template, const-generic specialisation, five bindings, shared body — clean Lock-14. §4.4 dispositions TOML `\u`/`\U` as compile-validation-only (no production consumer this wave). | **CONFIRMED.** |
| E-4 | ACCEPT | Codec downstream of the string scanner; no boundary work. | **CONFIRMED.** |
| E-5 | ACCEPT | Stateless function call; no substrate impact. | **CONFIRMED.** |

### §2.6 — P2-F

| Item | V1 | V2 disposition | V2 verdict |
|---|---|---|---|
| F-1 | REVISE | §7.1 Intervention I inherits P2-A's now-RESOLVED dispositions (A-2/A-4/A-8). §7.4 carries no LOC budget (the V1 +150 LOC figure that conflicted with P2-A §5's ~265 is removed — §7.4 is a pure dependency graph). | **RESOLVED.** |
| F-2 | **REJECT** | §7.2 DirectBuild field-fact emit-site clause stripped verbatim; codec wires at the `parse-that-regex` unescape hot path only, aligned with P2-E §4.3. §0 fold note records the strip. | **RESOLVED.** |
| F-3 | **REJECT** | §7.3 retitled "admission shapes deferred"; admission 1 (REDRESS 33 `match_tiny_plain_string`) and admission 2 (REDRESS 89 bulk consumer) both deleted; all P2-D ASM admission shapes deferred to S-P3 under explicit REDRESS gates. | **RESOLVED.** |
| F-4 | **REJECT** | §7.4 reframed as an inter-report dependency graph (`I ← P2-A ← P2-B`; `II ← P2-E`, secondary `II ← P2-A`; `III ← P2-D ← P2-A`); no cost set, no wave sequence, no cumulative projection — explicitly deferred to S-P3 P3-B/P3-C. The V1 false synthesis claim "none re-opens a REDRESS-pre-blocked route" is gone. | **RESOLVED.** |
| F-5 | ACCEPT | asmjson non-anchored sidecar classification; §4 unchanged. | **CONFIRMED.** |
| F-6 | **REJECT** | §3 "Room to widen the lead" walked back to a finding; the final paragraph names DirectBuild Class A wiring as REDRESS 33 and `\uXXXX` field-fact fusion as REDRESS 66–69, and defers all widening to S-P3 with explicit material-differential gates. The 4 typed-GO rows are no longer threatened by any P2-F proposal. | **RESOLVED.** |
| F-7 | REJECT | The §5.2 architecture-lesson framing ("Pattern not yet used … Expected impact: lifts twitter, update_center, apache_builds, github_events") is **unchanged prose** — it still does not cite REDRESS 33 inline. The REDRESS-bound deferral now lives in §7.3 and §3, which is structurally sufficient (§5.2 is a competitor-architecture lesson, not an intervention proposal, and authors no admission). But V1 fold #14 asked for the citation *in §5.2*. | **RESOLVED-with-RESIDUAL.** §5.2 authors no intervention and §7.3 + §3 carry the REDRESS 33 deferral, so the regression route is closed; the missing inline citation in §5.2 is a cosmetic completeness gap, non-blocking. |

### §2.7 — V2-D's reframed REDRESS 82 differential (cross-cut)

V1 CH6-D-1 (F1) found P2-D §2.1's claim that `unescape_uxxxx_x4_neon` is
"neither wired into the parse-that-regex hot path" was factually wrong —
the kernel IS wired at `parse-that-regex/src/lib.rs:402` inside
`unescape_four_unicode_escapes`. The V2 fold corrects this: P2-D §3.5 +
§7 REDRESS-82 row now state "The differential is NOT 'wire the kernel' —
`unescape_uxxxx_x4_neon` is already consumed at `…lib.rs:402`." The V2
differential is three-axis: (1) **broaden** the opportunistic-x4-only
batcher to all-quartet handling — thread the per-quartet
`unescape_uxxxx_neon` into the single-quartet / mixed-escape /
surrogate-split fall-through (which currently drops to the scalar
`decode_unicode_escape`); (2) **rebind** consumer cardinality from the
parser-owned `unescape_string` materialiser (REDRESS 82's rejected shape)
to the P2-A union-substrate tape-cell projection; (3) add the
direct-route falsification gate on `unicode_escapes/direct`,
`y_string_unicode/direct`, `unicode_mixed/direct`. **Verdict: HONEST AND
MATERIALLY DIFFERENT.** The reframe is not a relabel — it changes axis (d)
the measured shape (opportunistic-x4 → all-quartet engagement) and axis
(b) the consumer (parser-owned materialiser → union-substrate tape-cell
projection). The V2 text is candid that, absent P2-A, broadening alone
"only reduces fall-through traffic in the *parser-owned* helper, which is
the shape REDRESS 82 rejected" — i.e. it explicitly states the
differential is lost without P2-A and binds the slice to P2-A's landing
(no-orphan). This is the correct honest posture: the differential is real
*conditional on P2-A*, and the report says so. **CONFIRMED honest.**

### §2.8 — Typed-GO + direct-GO row protection (cross-cut)

The 4 typed-GO rows (`twitter`, `update_center`, `mesh`, `marine_ik` /
`real_typed_struct`) and 3 direct-GO rows (`citm_catalog`, `marine_ik`,
`unicode_basic` / `direct_to_struct`) are now explicitly protected:

- **P2-C §4.3** retains the verbatim guard: "Existing four typed GO rows
  hold their `A / GO` outcome … no regression below sonic × 1.10⁻¹." This
  is a binding falsifiability gate on the only P2 report that touches the
  typed row-table.
- **P2-F §3** no longer proposes any typed-row substrate change — the
  "Room to widen the lead" intervention is walked back to a finding, so
  the V1 silent-regression route from §3 is closed.
- **P2-D §3.5** adds the direct-route no-regression CI guard on
  `unicode_escapes/direct`, `y_string_unicode/direct`,
  `unicode_mixed/direct` (a 20–40 LOC harness slice) — this protects the
  direct plane the codec broadening's falsification gate covers.

**Verdict: the 4 typed-GO + 3 direct-GO rows are explicitly protected.**
P2-C §4.3 is the typed-GO guard; P2-F §3 walk-back removes the only V1
threat; P2-D §3.5 adds the direct-plane guard. No P2 V2 report puts an
admitted row at silent-regression risk.

## §3 — Aggregate verdict

V2 cohort CH3 REGRESSION-disposition summary:

| Report | CONFIRMED | RESOLVED | RESID-REVISE | REGRESSION | Total |
|---|---:|---:|---:|---:|---:|
| P2-A | 6 | 3 | 0 | 0 | 9 |
| P2-B | 5 | 1 | 0 | 0 | 6 |
| P2-C | 6 | 0 | 0 | 0 | 6 |
| P2-D | 7 | 1 | 2 | 0 | 10 |
| P2-E | 4 | 1 | 0 | 0 | 5 |
| P2-F | 2 | 4 | 1 | 0 | 7 |
| **Total** | **30** | **10** | **3** | **0** | **43** |

CONFIRMED + RESOLVED (clean dispositions): **40/43 = 93.0%**. The 3
RESIDUAL-REVISE items (D-1, D-10 — the same EOR3 six-row-gate defect
counted across two rows; F-7 — missing inline REDRESS 33 citation in
§5.2) are non-blocking: D-1/D-10's EOR3 slice blocks on P2-A and defers
its final cost-set to S-P3, and the Lock-16 host-cap-gated-specialisation
reframe already dissolves the "default rewire" REDRESS 88 objection; F-7's
§5.2 authors no intervention and the REDRESS 33 deferral is carried by
§7.3 + §3. **Zero REGRESSION dispositions** — the V2 fold opened no new
REDRESS-reopen route.

The 3 V1 REJECTs are all RESOLVED: F-2 (DirectBuild emit-site stripped),
F-3 (both admissions deleted), F-6 (room-to-widen walked back). The 11 V1
REVISEs: 8 fully FOLDED, 3 PARTIALLY FOLDED (D-1/D-10/F-7, the
RESIDUAL-REVISE items).

**Verdict against the §3Z convergence criterion.** V1 was 67.4%; V2 is
**93.0% clean (40/43)**. If the 3 RESIDUAL-REVISE items are scored
strictly as non-ACCEPT, V2 is below the 95% bar by 2.0 points; if scored
as ACCEPT (both residuals are completeness gaps with the regression route
already closed, not live reopens), V2 is at 95.3%. The honest CH3 reading:
**V2 clears the REJECT class entirely and clears the substantive
REGRESSION bar — no admitted row is at risk and no route is reopened —
but three cosmetic-completeness folds remain.** Per §3Z (≥95% ACCEPT for
two consecutive cycles), the prudent disposition is **REVISE — V3 fold the
3 residuals, then CH3 re-runs clean.** The residuals are surgical
single-sentence additions (one six-row-gate sentence in P2-D §5.3.1; one
REDRESS 33 citation in P2-F §5.2); they do not require scope changes and
do not route the cohort back to research-grade rework. P2-A, P2-B, P2-C,
P2-E are each fully converged on CH3 at V2. P2-D and P2-F carry the 3
residuals.

## §4 — Remaining REDRESS-regression risks

| # | Origin | REDRESS entry | Risk | V3 action |
|---:|---|---|---|---|
| 1 | D-1 / D-10 | 88 + 89 + HANDOFF §5 | The §5.3.1 EOR3 slice is now Lock-16 host-cap-gated (correct), but its falsification posture does not name the W10b six-row block. The Lock-16 gate dissolves the "default rewire" objection — the EOR3 body is *not* the production default — so this is a **completeness** gap, not a live reopen. Still: the EOR3 vector ladder is on the structural-bitmap producer's hot path *when `FEAT_SHA3=1`*, and the W10b campaign proved prefix-XOR-hot-body changes regress the WIN block even when correctness-green. | V3: add one sentence to P2-D §5.3.1 — "the EOR3 candidate's S-P3 admission carries a no-regression gate on `canada`, `citm_catalog`, `instruments`, `marine_ik`, `mesh`, `numbers`" — mirroring §4.4's CSSC CTZ gate. |
| 2 | F-7 | 33 | P2-F §5.2 ("Cost-fact-gated NEON `vqtbl1q_u8` tiny-string equality … Pattern not yet used … Expected impact: lifts twitter, update_center, apache_builds, github_events") names a REDRESS-33-pre-blocked pattern as an architecture lesson without an inline citation. The regression route is closed (§5.2 authors no intervention; §7.3 + §3 carry the REDRESS 33 deferral), so this is a **cosmetic** gap. | V3: add a parenthetical to P2-F §5.2 — "(this exact wiring is the REDRESS 33 rejected shape; any S-P3 admission requires a material-differential gate per §7.3)". |
| 3 | D-4 / D-1 / D-2 / D-10 | 82 + 88 + 89 (no-orphan chain) | P2-D §3 (codec broadening), §4.4 (CSSC CTZ), §5.3.1 (EOR3), §5.4 (dead-SIMD-scanner wiring) all "block on P2-A landing in the same wave OR fail CH5." This is the correct no-orphan posture, but it creates a **cascade dependency**: if P2-A does *not* pass CH3+CH5 in the same wave, four P2-D slices lose their union-substrate consumer simultaneously and fall back to parser-owned shapes that are REDRESS-rejected. This is not a V2-fold defect — it is a structural risk S-P3 wave-sequencing must honour. | S-P3 P3-B: P2-A must land in the same wave as any P2-D consumer slice, or the P2-D slices are held. Already stated by P2-D §3.5/§4.4/§5.4; flagged here so S-P3 sequencing does not split the wave. |
| 4 | A-8 (residual watch) | Lock 1 cardinality + Lock 14 | P2-A's per-grammar opt-in resolves the storage-cost objection, but the class column is still a representation refinement on `OffsetTape` that *every* `OffsetTape`-routed grammar inherits (JSON / CSS L4 / Sheets / BBNF-self per §3's five-grammar enumeration). The non-JSON grammars have no SK-V9 production consumer for the column. This is a CH4-COST concern (carried separately); from CH3 it is **not** a regression — the column is opt-in and the substrate is grammar-neutral. No V3 action; noted for CH4 cross-check. | None (CH4 owns the storage-cost question). |

The cohort carries **no live REDRESS-reopen** at V2. The 3
RESIDUAL-REVISE items are completeness folds (risks 1 + 2); risk 3 is a
wave-sequencing constraint S-P3 already inherits from the report text;
risk 4 is a CH4 hand-off. V2 dispatch resolved every REJECT and every
substantive REVISE.

— end CH3 V2.
