# CHALLENGE CH4 — COST (cycle V4)

Lens: CH4 Cost. Per PASS-ALPHA §3 ("what is the LOC budget per intervention? Risk
classification? Wave alignment? Same-wave consumer present per intervention?") +
ORCHESTRATOR §3W lens set. Each candidate must carry: LOC budget + risk class +
wave alignment + same-wave consumer + scalar-ref + checkasm expectation.

Subject under bracket: CSS L4 typed parsing must BEAT lightningcss (the fair
full-CSSOM comparator) via the unified tape/layout/projection model + dav1d-style
aarch64 NEON hot leaves; preserve-rich-ast; no x86; fully generalized for SKINNY,
foldable into TOTALITY. aarch64 Apple M5 Max only.

Reviewed: `research/alpha/{alphaA..alphaE}.md`, `SYNTHESIS.md`, `HANDOFF.md`.
Note: there is no `alphaF.md`; the α-F contract draft IS `SYNTHESIS.md` + `HANDOFF.md`
at tranche root (PASS-ALPHA §2 row α-F, §6 tree — confirmed both present at root:
`SYNTHESIS.md` 40480 B, `HANDOFF.md` 15296 B). CH4 brackets the candidate cost
surface in alphaE (where LOC/risk/wave/consumer/scalar-ref/checkasm live) and the
receiver-cost surfaces in SYNTHESIS §0.1/§0.3/§0.5 + HANDOFF Next-Move.

This is cycle V4. The CH4 cost surface CONVERGED at V3: CH4-V3 returned 12 ACCEPT,
0 REVISE, 0 REJECT (CH4-V3.md:235-238), with the sole CH4-V2 REVISE (the fabricated
`resolve_builder_routes` symbol in C1's owner path) confirmed FOLDED and grep-clean,
and zero orphan REVISE carried into V3. This V4 review's job: (1) re-confirm the V3
cost surface holds at HEAD; (2) confirm the three V4-changelog sibling folds (CH1-a,
CH1-b, F1) are cost-neutral to every alphaE candidate; (3) re-bracket for any new
cost defect.

---

## Verification performed (every disposition is grounded; re-greped at V4 HEAD `1c5bd7a25`)

| Check | Result | Evidence |
|---|---|---|
| HEAD is `1c5bd7a25` (the bracket) | YES | `git rev-parse --short HEAD` = `1c5bd7a25` |
| `resolve_builder_routes` (V2 REVISE target) grep-clean | YES (struck) | `grep -rn resolve_builder_routes skinny/` empty |
| C1 owner-path seam: 7× `RequestFactsProfile` `RequestFacts` literals | YES | `grep -n 'RuntimeEmitterKind::RequestFacts' regen_css.rs` = exactly `:45,63,81,99,117,135,153` |
| C1 entry fn `regen_css` | YES | `regen_css.rs:164` = `pub(crate) fn regen_css(root: &Path) -> Result<()>` |
| `W5C_REQUEST_FACT_PROFILES` const + selection + 2 consumers (C0 retire target) | YES | `lib.rs:336` const; `:299` selected; `:567,:611` `for profile in …` consumers |
| `digit_mac` udot orphan + scalar-ref (C4a) | YES | `digit_mac.rs:5` `parse_4_digits`, `:12` dotprod branch, `:15` `#[cfg(not(target_feature="dotprod"))]` scalar twin, `:27` `parse_4_digits_dotprod`, `:40` `udot`, `:53` `dot4_i8` |
| i8mm grep-clean-absent (C4b NET-NEW claim) | YES (NONE) | `grep -rn 'is_aarch64_feature_detected!("i8mm")' skinny/crates/` empty |
| `checkasm_digit_mac` / `checkasm_i8mm` present today (C4a/C4b "test is the NEW artefact") | ABSENT | `bbnf-simd/tests/` carries `checkasm_{ascii_set_member_find_64, bitmap_next_set_bit, bitmap_prefix_xor_64, bulk_emit_positions_64, byte_class_from_eq_set_64, byte_class_from_table_64, eob_pad_clamp, escape_mask_64, parity, structural_terminator_64, utf8_block}` — NO digit_mac/i8mm test — confirms both are the NEW artefact |
| `select_classifier`:42 / `PrimitiveKernels`:50 / `OnceLock`:59 / `lo6_table_admissible`:101 / `classify_chunk`:19 (C2 reuse) | YES | `bbnf-simd/src/dispatch.rs:42,50,59,101,19`; `:21` `SelectedBackend::Scalar => crate::scalar::classify_chunk(...)` = C2 scalar-ref |
| `RuntimeEmitterKind::{CompiledLowering, RequestFacts}` selection (C0 route) | YES | `lib.rs:282` (CompiledLowering=JSON), `:291` (RequestFacts=CSS) — cited in SYNTHESIS §0.1:54-55 |
| fixture count = 148 (overfit fingerprint #2) | YES | `grep -c 'fn parse_' generated_real_typed.rs` = **148** |
| V4 sibling fold CH1-a (alphaA:138-148 reconciliation) landed | YES | alphaA:140-147 "Cross-artefact reconciliation note (V4 fold, binding) … 24 / lines 112-135 … the V2 '6' undercount is resolved across the cohort" |
| V4 sibling fold CH1-b (alphaC:225-231 grep-substring mislabel) landed | YES | alphaC:227-231 "25 substring matches, of which 24 are `^| css_l4/` table rows (112-135) and the 25th (:154) is a prose REDRESS-127 companion reference … there is NO admitted/distinct W6 typed CSS row" |
| V4 sibling fold F1 (alphaD:154 O5 css_l4.toml LOC relabel) landed | YES | alphaD O5 cell: "The skinny-greppable exit gate is grammar-derivation, NOT TOML-LOC count … the 594-line `css_l4.toml` LOC convergence is a TOTALITY metric (SK-V18 fold), INFORMATIONAL only, NOT an SK-V17 close/exit gate (SYNTHESIS §0.1)" |
| SYNTHESIS §0.3 seam-flip row carries the 7 literals + retire | YES | SYNTHESIS:169-170 names `regen_css.rs` "seven `RequestFactsProfile` literals that the seam-flip edits"; totality paths explicitly SK-V18 fold (`:170-174`) |
| HANDOFF Next-Move W2 seam-flip + same-wave rule | YES | HANDOFF:202-206 "(W2) tape activation + builder seam flip (CSS off `RuntimeEmitterKind::RequestFacts`, retire `W5C_REQUEST_FACT_PROFILES`)"; `:210-211` "Each primitive lands WITH its hot-path consumer in the same commit (no orphan kernels)" |
| HANDOFF `tape_activated` wrong-tree-cost guard | YES | HANDOFF:226-228 "proven by `PayloadArena` write/alloc counters — NOT by a grep returning non-zero in `crates/core/`" |
| C4a/C4b terminal split present in HANDOFF W5 | YES | HANDOFF:207-209 "C4a udot orphan wiring; C4b i8mm kernel ONLY if the W4 re-profile proves the digit leaf is a top-N tailwind self-time leaf" |

Every numeric LOC anchor alphaE uses to size its budgets is verified exact at HEAD.
No fabricated path, no fabricated symbol this cycle (the V2 `resolve_builder_routes`
defect remains struck + grep-clean). The three V4 sibling folds are confirmed landed.

---

## V4 fold confirmation (the three sibling REVISEs from V3 — none touched α-E's cost surface)

CH4-V3 raised ZERO new dispositions (12/12 ACCEPT). The three V3-cycle REVISEs that
fed the V4 changelog all targeted SIBLING artefacts (alphaA, alphaC, alphaD), not the
α-E candidate cost surface:

- **V3-CH1-a → alphaA:138-148** (broadcast-row count reconciliation, 24/112-135):
  VERIFIED landed (alphaA:140-147). This is a measurement-count reconciliation; it
  does not alter any LOC budget, risk class, wave edge, scalar-ref, checkasm, or
  same-wave-consumer claim in α-E. Cost-neutral. CONFIRMED.
- **V3-CH1-b → alphaC:227-231** (grep-substring "25 vs 24" mislabel correction):
  VERIFIED landed. A REDRESS-citation-count correction; no α-E candidate cost surface
  touched. Cost-neutral. CONFIRMED.
- **F1 orphan → alphaD:154 O5** (css_l4.toml LOC label relabelled to TOTALITY/SK-V18,
  not an SK-V17 exit gate): VERIFIED landed. This is the ONE V4 fold with a cost
  bearing, and it is in the RIGHT direction: it explicitly removes the 594-line
  `css_l4.toml` from the SK-V17 close/exit-gate surface (it is a repo-root xtask
  TOTALITY artefact, not a skinny owner path), reaffirming "the skinny-greppable exit
  gate is grammar-derivation, NOT TOML-LOC count." This PREVENTS a receiver from
  burning LOC chasing a TOML-LOC-convergence metric on an un-benched tree — it is a
  cost guard, cross-consistent with SYNTHESIS §0.1 (totality paths = SK-V18 fold) and
  the HANDOFF `tape_activated` wrong-tree guard. CONFIRMED cost-correct.

The α-E candidate body (C0/C1/C2/C3/C4a/C4b + §2/§3/§4) is byte-identical to V3 (the
alphaE V4 changelog at :12-21 states this explicitly: "The candidate content below is
therefore UNCHANGED from V3 (it converged); only this changelog and the cycle stamp
advance to V4"). Every cost anchor re-verified exact at HEAD. No new cost defect.

---

## Dispositions (path:line + concrete fix)

### alphaE candidate sections (the primary CH4 surface)

**[ACCEPT] alphaE §C0 (lines 163–214) — de-fact-stream typed Track 1.**
LOC ~400–700 (alphaE:201) with generated.rs flagged as template output under
`generated-size-budget` (alphaE:201-203): credible against the regen surface
(7× `css_l4_*/generated.rs`). Risk MEDIUM-HIGH (alphaE:204) with the routing-fidelity
blocker cited (`w6tape-conversion-report.md:54`, alphaE:204-206). Scalar-ref N/A
correctly justified (codegen route, not a primitive — CH4 scalar-ref binds
*primitives*, alphaE:184-185). checkasm N/A with the 8-field structural equality
(rules=10136/style=9561/sel=9561/decls=20043, errors=0) re-run on the NEW path as the
correctness gate (alphaE:186-190). Same-wave consumer named + real: the benched
`track1_*` fns consume the typed summary in the SAME commit the generator stops
emitting fact-stream (alphaE:191-193). `W5C_REQUEST_FACT_PROFILES` (`lib.rs:336`,
verified) named as the Lock-14 phrase-#1 retire target (alphaE:179-180,196). Wave
alignment (precondition, no perf promise, alphaE:165,199-200) is honest. ACCEPT.

**[ACCEPT] alphaE §C1 (lines 216–296) — tape wiring + lazy cursor view.**
The V2 REVISE remains fully reconciled (FOLD-CONFIRMED V3, re-verified at V4 HEAD).
LOC ~600–1000 credible (alphaE:277). Risk HIGH (alphaE:279) with the two named
blockers (routing fidelity + borrowed-slice-vs-lazy, `w6tape-conversion-report.md:54,55`,
alphaE:279-284) and the directive decision promoted to an ENTRY GATE (alphaE:282-284).
Scalar-ref N/A (data-structure migration, alphaE:240-241) correct. checkasm N/A with
`tape_substrate` 10/10 extended to CSS (alphaE:242-244). Same-wave consumer = the C0
typed-summary fn consuming the lazy `ValueRef` view in the SAME commit, Lock-1
same-wave rule (`LOCKS.md:75`) cited (alphaE:245-248). The owner-path seam is correct
+ verified exact at HEAD: the seven `RequestFactsProfile` literals at
`regen_css.rs:45,63,81,99,117,135,153` + entry fn `:164` + consumers `lib.rs:567,611`
(alphaE:231-238). The generality EXIT gate (alphaE:249-268) is keyed to JSON+CSS only
with `sheets_witness` struck as non-dischargeable (24-LOC `EventGrammar`, no
`BackendRule`, fail-close negative controls at `lib.rs:1075-1090`) and the Sheets
rider demoted to SK-V18 — a CH2 fold that is also cost-correct (it stops the receiver
burning LOC on a non-existent projection target). ACCEPT.

**[ACCEPT] alphaE §C2 (lines 298–363) — NEON structural pre-scan via PrimitiveKernels.**
The model CH4 candidate. LOC ~300–500, "kernels REUSED, zero new SIMD source"
(alphaE:352-353): verified — `select_classifier`:42, `PrimitiveKernels`:50,
`lo6_table_admissible`:101 all exist in skinny. Scalar-ref PRESENT
(`scalar::classify_chunk` `dispatch.rs:21` verified; `scan_structurals_scalar`
`json/scan.rs:32` cited, alphaE:318-320). checkasm PRESENT + precedented — the cited
tests (`checkasm_byte_class_from_table_64`, `checkasm_ascii_set_member_find_64`,
`checkasm_structural_terminator_64`, `checkasm_bulk_emit_positions_64`) all verified
present in `bbnf-simd/tests/`. The lo6-admissibility fallback (alphaE:330-333,348-350)
correctly makes scalar the honest answer on alphabet collision — no CSS special-case.
Same-wave consumer = C1 tape build consuming the `Vec<u32>` index (alphaE:334-338).
Risk MEDIUM, correctly the lowest perf-risk lever (alphaE:354-355). The
S-P1-re-confirm obligation on the 56%/10% hot-leaf figures (alphaE:300-302) is the
right cost-control (don't size a kernel against an unconfirmed core-tree profile).
ACCEPT.

**[ACCEPT] alphaE §C3 (lines 364–410) — commit-by-construction spine.**
LOC ~400–700 (alphaE:404). Risk HIGH correctly flagged as "the candidate most likely
to need a triumvirate" (alphaE:405-407) with the provability-of-non-deposition crux
named. Scalar-ref N/A (codegen control-flow, alphaE:382). checkasm N/A with the NEW
gate (no-checkpoint Alts PROVEN non-depositing at GENERATION time, not heuristically,
alphaE:383-387) — the right cost-control; the equality count catches an over-eager
removal (CH5 coupling acknowledged, alphaE:386-387). Same-wave consumer = the parser
spine itself (alphaE:388-389). The S-P1-re-confirm tag on the ~31% own-compute figure
(alphaE:366-369) is correct. ACCEPT.

**[ACCEPT] alphaE §C4a (lines 412–443) — wire orphan udot into CSS number leaf.**
The LOW-risk half of the V1 split, correctly budgeted. LOC ~100–150 (alphaE:437),
risk LOW (alphaE:438-439), scalar-ref PRESENT (`digit_mac.rs:15-22` cfg-fallback —
verified at `:15` `#[cfg(not(target_feature="dotprod"))]`, alphaE:423-425), checkasm
REQUIRED+NEW (`checkasm_digit_mac`, verified absent today, so "the test is the new
artefact" at alphaE:426-428 is honest), same-wave consumer = the CSS number leaf
consuming `parse_4_digits` in the SAME commit udot is wired (alphaE:429-431), whole
purpose = retire the `digit_mac` orphan. Admits unconditionally (alphaE:417-418).
ACCEPT.

**[ACCEPT] alphaE §C4b (lines 445–492) — NET-NEW i8mm kernel, GATED.**
The MEDIUM-HIGH-risk half, correctly gated. LOC ~150–300 (alphaE:483), risk
MEDIUM-HIGH (alphaE:485-486), scalar-ref PRESENT+REQUIRED (scalar twin mandatory,
alphaE:461-463), checkasm REQUIRED+NEW (`checkasm_i8mm_*`, alphaE:464-467, verified
absent), and — the load-bearing CH4 cost discipline — a hard ENTRY GATE
(alphaE:471-473): C4b lands ONLY if a Wave-5 re-profile proves the digit/number leaf
is top-N tailwind self-time; if not, C4b does NOT dispatch (no net-new orphan kernel).
i8mm grep-clean-absent re-verified at V4 HEAD — the net-new feature-detection
plumbing cost is real + acknowledged. The exit gate permits an honest profiled
residual (no paper-close, alphaE:474-482). Same-wave consumer = the CSS consumer lands
with the kernel; runtime detection threads the OnceLock table ONCE (`dispatch.rs:58/59`,
verified), never in the per-leaf hot loop (alphaE:468-470,487-492). Model gated-kernel
candidate. ACCEPT.

**[ACCEPT] alphaE §2 dependency order (lines 496–522) + §3/§4 (lines 524–581).**
Wave alignment C0→C1→C2→C3→C4a→C4b with per-edge gates (≥30→≥80→≥300→cross) and the
explicit ENTRY GATE on C1 (borrowed-slice-vs-lazy, alphaE:502) is sound CH4
sequencing. The "C0+C1 coupled, may be one wave" note (alphaE:518) is correct. The
C4a-unconditional / C4b-gated terminal split (alphaE:511-516) is the V1 fold, correctly
sequenced. §3 cross-cutting discipline carries the JSON+CSS witness scoping
(alphaE:539-557); §4 binds the unmeasurable-CSS case to PASS-ALPHA §8 and carries the
C4b hard entry gate (alphaE:578-580). ACCEPT.

### SYNTHESIS receiver-cost sections (CH4 owner-path + wave-alignment surface)

**[ACCEPT] SYNTHESIS §0.1 (benched-surface note + close-condition table).**
Owner-path cost surface is correctly on the skinny tree: `Tape`/`ValueRef`/
`TapeBuilder` (`runtime/src/tape/`), `BackendRule` + `lower/tape_plan.rs`, `dispatch.rs`
`select_classifier`:42 / `lo6_table_admissible`:101 / `PrimitiveKernels`, `digit_mac.rs`
udot orphan (`parse_4_digits_dotprod` :27 / `udot` :40 — all verified at HEAD,
SYNTHESIS:60-64). The CSS-routing-live note correctly names `W5C_REQUEST_FACT_PROFILES`
(`lib.rs:336`) as a Lock-14 phrase-#1 construct "SK-V17 must retire, not extend"
(SYNTHESIS:57-58). The totality-tree symbols (`StructLayout`/`OpenFrame`/`CssArena`/
`TapeStructBuilder`) are explicitly the SK-V18 fold target, NOT SK-V17 owner paths
(SYNTHESIS:65+) — the load-bearing CH4 cost guard (a receiver editing `crates/core/`
burns LOC on an un-benched tree). ACCEPT.

**[ACCEPT] SYNTHESIS §0.3 receiver goalset (lines 168–176+).** The seam-flip row
names the concrete edit site (`regen_css.rs` seven `RequestFactsProfile` literals,
SYNTHESIS:169-170) — verified exact — so the receiver does not re-derive it. Every
owner path targets the skinny tree; the totality paths
(`crates/core/src/backend/rust/emitter/`, the totality `emit_builder` fn + `OpenFrame`
template, `css_l4/builder.rs:274`) are explicitly the SK-V18 fold target, "a receiver
editing them would burn LOC on an un-benched tree (CH1-R1)" (SYNTHESIS:170-174). This
is cost-correct receiver scoping. ACCEPT.

**[ACCEPT] SYNTHESIS §0.5 per-corpus close conditions.** Wave alignment of
interventions to corpora is sound and unchanged from V3; the F1 V4 fold (css_l4.toml →
SK-V18, INFORMATIONAL) reinforces that the SK-V17 close criterion is grammar-derivation
+ a regular-corpus crossing, NOT a TOML-LOC metric. Corpus set verified
`{bootstrap, tailwindcss, material-components-web, animate}` (`css_l4_corpus.rs:22-54`)
— no phantom `normalize`. ACCEPT.

### HANDOFF receiver-cost section

**[ACCEPT] HANDOFF §Next-Move (lines 196–232).** The W1–W5 wave sequence carries
owner paths on the skinny tree (`skinny/crates/codegen/`, `bbnf-simd/src/dispatch.rs`,
`runtime/src/tape/`), the W2 seam-flip naming `RuntimeEmitterKind::RequestFacts` off +
`W5C_REQUEST_FACT_PROFILES` retire (HANDOFF:202-206, verified exact), the
C4a-unconditional / C4b-gated terminal (HANDOFF:207-209), and the "Each primitive
lands WITH its hot-path consumer in the same commit (no orphan kernels)" same-wave rule
(HANDOFF:210-211). The `tape_activated` proof obligation (PayloadArena write/alloc
counters, NOT a `crates/core/` grep, HANDOFF:226-228) is the correct wrong-tree-cost
guard. Revert protocol / hard caps / triumvirate discipline are contract-correctly
sanctioned-deferred to S-P3 per PASS-ALPHA §4.4 — not a paper-close. Wave alignment +
same-wave-consumer cost discipline present throughout. ACCEPT.

---

## Counts

- ACCEPT: 12 (alphaE C0, C1, C2, C3, C4a, C4b, §2/§3/§4; SYNTHESIS §0.1, §0.3
  receiver goalset, §0.5; HANDOFF Next-Move)
- REVISE: 0
- REJECT: 0

CH4 was converged at V3 (12/12 ACCEPT, zero REVISE, zero REJECT; the sole CH4-V2
REVISE closed with a verified fold). At V4 the alphaE candidate body is byte-identical
(converged, per alphaE:12-21), every LOC budget is sized against a line-count anchor
re-verified exact at HEAD `1c5bd7a25`, every scalar-ref / checkasm / same-wave-consumer
claim is verified present (or correctly flagged as the NEW artefact for C4a/C4b
checkasm + the C4b net-new i8mm kernel), and every risk class + wave-alignment edge is
internally consistent across alphaE / SYNTHESIS / HANDOFF.

The three V4-changelog sibling folds (CH1-a, CH1-b, F1) are confirmed landed and
cost-neutral to every α-E candidate — F1 is the only one with a cost bearing and it is
in the correct direction (removes the 594-line `css_l4.toml` TOML-LOC metric from the
SK-V17 close/exit-gate surface, a cost guard consistent with the SYNTHESIS §0.1
totality=SK-V18-fold scoping). CH4 raises no new cost matter; this lens is cost only.

Convergence note: 12/12 = 100% ACCEPT this lens, zero REVISE, zero REJECT, zero orphan
REVISE carried into V4. CH4 is converged for the second consecutive cycle (V3 + V4).
