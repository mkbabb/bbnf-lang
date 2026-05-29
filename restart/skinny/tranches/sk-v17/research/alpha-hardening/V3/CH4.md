# CHALLENGE CH4 — COST (cycle V3)

Lens: CH4 Cost. Per PASS-ALPHA §3 ("what is the LOC budget per intervention? Risk
classification? Wave alignment? Same-wave consumer present per intervention?") +
ORCHESTRATOR §3W lens set. Each candidate must carry: LOC budget + risk class +
wave alignment + same-wave consumer + scalar-ref + checkasm expectation.

Subject under bracket: CSS L4 typed parsing must BEAT lightningcss via the unified
tape/layout/projection model + dav1d-style aarch64 NEON hot leaves; preserve-rich-ast;
no x86; fully generalized for SKINNY, foldable into TOTALITY. aarch64 Apple M5 Max only.

Reviewed: `research/alpha/{alphaA..alphaE}.md`, `SYNTHESIS.md`, `HANDOFF.md`.
Note: there is no `alphaF.md`; the α-F contract draft IS `SYNTHESIS.md` + `HANDOFF.md`
at tranche root (PASS-ALPHA §2 row α-F, §6 tree). CH4 brackets the candidate cost
surface in alphaE (where LOC/risk/wave/consumer/scalar-ref/checkasm live) and the
receiver-cost surfaces in SYNTHESIS §0.1/§0.3/§0.5 + HANDOFF Next-Move.

This is cycle V3. CH4-V2 raised exactly one REVISE disposition:
- alphaE C1 owner-path symbol `resolve_builder_routes` was fabricated (right file
  `regen_css.rs`, wrong symbol); the fix named the seven real per-grammar
  `RequestFactsProfile` literals at `regen_css.rs:45–153`.
This V3 review's first job is to confirm that fold landed, then re-bracket the V3
cost surface for any new defect.

---

## Verification performed (every disposition is grounded; re-greped at V3 HEAD `1c5bd7a25`)

| Check | Result | Evidence |
|---|---|---|
| HEAD is `1c5bd7a25` (the bracket) | YES | `git rev-parse --short HEAD` = `1c5bd7a25` |
| `resolve_builder_routes` (V2 REVISE target) grep-clean | YES (struck) | `grep -rn resolve_builder_routes skinny/` empty |
| C1 owner-path now cites real seam-flip site | YES | alphaE:199 names `regen_css.rs:45,63,81,99,117,135,153`; verified `emitter: codegen::RuntimeEmitterKind::RequestFacts` at exactly those 7 lines; entry fn `regen_css` `:164` confirmed `pub(crate) fn regen_css(root: &Path)` |
| `W5C_REQUEST_FACT_PROFILES` const + consumers | YES | `codegen/src/lib.rs:336` const; consumers `:567,:611` (`for profile in …`); selected `:299` |
| `lib.rs:1075-1090` fail-close `google_sheets`/`bbnf` (C1 EXIT-gate basis) | YES | `w5a_sheets_bbnf_fail_closed_through_runtime_contract` asserts `Err(CodegenError::Lowering(…))` for both grammar names |
| `track1_facts -> Result<String,String>` is the benched CSS path (C0 subject) | YES | `nonjson_css_l4.rs:596` `pub fn track1_facts(input: &str) -> Result<String, String>` |
| `assert_lightningcss_strict_equality` def + call sites (telemetry-honesty row) | YES | def `nonjson_css_l4.rs:776`; calls `:1057,:3460` |
| `RuntimeEmitterKind::{CompiledLowering, RequestFacts}` selection | YES | `lib.rs:282` (CompiledLowering=JSON), `:291` (RequestFacts=CSS) |
| `digit_mac` udot orphan + scalar fallback (C4a) | YES | `digit_mac.rs:5` `parse_4_digits`, `:10-13` dotprod path, `:15` `#[cfg(not(target_feature="dotprod"))]` scalar twin, `:27` `parse_4_digits_dotprod`, `:40` `udot`, `:53` `dot4_i8` |
| i8mm grep-clean (C4b NET-NEW claim) | YES (NONE) | `grep -rn 'is_aarch64_feature_detected!("i8mm")' skinny/crates/` empty |
| `checkasm_digit_mac` / `checkasm_i8mm` present today (C4a/C4b "test is new") | ABSENT | `bbnf-simd/tests/` carries `checkasm_{ascii_set_member_find_64, bitmap_next_set_bit, bitmap_prefix_xor_64, bulk_emit_positions_64, byte_class_from_eq_set_64, byte_class_from_table_64, eob_pad_clamp, escape_mask_64, parity, structural_terminator_64, utf8_block}` — NO digit_mac/i8mm test — confirms both are the NEW artefact |
| `select_classifier`:42 / `PrimitiveKernels`:50 / `lo6_table_admissible`:101 / `classify_chunk`:19 / OnceLock:59 (C2 reuse) | YES | `bbnf-simd/src/dispatch.rs:42,50,101,19,59`; `:21` `SelectedBackend::Scalar => crate::scalar::classify_chunk(...)` (C2 scalar-ref) |
| `TapeBuilder`:42 / `push_plain_offset`:71 (C1/C3 seam) | YES | `runtime/src/tape/assembler.rs:42,71` (`:72` `let len = self.offsets.len();` = the O(1) checkpoint anchor) |
| `value_from_ref`:143 (C1/C2 lazy-view isomorphism) | YES | `json/value.rs:143` |
| `scan_structurals`:22 / `scan_structurals_scalar`:32 (C2 isomorphism + scalar-ref) | YES | `json/scan.rs:22,32` |
| fixture count = 148 (alphaE corrected 187→148) | YES | `grep -c 'fn parse_' generated_real_typed.rs` = **148** |
| 7× CSS generated.rs total | YES (4522) | `wc -l css_l4_*/generated.rs` tail = 4522 |
| `nonjson_css_l4.rs`=3644, `json/scan.rs`=337, `json/value.rs`=172 (copy-model size) | YES | `wc -l` |
| `lower/` modules (`tape_plan`=174, `offset_tape`=17, `event_tape`=17 + `eager_tape`) | YES | `ls skinny/crates/codegen/src/lower/`; `wc -l` |
| `sheets_witness` = 24-LOC `EventGrammar` trait impl (generality basis) | YES | `runtime/src/grammars/sheets_witness/event_grammar_witness.rs` = 24 LOC |
| skinny tape substrate location + size | YES | `runtime/src/tape/{mod.rs=237, assembler.rs=124, event_grammar.rs=31, offsets.rs=6}` (`TapeBuilder` non-generic, single sink) |
| benched CSS corpus set `{bootstrap, tailwindcss, material-components-web, animate}`, NO normalize | YES | `css_l4_corpus.rs:23,31,39` (+ animate); `normalize` grep-absent from the set |
| broadcast row count = 24 (not 6) | YES | `grep -c '^| css_l4/.*/direct_to_struct/main ' skinny/RESULTS.md` = 24 |

Every numeric LOC anchor alphaE uses to size its budgets is verified exact. No
fabricated path. No fabricated symbol this cycle — the V2 `resolve_builder_routes`
defect is struck and replaced with the real `regen_css.rs:45–153` profile-literal
flip-point.

---

## V2 fold confirmation (the sole CH4-V2 REVISE landed)

**FOLD (V2 REVISE alphaE C1 owner-path symbol): CONFIRMED LANDED.** alphaE V3
changelog (lines 12–20) strikes `resolve_builder_routes` and names the real seam:
the seven per-grammar `RequestFactsProfile` literals carrying
`emitter: RuntimeEmitterKind::RequestFacts` (`regen_css.rs:45,63,81,99,117,135,153`),
flipped via the `regen_css` fn (`:164`), with the two driving consumers
(`lib.rs:567,611`) named as the C0-retired loops. C1's owner-path body (alphaE:199–206)
carries the corrected citation verbatim. I re-greped all seven lines + the entry fn +
both consumers — every one is exact. FOLD ACCEPT.

The same correction propagated into **SYNTHESIS §0.3** "Tape activation + builder seam
flip" row (line 172: "the seven per-grammar `RequestFactsProfile` literals carrying
`emitter: RuntimeEmitterKind::RequestFacts` in `skinny/xtask/src/regen_css.rs:45,63,
81,99,117,135,153`, flipped off the fact-stream and regenerated via the `regen_css`
fn (`regen_css.rs:164`); the consumers are the two `for profile in …` loops at
`codegen/src/lib.rs:567,611`"). The V2 CH4 *completeness note* (which said S-P3 should
absorb the concrete seam edit site) is now folded directly into the SYNTHESIS receiver
row — the cost surface is no longer deferred. Cross-document consistency holds:
HANDOFF Next-Move §3 W2 (lines 199–202) carries the same seam description. FOLD
propagation ACCEPT.

---

## Dispositions (path:line + concrete fix)

### alphaE candidate sections (the primary CH4 surface)

**[ACCEPT] alphaE §C0 (lines 130–181) — de-fact-stream typed Track 1.**
LOC ~400–700 with generated.rs flagged as template output under `generated-size-budget`
(lines 168–170): credible against `nonjson_css_l4.rs`=3644 + 7×generated.rs=4522
regen. Risk MEDIUM-HIGH with the routing-fidelity blocker cited
(`w6tape-conversion-report.md:54`, line 171–173). Scalar-ref N/A correctly justified
(codegen route, not a primitive — CH4 scalar-ref binds *primitives*, line 151–152).
checkasm N/A with the 8-field equality (`rules=10136/style=9561/sel=9561/decls=20043`,
errors=0) re-run on the new path as the correctness gate (lines 153–157). Same-wave
consumer named and real: the benched `track1_*` fns (`nonjson_css_l4.rs:596`, verified)
consume the typed summary in the same commit the generator stops emitting fact-stream
(lines 158–160). `W5C_REQUEST_FACT_PROFILES` (`lib.rs:336`, verified) named as the
Lock-14 phrase-#1 retire target (lines 145–147,180–181). Wave alignment (precondition,
no perf promise, line 132–133/167) is honest. ACCEPT.

**[ACCEPT] alphaE §C1 (lines 183–263) — tape wiring + lazy cursor view.**
The V2 REVISE is fully reconciled (FOLD above). LOC ~600–1000 credible
(`json/scan.rs`=337 + `json/value.rs`=172 copy-model verified, plus 7-grammar view
modules + lower tape ops); risk HIGH with the two named blockers (routing fidelity +
borrowed-slice-vs-lazy, `w6tape-conversion-report.md:54,55`) and the directive
decision promoted to an ENTRY GATE (lines 249–251); scalar-ref N/A (data-structure
migration) correct (line 207–208); checkasm N/A with `tape_substrate` 10/10 extended
to CSS (line 209–211). Same-wave consumer = the C0 typed-summary fn consuming the lazy
`ValueRef` view in the same commit (lines 212–215), Lock-1 same-wave rule
(`LOCKS.md:75`) cited. The owner-path symbol is now correct — the seven
`RequestFactsProfile` literals at `regen_css.rs:45–153` + entry fn `:164` + consumers
`lib.rs:567,611` (lines 199–206), all verified exact. The generality EXIT gate
(lines 216–235) is correctly keyed to JSON+CSS only with `sheets_witness` struck as
non-dischargeable (24-LOC `EventGrammar`, no `BackendRule`, fail-close negative
controls at `lib.rs:1075-1090` — verified) and the Sheets rider demoted to SK-V18 —
that is a CH2 fold, but it is also cost-correct (it stops the receiver burning LOC
on a non-existent projection target). ACCEPT.

**[ACCEPT] alphaE §C2 (lines 265–329) — NEON structural pre-scan via PrimitiveKernels.**
The model CH4 candidate. LOC ~300–500, "kernels REUSED, zero new SIMD source"
(lines 319–320): verified — `select_classifier`:42, `PrimitiveKernels`:50,
`lo6_table_admissible`:101 all exist in skinny. Scalar-ref PRESENT
(`scalar::classify_chunk` `dispatch.rs:21` verified; `scan_structurals_scalar`
`json/scan.rs:32` verified). checkasm PRESENT and precedented — the cited tests
(`checkasm_byte_class_from_table_64`, `checkasm_ascii_set_member_find_64`,
`checkasm_structural_terminator_64`, `checkasm_bulk_emit_positions_64`) all verified
present in `bbnf-simd/tests/`. The lo6-admissibility fallback (lines 298–300, 315–317)
correctly makes scalar the honest answer on alphabet collision — no CSS special-case.
Same-wave consumer = C1 tape build consuming the `Vec<u32>` index (lines 301–305).
Risk MEDIUM, correctly the lowest perf-risk lever (line 321–322). The
S-P1-re-confirm-on-benched-path obligation on the 56%/10% hot-leaf figures (line 272)
is the right cost-control (don't size a kernel against an unconfirmed core-tree
profile). ACCEPT.

**[ACCEPT] alphaE §C3 (lines 331–377) — commit-by-construction spine.**
LOC ~400–700 credible (`tape_plan.rs`=174/`offset_tape.rs`=17/`event_tape.rs`=17 all
exist in `lower/`, verified). Risk HIGH correctly flagged as "the candidate most
likely to need a triumvirate" (lines 372–374) with the provability-of-non-deposition
crux named. Scalar-ref N/A (codegen control-flow) correct (line 349). checkasm N/A
with the NEW gate (no-checkpoint Alts PROVEN non-depositing at generation time, not
heuristically, lines 350–354) — the right cost-control: the equality count catches an
over-eager removal (CH5 coupling acknowledged, line 353–354). Same-wave consumer = the
parser spine itself (lines 355–356). The S-P1-re-confirm tag on the ~31% own-compute
figure (lines 333–335) is correct. ACCEPT.

**[ACCEPT] alphaE §C4a (lines 379–410) — wire orphan udot into CSS number leaf.**
The LOW-risk half of the V1 split, correctly budgeted. LOC ~100–150, risk LOW,
scalar-ref PRESENT (`digit_mac.rs:15-22` cfg-fallback — verified at `:15`
`#[cfg(not(target_feature="dotprod"))]`), checkasm REQUIRED+NEW (`checkasm_digit_mac`,
verified absent today, so the "the test is the new artefact" claim at lines 394–395 is
honest), same-wave consumer = the CSS number leaf consuming `parse_4_digits` in the
same commit udot is wired (lines 396–398), whole purpose = retire the `digit_mac`
orphan. Admits unconditionally (line 384–385). ACCEPT.

**[ACCEPT] alphaE §C4b (lines 412–459) — NET-NEW i8mm kernel, GATED.**
The MEDIUM-HIGH-risk half, correctly gated. LOC ~150–300, risk MEDIUM-HIGH, scalar-ref
PRESENT+REQUIRED (scalar twin mandatory, lines 428–430), checkasm REQUIRED+NEW
(`checkasm_i8mm_*`, lines 431–434, verified absent), and — the load-bearing CH4 cost
discipline — a hard ENTRY GATE (lines 438–440): C4b lands ONLY if a Wave-5 re-profile
proves the digit/number leaf is top-N tailwind self-time; if not, C4b does NOT dispatch
(no net-new orphan kernel). i8mm grep-clean-absent verified — the net-new
feature-detection plumbing cost is real and acknowledged. The exit gate permits an
honest profiled residual (no paper-close, lines 441–449). Same-wave consumer = the CSS
consumer lands with the kernel; runtime detection threads the OnceLock table ONCE
(`dispatch.rs:58/59`, verified), never in the per-leaf hot loop (lines 435–437,
454–459). Model gated-kernel candidate. ACCEPT.

**[ACCEPT] alphaE §2 dependency order (lines 463–489) + §3/§4 (lines 491–547).**
Wave alignment C0→C1→C2→C3→C4a→C4b with per-edge gates (≥30→≥80→≥300→cross) and the
explicit ENTRY GATE on C1 (borrowed-slice-vs-lazy) is sound CH4 sequencing. The
"C0+C1 coupled, may be one wave" note (line 485) is correct. The C4a-unconditional /
C4b-gated terminal split (lines 478–482) is the V1 fold, correctly sequenced. §3
cross-cutting discipline carries the JSON+CSS witness scoping; §4 correctly binds the
unmeasurable-CSS case to PASS-ALPHA §8 and carries the C4b hard entry gate
(lines 545–547). ACCEPT.

### SYNTHESIS receiver-cost sections (CH4 owner-path + wave-alignment surface)

**[ACCEPT] SYNTHESIS §0.1 (benched-surface note lines 25–62; close-condition table
lines 101–114).** Owner-path cost surface is correctly on the skinny tree:
`Tape`/`ValueRef`/`TapeBuilder` (`runtime/src/tape/`, verified `assembler.rs:42,71`),
`BackendRule` + `lower/tape_plan.rs`, `dispatch.rs` `select_classifier`/
`lo6_table_admissible`, `digit_mac.rs` udot orphan — all verified. The totality-tree
symbols (`StructLayout`/`OpenFrame`/`CssArena`/`TapeStructBuilder`) are explicitly the
SK-V18 fold target, NOT SK-V17 owner paths (lines 59–62) — this is the load-bearing
CH4 cost guard (a receiver editing `crates/core/` burns LOC on an un-benched tree).
The "NO new cursor/builder type is introduced" Lock-1 guard is preserved (Tape
activation row, line 104). Wave alignment cost is correctly targeted. ACCEPT.

**[ACCEPT] SYNTHESIS §0.3 receiver goalset (lines 159–176).** The V2 completeness note
is folded: the "Tape activation + builder seam flip" row (line 172) now names the
concrete seam-flip site (`regen_css.rs:45,63,81,99,117,135,153` + `regen_css` fn `:164`
+ consumers `lib.rs:567,611`) — all verified exact — so the receiver no longer has to
re-derive the edit site. Every owner path targets the skinny tree (line 161–167
explicitly names the totality emitter paths as the SK-V18 fold target). The
lazy-view-projection-generator row, CSS-typed-equality-re-proof row, N≥50-telemetry +
full-CSSOM-comparator row, NEON-grammar-general-hot-leaf-union row (with RE-PROFILE +
scalar-ref + checkasm + same-wave + aarch64-only + non-JSON exercise + C4a/C4b split,
line 175), and generated-state-clean-regen row are all cost-correct receiver
obligations with the right gates. ACCEPT.

**[ACCEPT] SYNTHESIS §0.5 per-corpus close conditions (lines 259–294).** Wave
alignment of interventions to corpora is sound: animate/bootstrap → four-lever stack
(W1/W2/W3/W4); tailwind → W5 (delimiter tuning + C4a udot + C4b i8mm-gated, line 286).
The tranche-level criterion ("at least one regular corpus crosses", line 289–290) with
tailwind allowed to land short + honest residual is correct cost-bounding. The Wave-0
re-baseline of the lightningcss bar (lines 269–272) correctly prevents stale-number
gates; the UNMEASURED-PENDING per-corpus-endpoint discipline (lines 274–280) correctly
refuses to size a gate against an inferred per-corpus lightningcss split. Corpus set
verified `{bootstrap, tailwindcss, material-components-web, animate}` at
`css_l4_corpus.rs:22-54` (line 263) — no phantom `normalize`. ACCEPT.

### HANDOFF receiver-cost section

**[ACCEPT] HANDOFF §Next-Move (lines 183–232).** The W1–W5 wave sequence (lines
196–208) carries owner paths on the skinny tree (`skinny/crates/codegen/`,
`bbnf-simd/src/dispatch.rs`, `runtime/src/tape/`), the C4a-unconditional /
C4b-gated terminal (lines 205–207), and the "each primitive lands WITH its hot-path
consumer in the same commit (no orphan kernels)" same-wave rule (lines 207–208). The
W2 seam-flip carries the same `RequestFactsProfile`-off-`RequestFacts` description as
SYNTHESIS §0.3 (lines 199–202). The `tape_activated` proof obligation (PayloadArena
write/alloc counters, NOT a `crates/core/` grep, lines 213–216) is the correct
wrong-tree-cost guard. Revert protocol / hard caps / triumvirate discipline are
explicitly sanctioned-deferred to S-P3 per PASS-ALPHA §4.4 (lines 231–232) — this is
contract-correct (PASS-ALPHA §4.4 names those as the S-P3 SPEC layer), not a paper-close.
Wave alignment + same-wave-consumer cost discipline present throughout. ACCEPT.

---

## Counts

- ACCEPT: 12 (alphaE C0, C1, C2, C3, C4a, C4b, §2/§3/§4; SYNTHESIS §0.1, §0.3
  receiver goalset, §0.5; HANDOFF Next-Move)
- REVISE: 0
- REJECT: 0

The sole CH4-V2 REVISE (alphaE C1 owner-path symbol `resolve_builder_routes`
fabricated) is confirmed FOLDED: the symbol is struck (grep-clean), the real
seam-flip surface (seven `RequestFactsProfile` literals at `regen_css.rs:45–153` +
entry fn `:164` + consumers `lib.rs:567,611`) is named in both alphaE C1 and the
SYNTHESIS §0.3 receiver row, and every cited line is verified exact at HEAD
`1c5bd7a25`. The V2 completeness note (defer the seam edit site to S-P3) is also
folded — the SYNTHESIS receiver row now carries the concrete site.

Every LOC budget is sized against a verified line-count anchor; every scalar-ref,
checkasm, and same-wave-consumer claim is verified present (or correctly flagged as
the NEW artefact for C4a/C4b checkasm and the C4b net-new i8mm kernel); every risk
class and wave-alignment edge is internally consistent across alphaE / SYNTHESIS /
HANDOFF. CH4 raises no new CH1/CH2/CH3/CH5/CH6 matters; this lens is cost only.

Convergence note: 12/12 = 100% ACCEPT this lens, zero REVISE, zero REJECT. CH4 is
converged. The single V2 REVISE is closed with a verified fold; there is no orphan
REVISE carried into V3.
