# CHALLENGE CH4 — COST (cycle V2)

Lens: CH4 Cost. Per PASS-ALPHA §3 ("what is the LOC budget per intervention? Risk
classification? Wave alignment? Same-wave consumer present per intervention?") +
ORCHESTRATOR §3W lens set. Each candidate must carry: LOC budget + risk class +
wave alignment + same-wave consumer + scalar-ref + checkasm expectation.

Subject under bracket: CSS L4 typed parsing must BEAT lightningcss via the unified
tape/layout/projection model + dav1d-style aarch64 NEON hot leaves; preserve-rich-ast;
no x86; fully generalized for SKINNY, foldable into TOTALITY. aarch64 Apple M5 Max only.

Reviewed: `research/alpha/{alphaA..E}.md`, `SYNTHESIS.md`, `HANDOFF.md`.
Note: there is no `alphaF.md`; the α-F contract draft IS `SYNTHESIS.md` + `HANDOFF.md`
at tranche root (PASS-ALPHA §2 row α-F, §6 tree). CH4 brackets the candidate cost
surface in alphaE (where LOC/risk/wave/consumer/scalar-ref/checkasm live) and the
receiver-cost surfaces in SYNTHESIS §0.1/§0.3/§0.5 + HANDOFF.

This is cycle V2. CH4-V1 raised exactly two REVISE dispositions:
1. alphaE C4 bundled two distinct-risk interventions under one ~250–450 LOC line.
2. SYNTHESIS §0.1/§0.3 owner paths targeted the UN-BENCHED core tree.
This V2 review's first job is to confirm both folds landed, then re-bracket the V2
cost surface for any new defect.

---

## Verification performed (every disposition is grounded; re-greped at V2 HEAD `1c5bd7a25`)

| Check | Result | Evidence |
|---|---|---|
| `W5C_REQUEST_FACT_PROFILES` const exists at lib.rs:336 (C0 retire target) | YES | `skinny/crates/codegen/src/lib.rs:336` `const W5C_REQUEST_FACT_PROFILES: &[RequestFactsProfile] = &[` (also referenced :299,:567,:611) |
| `track1_facts -> Result<String,String>` is the benched CSS path | YES | `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:596` |
| `digit_mac` udot orphan: `parse_4_digits_dotprod`/`udot` lines | YES | `digit_mac.rs:27` (`parse_4_digits_dotprod`), `:40` (`udot`), scalar fallback `:15` (`#[cfg(not(target_feature="dotprod"))]`), `dot4_i8` `:53` — all as alphaE C4a cites |
| i8mm anywhere in skinny (C4b NET-NEW claim) | NONE | `grep -rn 'is_aarch64_feature_detected!("i8mm")' skinny/crates/` empty — confirms C4b is net-new |
| `select_classifier`:42 / `lo6_table_admissible`:101 / `PrimitiveKernels`:50 (C2 reuse surface) | YES | `skinny/crates/bbnf-simd/src/dispatch.rs:42,101,50,58` |
| `scalar::classify_chunk` scalar-ref (C2) | YES | `dispatch.rs:21` `SelectedBackend::Scalar => crate::scalar::classify_chunk(...)`, signature `:19` |
| `scan_structurals`:22 / `scan_structurals_scalar`:32 (C2 isomorphism) | YES | `json/scan.rs:22,32` |
| `value_from_ref`:143 (C1/C2 lazy-view isomorphism) | YES | `json/value.rs:143` |
| `TapeBuilder`:42 / `push_plain_offset`:71 (C1 append seam) | YES | `runtime/src/tape/assembler.rs:42,71` |
| checkasm tests present (C2/C4a precedent) | YES | `bbnf-simd/tests/`: `checkasm_{byte_class_from_table_64, ascii_set_member_find_64, bulk_emit_positions_64, bitmap_next_set_bit, structural_terminator_64, byte_class_from_eq_set_64, escape_mask_64, ...}` |
| `checkasm_digit_mac` present today (C4a says NEW) | ABSENT | not in `bbnf-simd/tests/` — confirms C4a's "test is the new artefact" |
| fixture count 148 (alphaE corrected 187→148) | YES | `grep -c 'fn parse_' generated_real_typed.rs` = **148** |
| 7× CSS generated.rs = 646 lines each (4522 total) | YES | `wc -l css_l4_*/generated.rs` |
| `runtime_generator.rs`=1336, `grammar_provider.rs`=308, `nonjson_css_l4.rs`=3644 | YES | `wc -l` |
| `json/scan.rs`=337, `json/value.rs`=172 (copy-model size) | YES | `wc -l` |
| `lower/` has `{offset_tape,event_tape,tape_plan,eager_tape}.rs` | YES | `ls skinny/crates/codegen/src/lower/` |
| `sheets_witness` = 25-line stub (generality witness) | YES | `event_grammar_witness.rs` 24 + `mod.rs` 1 = 25 |
| `RuntimeEmitterKind::{CompiledLowering,RequestFacts}` emit fork | YES | `grammar_provider.rs:41,42`; selected `lib.rs:282,291` |
| `emit_fact_stream` at generated.rs:5 (C0/C1 retire) | YES | `css_l4_*/generated.rs:5` `pub fn emit_fact_stream(input: &str) -> Result<String, CssFactError>` |
| `regen_css.rs` exists; CSS routes assign `emitter: RequestFacts` | YES (file); **symbol mis-cited** | `skinny/xtask/src/regen_css.rs` exists (9343 B); `emitter: RuntimeEmitterKind::RequestFacts` at `:45,63,81,99,117,135,153`; entry fn is `regen_css` `:164`. There is **no `resolve_builder_routes`** anywhere in skinny (grep clean). |

Every numeric LOC anchor alphaE uses to size its budgets is verified exact. No
fabricated path. One fabricated *symbol name* inside a real file (`resolve_builder_routes`).

---

## V1 fold confirmation (both CH4-V1 REVISE dispositions landed)

**FOLD-1 (V1 REVISE alphaE C4): CONFIRMED LANDED.** alphaE V2 (changelog lines 13–17,
candidates lines 341–421) splits C4 into:
- **C4a** (`alphaE:341–372`): wire the existing `digit_mac` udot orphan into the CSS
  number leaf. LOC ~100–150, risk **LOW**, scalar-ref PRESENT (`digit_mac.rs:15-22`
  cfg-fallback — verified), checkasm REQUIRED+NEW (`checkasm_digit_mac`, verified
  absent today so genuinely new), same-wave consumer = the CSS number leaf, admits
  **unconditionally** as orphan-retirement. This is exactly the V1 fix text.
- **C4b** (`alphaE:374–421`): NET-NEW runtime-detected i8mm kernel. LOC ~150–300, risk
  **MEDIUM-HIGH**, scalar-ref PRESENT+REQUIRED (scalar twin mandatory), checkasm
  REQUIRED+NEW (`checkasm_i8mm_*`), **GATED** behind a Wave-5 re-profile proving the
  digit/number leaf is top-N tailwind self-time; if unmet, **C4b does NOT land — no
  orphan kernel** (`alphaE:379–382,400–402`). i8mm grep-clean-absent verified — the
  net-new framing is correct.
Per-sub-candidate LOC + risk are now stated separately (the V1 ask). FOLD-1 ACCEPT.

**FOLD-2 (V1 REVISE SYNTHESIS §0.1/§0.3 owner-path mis-target): CONFIRMED LANDED.**
SYNTHESIS now carries a "Benched-surface note" (lines 21–58) that grep-verifies
`StructLayout`/`OpenFrame`/`CssArena`/`TapeStructBuilder`/`begin_compound`/
`css_l4/builder.rs:274` are **absent from `skinny/crates/`** (I re-verified the
absence by greping the live citations), and explicitly names the totality core-tree
paths as the **SK-V18 fold target, NOT SK-V17 owner paths** (lines 55–58, 146–149).
§0.3 receiver goalset (lines 151–158) now targets the skinny tree exclusively:
`skinny/crates/codegen/{grammar_provider.rs, lower/{tape_plan,offset_tape,event_tape}.rs}`,
`assembler.rs:42,71`, `runtime_generator.rs:17-25`, `runtime/src/tape/`,
`bbnf-simd/src/dispatch.rs`. The V1 ask was reconciled verbatim, and §0.3 row 1's
"NO new cursor/builder type is introduced" Lock-1 guard is preserved. FOLD-2 ACCEPT.

The C4a/C4b split also propagated into SYNTHESIS §0.3 NEON row (line 157: "udot orphan
wiring (C4a) admits unconditionally; the i8mm kernel (C4b) lands ONLY if the re-profile
proves the digit leaf is a top-N tailwind self-time leaf") and HANDOFF §Next-Move 3
(W5 "C4a udot orphan wiring; C4b i8mm kernel ONLY if the W4 re-profile proves...").
Cross-document consistency holds. FOLD propagation ACCEPT.

---

## Dispositions (path:line + concrete fix)

### alphaE candidate sections (the primary CH4 surface)

**[ACCEPT] alphaE §C0 (lines 107–158) — de-fact-stream typed Track 1.**
LOC ~400–700 with generated.rs flagged as template output under `generated-size-budget`
(line 145–147): credible against `runtime_generator.rs`=1336 + `nonjson_css_l4.rs`=3644
+ 7×646 regen. Risk MEDIUM-HIGH with the routing-fidelity blocker cited
(`w6tape-conversion-report.md:54`). Scalar-ref N/A correctly justified (codegen route,
not a primitive — CH4 scalar-ref binds *primitives*, line 128–129). checkasm N/A with
the 8-field equality (`rules=10136/style=9561/sel=9561/decls=20043`) re-run on the new
path as the correctness gate (line 130–134). Same-wave consumer named and real: the
benched `track1_*` fns (`nonjson_css_l4.rs:596`, verified) consume the typed summary
in the same commit the generator stops emitting fact-stream (line 135–137). The V2
addition — naming `W5C_REQUEST_FACT_PROFILES` (`lib.rs:336`, verified) as a Lock-14
phrase-#1 retire target (line 113,122–123,158) — is correct and load-bearing. Wave
alignment (precondition, no perf promise) is honest. ACCEPT.

**[REVISE] alphaE §C1 (lines 160–225) — tape wiring + lazy cursor view.**
The candidate is structurally sound — LOC ~600–1000 credible (`json/scan.rs`=337 +
`json/value.rs`=172 copy-model verified, plus 7-grammar view modules + lower tape
ops); risk HIGH with the two named blockers (routing fidelity + borrowed-slice-vs-lazy,
`w6tape-conversion-report.md:54,55`) and the directive decision promoted to an ENTRY
GATE (line 211–213); scalar-ref N/A (data-structure migration) correct; checkasm N/A
with `tape_substrate` 10/10 extended to CSS; same-wave consumer = the C0 typed-summary
fn consuming the lazy `ValueRef` view in the same commit (line 183–186), Lock-1
same-wave rule (`LOCKS.md:75`) cited; generality bound as an EXIT gate requiring the
generator to emit a `sheets_witness` view (line 187–197) — all of this ACCEPTS.
**The CH4 defect is a mis-cited owner-path symbol.** Line 175–176 names the C1 cost
surface as `skinny/xtask/src/regen_css.rs` (`resolve_builder_routes` — flip CSS routes
from eager fact-stream to tape append). The **file is correct** (`regen_css.rs` exists,
9343 B, and IS where each CSS grammar's `RequestFactsProfile` literal assigns
`emitter: RuntimeEmitterKind::RequestFacts` at `:45,63,81,99,117,135,153`, the exact
flip-point C1 wants), but the function **`resolve_builder_routes` does not exist
anywhere in skinny** (grep clean). This is the same defect *family* as the V1 SYNTHESIS
owner-path REVISE (receiver burns search/LOC budget on a non-existent symbol), at
smaller scale. **Fix:** replace `resolve_builder_routes` with the real surface —
"`skinny/xtask/src/regen_css.rs` (the seven per-grammar `RequestFactsProfile` literals
at `:45–153` carrying `emitter: RuntimeEmitterKind::RequestFacts`; flip these to the
tape-emitting emitter via the `regen_css` regen fn at `:164`)". Optionally also cite
`skinny/crates/codegen/src/lib.rs:567,611` (the two `for profile in
W5C_REQUEST_FACT_PROFILES` consumers that drive emission). The rest of C1 ACCEPTS;
this is a one-line owner-path correction.

**[ACCEPT] alphaE §C2 (lines 227–291) — NEON structural pre-scan via PrimitiveKernels.**
The model CH4 candidate. LOC ~300–500, "kernels REUSED, zero new SIMD source"
(line 281–282): verified — `select_classifier`:42, `PrimitiveKernels`:50,
`lo6_table_admissible`:101, `classify_tbl4`, `build_lo6_table` all exist in skinny.
Scalar-ref PRESENT (`scalar::classify_chunk` `dispatch.rs:21` verified;
`scan_structurals_scalar` `json/scan.rs:32` verified). checkasm PRESENT and
precedented — the cited tests (`checkasm_byte_class_from_table_64`,
`checkasm_ascii_set_member_find_64`, `checkasm_structural_terminator_64`,
`checkasm_bulk_emit_positions_64`) all verified present. The lo6-admissibility
fallback (line 259–262) correctly makes scalar the honest answer on alphabet collision
— no CSS special-case. Same-wave consumer = C1 tape build consuming the `Vec<u32>`
index (line 263–267). Risk MEDIUM, correctly the lowest perf-risk lever. The V2
addition of the S-P1-re-confirm-on-benched-path obligation on the 56%/10% hot-leaf
figures (line 234) is the right cost-control (don't size a kernel against an
unconfirmed core-tree profile). ACCEPT.

**[ACCEPT] alphaE §C3 (lines 293–339) — commit-by-construction spine.**
LOC ~400–700 credible (`tape_plan.rs`/`offset_tape.rs`/`event_tape.rs` all exist in
`lower/`, verified). Risk HIGH correctly flagged as "the candidate most likely to need
a triumvirate" (line 334–336) with the provability-of-non-deposition crux named.
Scalar-ref N/A (codegen control-flow) correct. checkasm N/A with the NEW gate
(no-checkpoint Alts PROVEN non-depositing at generation time, not heuristically,
line 312–316) — the right cost-control: the equality count catches an over-eager
removal (CH5 coupling acknowledged). Same-wave consumer = the parser spine itself
(line 317–318). The V2 S-P1-re-confirm tag on the ~31% own-compute figure (line
295–296) is correct. ACCEPT.

**[ACCEPT] alphaE §C4a (lines 341–372) — wire orphan udot into CSS number leaf.**
This is the LOW-risk half of the V1 split, and it is correctly budgeted. LOC ~100–150,
risk LOW, scalar-ref PRESENT (`digit_mac.rs:15-22` cfg-fallback — verified at :15),
checkasm REQUIRED+NEW (`checkasm_digit_mac`, verified absent today, so the "test is the
new artefact" claim at line 356–357 is honest), same-wave consumer = the CSS number
leaf consuming `parse_4_digits` in the same commit udot is wired (line 358–360),
whole purpose = retire the `digit_mac` orphan. Admits unconditionally. ACCEPT.

**[ACCEPT] alphaE §C4b (lines 374–421) — NET-NEW i8mm kernel, GATED.**
The MEDIUM-HIGH-risk half, correctly gated. LOC ~150–300, risk MEDIUM-HIGH, scalar-ref
PRESENT+REQUIRED (scalar twin mandatory, line 390–392), checkasm REQUIRED+NEW
(`checkasm_i8mm_*`, line 393–396), and — the load-bearing CH4 cost discipline — a hard
ENTRY GATE (line 400–402): C4b lands ONLY if a Wave-5 re-profile proves the digit/number
leaf is top-N tailwind self-time; if not, C4b does NOT dispatch (no net-new orphan
kernel). i8mm grep-clean-absent verified — the net-new feature-detection plumbing cost
is real and acknowledged. The exit gate permits an honest profiled residual (no
paper-close, line 403–411). Same-wave consumer = the CSS consumer lands with the kernel;
runtime detection threads the OnceLock table ONCE (`dispatch.rs:58`), never in the
per-leaf hot loop (line 397–399, 418–419). This is the model gated-kernel candidate.
ACCEPT.

**[ACCEPT] alphaE §2 dependency order (lines 425–451) + §3/§4 (lines 453–503).**
Wave alignment C0→C1→C2→C3→C4a→C4b with per-edge gates (≥30→≥80→≥300→cross) and the
explicit ENTRY GATE on C1 (borrowed-slice-vs-lazy) is sound CH4 sequencing. The
"C0+C1 coupled, may be one wave" note (line 447) is correct. The C4a-unconditional /
C4b-gated terminal split (lines 440–444) is the V1 fold, correctly sequenced. §4
correctly binds the unmeasurable-CSS case to PASS-ALPHA §8 and carries the C4b hard
entry gate. ACCEPT.

### SYNTHESIS receiver-cost sections (CH4 owner-path + wave-alignment surface)

**[ACCEPT] SYNTHESIS §0.1 + §0.3 (benched-surface note lines 21–58; receiver goalset
lines 151–158).** The V1 owner-path REVISE is fully reconciled (FOLD-2 above). All
receiver owner paths now target the skinny tree; the core-tree symbols are explicitly
the SK-V18 fold target (lines 55–58, 146–149); the "NO new cursor/builder type" Lock-1
guard is preserved (§0.3 row 1, line 153). The §0.3 NEON row (line 157) carries the
C4a-unconditional / C4b-gated split. Wave-alignment cost is now correctly targeted.
ACCEPT.
Note: the §0.3 "Tape activation + builder seam flip" row (line 154) cites
`runtime_generator.rs:17-25` and `assembler.rs:42,71` (both verified) and the
`W5C_REQUEST_FACT_PROFILES` deletion (`lib.rs:336`, verified) — but, like alphaE C1,
does NOT name the `regen_css.rs:45–153` profile-literal flip-point where the
`emitter: RequestFacts` assignment actually lives. This is not a REVISE on SYNTHESIS
(the row's cited paths are all real and the seam is correctly described as "route CSS
off `RuntimeEmitterKind::RequestFacts`"); it is a completeness note that S-P3 should
absorb when it authors owner paths in SPEC.md: the concrete edit site is
`regen_css.rs:45–153`.

**[ACCEPT] SYNTHESIS §0.5 per-corpus close conditions (lines 226–261).** Wave
alignment of interventions to corpora is sound: animate/bootstrap → four-lever stack
(W1/W2/W3/W4); tailwind → W5 (delimiter tuning + C4a udot + C4b i8mm-gated). The
tranche-level criterion ("at least one regular corpus crosses") with tailwind allowed
to land short + honest residual is correct cost-bounding. The Wave-0 re-baseline of
the lightningcss bar (lines 236–239) correctly prevents stale-number gates. The W5
intervention list (line 253) carries the C4a/C4b split. The UNMEASURED-PENDING
per-corpus-endpoint discipline (lines 241–247) correctly refuses to size a gate
against an inferred per-corpus lightningcss split. ACCEPT.

**[ACCEPT] SYNTHESIS §0.3 receiver rows "CSS typed equality re-proof", "N>=50
telemetry", "NEON grammar-general hot-leaf union", "Generated-state clean regen"
(lines 155–158).** CSS typed equality re-proof, N≥50 telemetry + full-CSSOM comparator,
NEON grammar-general hot-leaf union (RE-PROFILE first + scalar-ref + checkasm +
same-wave + aarch64-only + non-JSON exercise + C4a/C4b split), and generated-state
clean regen are all cost-correct receiver obligations with the right gates. The NEON
row explicitly carries the re-profile obligation + scalar-ref + checkasm + same-wave +
the C4a-unconditional/C4b-gated split (line 157), satisfying CH4. ACCEPT.

### HANDOFF receiver-cost section

**[ACCEPT] HANDOFF §Next-Move (lines 163–212).** The four-lever wave sequence
(lines 178–188) carries owner paths on the skinny tree, the C4a-unconditional /
C4b-gated terminal (line 184–188), the "each primitive lands WITH its hot-path consumer
in the same commit (no orphan kernels)" same-wave rule (line 187–188), and the explicit
sanctioned-deferral of revert protocol / hard caps / triumvirate discipline to S-P3
per PASS-ALPHA §4.4 (line 211–212). Wave alignment + same-wave-consumer cost discipline
present throughout. ACCEPT.

---

## Counts

- ACCEPT: 11 (alphaE C0, C2, C3, C4a, C4b, §2/§3/§4; SYNTHESIS §0.1+§0.3, §0.5,
  §0.3 receiver rows 3–6; HANDOFF Next-Move)
- REVISE: 1 (alphaE C1 owner-path symbol `resolve_builder_routes` fabricated — right
  file, wrong symbol; one-line fix naming the real `regen_css.rs:45–153` profile-literal
  flip-point)
- REJECT: 0

Both V1 CH4 REVISE dispositions (C4 split; SYNTHESIS owner-path mis-target) are
confirmed FOLDED. The sole V2 REVISE is the residual sibling of the V1 owner-path
defect — a fabricated function name inside an otherwise-correct owner file — and carries
a concrete path:line fix (`regen_css.rs:45–153` / `regen_css` fn `:164`), so it is not
an orphan REVISE. Every LOC budget is sized against a verified line-count anchor; every
scalar-ref, checkasm, and same-wave-consumer claim is verified present (or correctly
flagged as the NEW artefact for C4a/C4b checkasm). CH4 raises no CH1/CH2/CH3/CH5/CH6
matters; this lens is cost only.

Convergence note: 11/12 = 91.7% ACCEPT this lens. The single REVISE is a one-line
owner-path symbol correction with a cited fix; folding it into V3 alphaE C1 is
trivial and would carry CH4 to 100% ACCEPT.
