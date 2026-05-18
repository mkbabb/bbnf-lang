# SK-V9 S-P2 CHALLENGE V3 — CH1 CORRECTNESS

Pass: S-P2 Research. Cycle: V3. Lens: CH1 CORRECTNESS.
Date: 2026-05-18.
Authority: `restart/prompts/ORCHESTRATOR.md` §3W / §3Z.
Re-verification cycle: CH1 already converged at V1 (96.7%) + V2 (98.2%)
— two consecutive ≥95%. V3 re-verifies that the eight-edit V3 fold
(`HARDENING-S-P2-V2-CONSOLIDATED.md` §V3-fold-requirements) introduced
no correctness defect and that the V3-added citations resolve.

Inputs verified this cycle:
- `restart/skinny/tranches/sk-v9/research/p2/hardening/V2/CH1.md`
  (the single carried-forward LOW REVISE — D11-V2 / V1-REVISE #1).
- `HARDENING-S-P2-V2-CONSOLIDATED.md` (the eight V3 fold targets).
- The V3-folded `skv9-p2-D-aarch64-asm-opportunities.md` (rev V3).
- The V3-folded `skv9-p2-F-sota-teardown-m5max.md` (cycle V3).
- `skinny/REDRESS.md` entries 28 + 33 (line ranges 324-337 / 394-418).
- `skinny/crates/runtime/src/grammars/json/generated.rs` (ContainerNext).
- `restart/ARCHITECTURE.md` §7.3 (`BackendShape` / `CollapsedStage`).
- `skinny/crates/parse-that-regex/src/lib.rs`,
  `skinny/crates/bbnf-simd/src/aarch64/` (spot-check source citations).

## §1 — V2-LOW-REVISE resolution

V2 CH1 carried exactly one defect: **D11-V2** (carry-forward of
V1-REVISE #1). P2-D cited REDRESS 28 + 33 by entry number only; the
explicit `REDRESS.md` line ranges named in the V1 CH1 §4 fold-target
(28 → 1241-1278, 33 → 1314-1343 *as V1/V2 read them*) were never
landed. V2 graded it LOW severity / citation hardness, non-load-bearing.

### §1.1 — D11-V2 fold landed — **RESOLVED**

The V3 fold landed the explicit line ranges in **three** sites of P2-D:

- **§5.5 prose** (`skv9-p2-D` lines 944-951): "REDRESS 28
  (`skinny/REDRESS.md:324-337`) admitted the host aarch64 [primitive
  kernels] … REDRESS 33 (`skinny/REDRESS.md:394-418`) refined …" —
  both entries now carry the explicit numeric line range inline.
- **§8 Sources** (`skv9-p2-D` line 1113): "entries 28
  (`REDRESS.md:324-337` — SK-V3 W0/1 host primitive admission, 16-byte
  tiny-string dispatch rejected), 33 (`REDRESS.md:394-418` — SK-V5 W3
  Class A `match_tiny_plain_string` NEON wiring invalidated …)".
- **§0 V3-fold footer** (`skv9-p2-D` lines 1189-1191): records the fold
  — "REDRESS 28 + 33 citations now carry explicit `skinny/REDRESS.md`
  line ranges (28 → `:324-337`, 33 → `:394-418`) in §5.5 and §8".

### §1.2 — Cited line ranges verified against `REDRESS.md` — **CONFIRMED**

The V3 fold did **not** use the line ranges V1/V2 CH1 carried
(1241-1278 / 1314-1343) — it cites **324-337** and **394-418**. This
cycle read `REDRESS.md` to adjudicate which is correct:

- `REDRESS.md:324-337` — entry **28**, header at line 324: "SK-V3 Wave
  0/1 closed SIMD parity and admitted the host aarch64 primitive
  kernels, but rejected active 16-byte tiny-string dispatch." Body
  (325-337) carries the `twitter` ~25% regression on both tracks. The
  cited range is **exact** — 324 is the entry header, 337 is the last
  body line before the blank separator.
- `REDRESS.md:394-418` — entry **33**, header at line 394: "SK-V5 Wave
  3: Class A `match_tiny_plain_string` NEON wiring is INVALIDATED as
  the parse-G fix …". Body (395-418) carries the kernel-versus-call-
  site mismatch and the cohort cites. The cited range is **exact** —
  394 is the entry header, 418 is the last body line.

The V1/V2 ranges (1241-1278 / 1314-1343) were **wrong** — V1 derived
them from a stale `REDRESS.md` revision; the file has since been
re-ordered (current `REDRESS.md` is 2729 lines, entry 28 sits at 324).
The V3 fold cites the *correct* live line ranges. The substantive
characterisation V2 verified (REDRESS 28's 16-byte dispatch / twitter
−25%; REDRESS 33's kernel-vs-call-site mismatch) is confirmed verbatim
against the now-correctly-cited bodies. D11-V2 is **fully closed** —
and the V3 fold is net-corrective, replacing a stale line range with
the verified-live one.

## §2 — V3 dispositions

Verdicts: ACCEPT (claim verified against evidence), REVISE (defect
requires fold), REJECT (load-bearing falsification).

### §2.1 — V3-added citations (the 8-edit fold)

| # | Claim | Verification | Verdict |
|---:|---|---|---|
| V1 | P2-D §5.5 cites `skinny/REDRESS.md:324-337` for REDRESS 28 | `REDRESS.md:324` is the entry-28 header; 337 the last body line — exact | **ACCEPT** |
| V2 | P2-D §5.5 cites `skinny/REDRESS.md:394-418` for REDRESS 33 | `REDRESS.md:394` is the entry-33 header; 418 the last body line — exact | **ACCEPT** |
| V3 | P2-D §8 Sources carries the same two line ranges with entry summaries | Line 1113 verified: `:324-337` + `:394-418` with correct one-line summaries of each entry | **ACCEPT** |
| V4 | The cited line ranges actually carry REDRESS 28 + 33 (not a stale range) | Read `REDRESS.md` 320-344 + 390-419: entry 28 at 324, entry 33 at 394 — both confirmed; V1/V2's 1241/1314 ranges were stale and are superseded correctly | **ACCEPT** |
| V5 | P2-D §5.3.1 EOR3 latency claim cites ARM DDI 0487 + M5-Max-unpublished caveat | Lines 818-822: "the EOR3/PMULL latency profile is per ARM DDI 0487 FEAT_SHA3 / FEAT_PMULL instruction descriptions; M5 Max P-core specifics are unpublished by Apple — treat the absolute cycle counts as a host-capability-gated estimate, the monotonic *ordering* EOR3 < PMULL is the load-bearing claim" — citation present, caveat honest, load-bearing claim correctly narrowed to the *ordering* not the absolute counts | **ACCEPT** |
| V6 | P2-D §5.3.1 EOR3 slice gains the six-row W10b no-regression maintain gate | Lines 858-864: "an explicit no-regression maintain gate on the six W10b WIN-block rows (`canada`, `citm_catalog`, `instruments`, `marine_ik`, `mesh`, `numbers`) as a hard blocking precondition" — mirrors the §4.4 CSSC CTZ slice; six rows named explicitly | **ACCEPT** |
| V7 | P2-F §2.1 ContainerNext reference now carries `generated.rs:341` enum-definition cite | Lines 86-88: "the enum is defined at `skinny/crates/runtime/src/grammars/json/generated.rs:341`, consumed at `:134-135` and emitted by `consume_array_next` at `:348-375`" — file:line present | **ACCEPT** |
| V8 | `generated.rs:341` is the `ContainerNext` enum definition | Source line 341: `enum ContainerNext {` (variants `Next(u8)`/`Done` at 342-343) — exact | **ACCEPT** |
| V9 | `generated.rs:134-135` consumes `ContainerNext` | Source 133-136: `match consume_array_next(state)?` → arms `ContainerNext::Next(byte)` (134) / `ContainerNext::Done` (135) — exact | **ACCEPT** |
| V10 | `consume_array_next` at `generated.rs:348-375` emits `ContainerNext` | Source: `fn consume_array_next` opens at 348; returns `ContainerNext::Next` at 370 and `ContainerNext::Done` at 375; body closes at 378. The 348-375 range covers definition through the last `ContainerNext` construction — accurate (the trailing `Err` + `}` at 377-378 are outside but carry no `ContainerNext` emit) | **ACCEPT** |
| V11 | P2-F §5.4 CollapsedStage reference anchored to `ARCHITECTURE.md` §7.3 | Lines 384-388: "CollapsedStage is the fifth `BackendShape` variant defined in the design corpus at `restart/ARCHITECTURE.md` §7.3 (`LayoutFacts.backend_shape`, enum at `ARCHITECTURE.md:1086`)" — anchor present | **ACCEPT** |
| V12 | `ARCHITECTURE.md` §7.3 is the `BackendShape` / `LayoutFacts.backend_shape` spec | §7.3 header at `ARCHITECTURE.md:1033` ("7.3 Side Tables"); the `BackendShape` enum spec block at 1060-1090; `LayoutFacts.backend_shape: HashMap<RuleId, BackendShape>` named at 1047/1060/1114 — §7.3 owns the spec, confirmed | **ACCEPT** |
| V13 | `ARCHITECTURE.md:1086` is the `CollapsedStage` enum variant | Source line 1086: `    CollapsedStage,` inside `pub enum BackendShape` (opens 1063) — exact; CollapsedStage *is* the fifth variant per the §7.3 prose at 1060 | **ACCEPT** |
| V14 | P2-F §5.2 sonic-rs `match_tiny_plain_string`-class lesson now inline-cites REDRESS 33 as pre-blocked | Lines 277-284: "wiring Class A NEON `match_tiny_plain_string` at the DirectBuild field-name match arm chain is the REDRESS 33 rejected shape … per `skinny/REDRESS.md` entries 33 and 66-69" — inline citation present; the lesson is correctly framed as a pre-blocked shape, not an admission | **ACCEPT** |
| V15 | P2-F §0 footer + P2-D §0 footer accurately record the V3 fold edits | P2-D §0 (1180-1192): five edits — six-row gate, DDI 0487 cite, §6.3 reword, REDRESS 28/33 line ranges, cascade-sequencing note. P2-F §0 (674-686): three edits — §5.2 REDRESS 33 inline-cite, §2.1 ContainerNext cite + §5.4 CollapsedStage anchor, asmjson §5 vocabulary path-anchor. All eight HARDENING-V2-CONSOLIDATED fold targets accounted for | **ACCEPT** |
| V16 | P2-D §0 footer cascade-sequencing constraint folded (CH3 fold #3) | Lines 1165-1171: the four "block on P2-A landing OR fail CH5" slices (§3, §4.4, §5.3.1, §5.4) create a wave-sequencing constraint S-P3 must honour — recorded; consistent with the §0 wording the V2 consolidation required | **ACCEPT** |
| V17 | P2-F §4 asmjson vocabulary anchor path-cite (CH2 hygiene fold #8) | §0 footer line 685: "anchors the canonical primitive-class taxonomy by path to `skv9-p1-v3-B-xctrace-time-profiler.md` §1.5" — non-CH1 hygiene item; the path-anchor introduces no correctness claim, only a provenance pointer | **ACCEPT** |

### §2.2 — V3 fold integrity (no new defect, no broken reference)

| # | Check | Verification | Verdict |
|---:|---|---|---|
| I1 | The eight V3 edits are additive (citations / gate / footer) — no claim retracted or rewritten | P2-D §0 (1180-1192) + P2-F §0 (674-686): every edit is a citation add, a gate add, or a footer note. No measured number, no verdict, no arithmetic touched | **ACCEPT** |
| I2 | V2-verified P2-E §6 PMU rederivation (load-bearing F2) untouched by V3 | The V3 fold targets only P2-D + P2-F (HARDENING-V2-CONSOLIDATED "two files"); P2-E carries no V3 edit — the F2 baseline + §6.2 arithmetic survive intact | **ACCEPT** |
| I3 | V2-verified P2-D F1 wiring fix (`unescape_uxxxx_x4_neon` at `lib.rs:402`) untouched | Source `parse-that-regex/src/lib.rs:402` re-read: `bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_x4_neon(&packed)` — still exact; V3 added only the REDRESS line ranges, did not touch §2.1/§3.x | **ACCEPT** |
| I4 | EOR3 six-row gate names rows consistent with the §4.4 CSSC CTZ slice | §5.3.1 names `canada / citm_catalog / instruments / marine_ik / mesh / numbers` as the "six W10b WIN-block rows" — the same six-row W10b set the §4.4 slice uses; internally consistent | **ACCEPT** |
| I5 | The DDI 0487 cite makes no falsifiable absolute-cycle claim | §5.3.1 explicitly demotes the absolute counts (PMULL 4-cycle / EOR3 1-cycle) to "a host-capability-gated estimate" and declares only the *ordering* EOR3 < PMULL load-bearing — the F6/V3 framing is honest; no uncited absolute claim survives | **ACCEPT** |
| I6 | The ContainerNext sub-citations (`:134-135`, `:348-375`) resolve | Source read: 134-135 are the `match` arms; 348 opens `consume_array_next`; 375 is the `ContainerNext::Done` return — all three resolve | **ACCEPT** |
| I7 | P2-F §5.4 V9.5-PSI "rejects Rust-emitted DPDAs" claim is consistent with §7.3 | `ARCHITECTURE.md:1110` ("Codegen-emitted *explicit* Rust automatons do not survive this lowering … the lone exception — `CollapsedStage` — therefore consumes hand-written NASM") — P2-F §5.4 "the V9.5-PSI binding rejects Rust-emitted DPDAs" is consistent with the §7.3 design corpus | **ACCEPT** |
| I8 | No V3 edit reopened a pre-blocked route | P2-F §5.2 V3 edit *strengthens* the pre-block (adds the REDRESS 33 inline-cite to the lesson); P2-D V3 edits add gates/citations. The V3 fold is monotone-conservative — it cannot reopen a route | **ACCEPT** |

### §2.3 — Spot-check (≥12 citations across P2-D + P2-F)

| # | Citation | Report | Verification | Verdict |
|---:|---|---|---|---|
| S1 | `unescape_uxxxx_neon` at `unescape_uxxxx.rs:74` | P2-D §8 | Source line 74: `pub unsafe fn unescape_uxxxx_neon(ptr: *const u8)` — exact | **ACCEPT** |
| S2 | `unescape_uxxxx_x4_neon` at `unescape_uxxxx.rs:125` | P2-D §8 | Source line 125: `pub unsafe fn unescape_uxxxx_x4_neon(quartets: &[u8; 16])` — exact | **ACCEPT** |
| S3 | `HEX_NIBBLE_LUT` at `unescape_uxxxx.rs:201` | P2-D §8 | Source line 201: `pub const HEX_NIBBLE_LUT: [u8; 16]` — exact | **ACCEPT** |
| S4 | `scan_string_special_block` at `string_block.rs:57` | P2-D §8 | Source line 57: `pub unsafe fn scan_string_special_block(` — exact | **ACCEPT** |
| S5 | `interesting_mask` at `string_block.rs:14-17` | P2-D §8 | Source 15-17: `pub fn interesting_mask(self) -> u16 { … terminator_mask \| escape_mask \| control_mask \| non_ascii_mask }` — the fn + body span 15-17 (14 is the `#[inline(always)]` attr); the 14-17 range covers attr-through-close, accurate | **ACCEPT** |
| S6 | `match_string_at_quote_trusted_utf8` at `lib.rs:162` | P2-D §8, P2-F | Source line 162: `pub fn match_string_at_quote_trusted_utf8(` — exact | **ACCEPT** |
| S7 | `validate_string_escape` at `lib.rs:284` | P2-D §8 | Source line 284: `fn validate_string_escape(input: &[u8], slash: usize)` — exact | **ACCEPT** |
| S8 | `read_hex_unit_scalar` at `lib.rs:945` | P2-D §8 | Source line 945: `fn read_hex_unit_scalar(hex: &[u8]) -> Option<u16>` — exact | **ACCEPT** |
| S9 | `hex_nibble` at `lib.rs:959` | P2-D §8 | Source line 959: `fn hex_nibble(byte: u8) -> u8` — exact | **ACCEPT** |
| S10 | `skip_string_plain_trusted` at `lib.rs:547` | P2-D §8 | Source line 547: `fn skip_string_plain_trusted(input: &[u8], mut cursor: usize)` — exact | **ACCEPT** |
| S11 | `unescape_uxxxx_x4_neon` consumed at `lib.rs:402` | P2-D §0/§2.1 | Source line 402: `bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_x4_neon(&packed)` — exact (re-confirmed) | **ACCEPT** |
| S12 | `unescape_four_unicode_escapes` dispatched at `lib.rs:778` | P2-D §0 | Source line 778: `if let Some(batch) = unescape_four_unicode_escapes(bytes, slash, &mut out)` — exact | **ACCEPT** |
| S13 | `REDRESS.md:324-337` = entry 28; `:394-418` = entry 33 | P2-D §5.5/§8 | Both ranges read; headers at 324 + 394; bodies terminate at 337 + 418 — exact (the V3 fold's headline correctness item) | **ACCEPT** |
| S14 | `generated.rs:341` = `ContainerNext` enum | P2-F §2.1 | Source line 341: `enum ContainerNext {` — exact | **ACCEPT** |
| S15 | `ARCHITECTURE.md:1086` = `CollapsedStage` variant; §7.3 owns `BackendShape` | P2-F §5.4 | Line 1086 `CollapsedStage,` inside `BackendShape`; §7.3 header at 1033 — exact | **ACCEPT** |
| S16 | `match_tiny_plain_string_neon` at `match_tiny_plain_string.rs:79` | P2-D §8 | Source: `pub unsafe fn match_tiny_plain_string_neon(` is at line **81**; line 79 is `#[cfg(target_arch = "aarch64")]`. Two-line offset — see §4 | **REVISE** (LOW; pre-existing, non-V3) |

**Spot-check: 15 ACCEPT, 1 REVISE.** Sixteen distinct citations
verified across P2-D + P2-F — beyond the ≥12 floor. The lone REVISE
(S16) is a pre-existing two-line citation offset, *not* a V3 edit, and
non-load-bearing (a §3.6 cross-reference).

## §3 — Aggregate verdict

| Cohort | ACCEPT | REVISE | REJECT | Total | ACCEPT rate |
|---|---:|---:|---:|---:|---:|
| §2.1 V3-added citations (8-edit fold) | 17 | 0 | 0 | 17 | 100% |
| §2.2 V3 fold integrity | 8 | 0 | 0 | 8 | 100% |
| §2.3 Spot-check | 15 | 1 | 0 | 16 | 93.8% |
| **Aggregate** | **40** | **1** | **0** | **41** | **97.6%** |

**Verdict.** CH1 CORRECTNESS re-verifies clean at **97.6% ACCEPT**
with **zero REJECTs** — above the ≥95% bar for a third consecutive
qualifying cycle (V1 96.7%, V2 98.2%, V3 97.6%).

- The **V2 carried-forward LOW REVISE (D11-V2)** is **fully closed**:
  P2-D §5.5 + §8 now carry explicit `REDRESS.md` line ranges for
  entries 28 (`:324-337`) and 33 (`:394-418`). The V3 fold further
  *corrected* the line ranges — V1/V2 CH1 had read stale ranges
  (1241-1278 / 1314-1343); the V3 fold cites the verified-live ranges.
  This cycle read `REDRESS.md` and confirmed 324-337 / 394-418 carry
  entries 28 + 33 verbatim. The fold is net-corrective.
- The **V3-added EOR3 latency citation** (P2-D §5.3.1) is honest: it
  cites ARM DDI 0487 FEAT_SHA3 / FEAT_PMULL, explicitly flags M5-Max
  P-core specifics as Apple-unpublished, demotes the absolute cycle
  counts to a host-capability-gated estimate, and narrows the
  load-bearing claim to the monotonic *ordering* EOR3 < PMULL. No
  uncited absolute-cycle claim survives.
- The **V3-added ContainerNext / CollapsedStage citations** resolve
  exactly: `generated.rs:341` is the `ContainerNext` enum;
  `ARCHITECTURE.md:1086` is the `CollapsedStage` variant inside
  `BackendShape`, whose spec §7.3 owns.
- The **V3 fold introduced no new uncited claim and broke no
  reference.** All eight edits are additive (citations, one
  no-regression gate, footer notes); the load-bearing F1 wiring fix
  and F2 PMU rederivation verified by V2 are untouched and survive
  intact. The fold is monotone-conservative.

Per §3Z: CH1 was already converged on V1+V2 (two consecutive ≥95%).
V3 confirms the convergence holds post-fold — the eight-edit V3 fold
is verified clean for CH1. **CH1 CORRECTNESS remains converged.**

## §4 — New defects

No defect was introduced by the V3 fold. One pre-existing, non-V3,
LOW-severity citation offset surfaced during the §2.3 spot-check:

| # | Defect | Owning report | Required edit | Severity |
|---:|---|---|---|---|
| 1 | S16. P2-D §8 Sources cites `match_tiny_plain_string_neon` at `match_tiny_plain_string.rs:79`; the function declaration is at line **81** (line 79 is the `#[cfg(target_arch = "aarch64")]` attribute, 80 the `#[target_feature]` attribute). A two-line offset. This citation is **pre-existing** — it was present in P2-D before the V3 fold and was not among the eight V3 edits; it was not exercised by the V2 CH1 spot-check. It is a §3.6 cross-reference ("low-6 TBL shape referenced in §3.6"), non-load-bearing. | P2-D | Change `match_tiny_plain_string.rs:79` to `:81` in the §8 Sources list — a one-token edit. May equally be folded at S-P3 close or formally waived as a sub-threshold citation-offset, since the function is uniquely named in the same file and the §3.6 reference is invocable. | LOW (citation offset; non-load-bearing; non-V3 origin) |

This is the *only* CH1 finding this cycle. It does not block CH1
convergence: it is LOW severity, predates the V3 fold, undermines no
falsifiability gate, and the named function resolves uniquely within
the cited file. The V3 fold itself — all eight edits — verified
defect-free. No load-bearing claim, arithmetic, verdict, or wiring
fact was disturbed by V3.
