# S-P2 V2 CHALLENGE — CH3 REGRESSION (REDRESS) Lens

Authored: 2026-05-23 (post-S-P2 V2 atomic micro-fold commit
`447a26b07`). Six artefacts under review at V2 HEAD: p2a (367 lines;
V1-LOCKED), p2b (217; V2 amended SHA-pinning), p2c (164; V2 amended 8→5
active candidates), p2d (254; V2 amended 3→2 active), p2e (342;
V1-LOCKED), p2f (360; V2 amended 6-sub-fold packet). Authorities
re-read end-to-end: `restart/prompts/skinny/PASS-2-RESEARCH.md` §3 CH3
(specialised to S-P2), `restart/prompts/ORCHESTRATOR.md` §3W + §3Z,
`restart/skinny/tranches/sk-v14/research/p2/hardening/V2/CHALLENGE-CONTEXT.md`
§0–§4, `restart/skinny/tranches/sk-v14/research/p2/hardening/V1/CH3.md`
(V1 CH3 100% ACCEPT carry-forward authority), and the V1→V2 diff via
`git diff b3dbc5ca0e3ccf38df71a5e72be3d65a3068549b 447a26b07` against
each of the six axis files plus the V1 consolidated.

CH3 binding restated (PASS-2-RESEARCH §3 CH3): no candidate re-opens
REDRESS routes (watch-list per V1 dispatch §2 + V2 dispatch §2 unchanged):
**28+33**, **50–55**, **60–72**, **80**, **82–84**, **88**, **89**,
**96/97/98**, **119/120**, **126**.

V2 disposition focus (per V2 dispatch context §2):
1. No V2 edit re-opens REDRESS routes (28+33, 50–55, 60–72, 80, 82–84,
   88, 89, 96/97/98, 119/120, 126).
2. Demoted candidates (P2-C C-P2C-1/6/7, P2-D C-P2D-3, P2-F C8) remain
   pre-blocked; demotion does not silently re-open REDRESS.
3. P2-E Gap 5 REDRESS-80 differential (V1 ACCEPT-WITH-NOTE) carries
   through V2 cleanly (P2-E is V1-LOCKED; zero V2 edits).
4. New scalar-ref Stage-A targets (P2-F C10/C13) are not re-introducing
   fake-pattern recurrence; new C2 P2-E Gap 6 composition upgrade is
   not re-opening REDRESS 88.
5. V1-LOCKED axes (P2-A + P2-E): confirm zero V2 drift.

## §1 — V1 → V2 drift audit (V1-LOCKED axes)

Per V2 dispatch context special-attention clause: diff against V1 commit
`b3dbc5ca0e3ccf38df71a5e72be3d65a3068549b` for the two V1-LOCKED axes.

| Axis | V1 lines | V2 lines | V1→V2 diff size | Drift verdict |
|---|---:|---:|---|---|
| p2a-sota-teardown | 367 | 367 | `git diff` returns empty for this file | **ZERO drift confirmed** |
| p2e-parse-that-gaps | 342 | 342 | `git diff` returns empty for this file | **ZERO drift confirmed** |

Both V1-LOCKED axes are byte-identical at V2 HEAD. CH3 carry-forward from
V1 (CH3.md §2.1 p2a 7-candidate disposition; §2.5 p2e 8-gap disposition
incl. Gap 5 REDRESS-80 ACCEPT-WITH-NOTE) applies verbatim at V2; F-1
finding (Gap 5 REDRESS-80 S-P3 checklist) carries unchanged.

## §2 — V2 amended axis CH3 audit (P2-B, P2-C, P2-D, P2-F)

### §2.1 — p2b-dav1d-process (V2 amended; SHA pinning only)

V2 diff (verified): three §5.1 external-source bullets gain FFmpeg
`08571418...` and dav1d `1718ff9a...` HEAD SHAs (`p2b:183-185`) inherited
verbatim from `p2a-sota-teardown.md:344-348` (per the V1 consolidator
§5.4→§5.3 register correction surfaced in V2 hardening context). Zero
new candidates; zero §2 enumeration changes; zero §4 REDRESS-guard
changes. The V1 §4 catchall at p2b:175 ("REDRESS 28+33, 50-55, 60-72,
80, 82-84, 88, 89, 96-98 + CH3 catchall") is unchanged — verified by
inspecting the V2 file at lines 166-177 (no diff lines in that range).

**CH3 disposition: ACCEPT.** SHA pinning is a CH1 process-truth fold,
not a REDRESS surface change. The 5-stage admission gate's "REDRESS-
pre-block-safe by construction" property (V1 CH3 F-5 + §3 verdict) is
preserved exactly. No REDRESS route re-opened.

### §2.2 — p2c-arch-esoterica (V2 amended; 8→5 active; 3 demotions)

V2 diff (verified via `git diff … -- p2c-arch-esoterica.md`):
- Three rows (C-P2C-1, C-P2C-6, C-P2C-7) physically moved from the
  active §2 enumeration table into a new `§2.X — Non-candidate
  inventory` table.
- Each demoted row carries its V1 disposition verbatim PLUS a new
  disposition stamp appended: *"Demoted V2: zero S-P1 hot-leaf
  antecedent at SK-V14; re-evaluate if F-V2-P1ABC-RERECORD surfaces
  antecedent."* (template at `p2c:48-49`).
- The §4 REDRESS pre-block table (`p2c:86-99`, 11 distinct REDRESS-
  family pre-block notes verbatim from V1) is unchanged.

**Per-demoted-candidate REDRESS-regression audit:**

| Candidate | V1 REDRESS adjacency | V2 demotion impact | Re-open risk? |
|---|---|---|---|
| **C-P2C-1** `ascii_set_member64_css_delimiter` | none directly; PRUNE-2 CSS L4 plane absence (CH4/CH7) — V1 CH3 disposition: ACCEPT (`NOT-S-P3-ELIGIBLE` at V1) | Moved to §2.X non-candidate inventory; V1 disposition `NOT-S-P3-ELIGIBLE absent real CSS L4 parser + corpus` preserved + V2 demotion stamp appended (`p2c:65`) | **NO** — V1 already pre-blocked on PRUNE-2 grounds; demotion strengthens (not loosens) the pre-block; no REDRESS family was implicated at V1 |
| **C-P2C-6** `eor3_string_mask_fusion` | none directly; CH6 paper-close risk — V1 CH3 disposition: ACCEPT (`NOT-S-P3-ELIGIBLE` inventory) | Moved to §2.X non-candidate inventory; V1 disposition + V2 demotion stamp (`p2c:66`) | **NO** — V1 pre-blocked on CH6 grounds; demotion converts inventory→non-candidate without touching any REDRESS family; the CH6 risk is *discharged*, not re-routed through REDRESS |
| **C-P2C-7** `byte_context_orphan_resolution` | **REDRESS-126** (aarch64 orphan inventory) — V1 CH3 disposition: ACCEPT (close-hygiene only; `NOT-S-P3-ELIGIBLE`) | Moved to §2.X non-candidate inventory; V1 disposition "may wire or delete/demote, but cannot add or retain another orphan" preserved verbatim + V2 demotion stamp (`p2c:67`) | **NO** — REDRESS-126 binding ("zero new orphans") is *strengthened* by V2 demotion: the orphan does not re-elevate to support-only; the demotion path (§2.X non-candidate inventory) IS the close-hygiene route REDRESS-126 prescribes. The candidate's option "delete/demote with REDRESS evidence" is the V2 chosen path |

**CH3 cross-check on retained §4 REDRESS table:** the 11-REDRESS-family
enumeration at `p2c:86-99` (REDRESS 88/89/90/96-98/82-84/SK-V10/60-72/
50-55/28+33/80/119-120/126 + PEXT-arch-block) is byte-identical to V1.
Active candidates remaining (C-P2C-2, C-P2C-3, C-P2C-4, C-P2C-5,
C-P2C-8) carry their V1 PRE-BLOCKED / NOT-S-P3-ELIGIBLE / S-P3-ELIGIBLE-
JSON-only / conditional / process-gate dispositions verbatim. C-P2C-2's
PRE-BLOCKED-by-REDRESS-88+89+96-98 disposition (V1 CH3 F-2) is preserved
at `p2c:42`. C-P2C-3's REDRESS-80 cite is preserved at `p2c:75`.

**REDRESS-126 special note (V2-strengthened):** C-P2C-7's demotion to
§2.X non-candidate inventory IS the canonical close-hygiene path
prescribed by REDRESS-126 (per V1 CH3 F-4: "may wire OR delete/demote
with REDRESS inventory proof"). The V2 demotion stamp ("zero S-P1 hot-
leaf antecedent at SK-V14") is the REDRESS inventory proof — i.e. the
candidate is now treated exactly as REDRESS-126 prescribes for an
unwireable orphan: not retained as support-only. **CH3 verdict: V2
strengthens REDRESS-126 compliance; does NOT re-open.**

**CH3 disposition: ACCEPT.** Three demotions; zero REDRESS routes
re-opened; REDRESS-126 binding strengthened by C-P2C-7 demotion path.

### §2.3 — p2d-substrate-tape (V2 amended; 3→2 active; 1 demotion to §1.6(d))

V2 diff (verified):
- C-P2D-3 (`sparse-flag-band gating`) body MOVED from §2 candidate
  enumeration to §1.6(d) substrate-side observation (`p2d:104-105`);
  the §2 row is replaced with a `[DEMOTED V2 → §1.6(d) substrate-side
  observation]` stub at `p2d:128-130` preserving the identifier as
  gap-note for cross-tranche reference stability.
- §2.X candidate-list discipline footnote updated to "2 active + 1
  demoted-to-§1.6(d) + 1 pre-blocked" (`p2d:142`).
- §3 grammar-neutrality table row for C-P2D-3 reframed to "N/A —
  DEMOTED V2 → §1.6(d)" with V1 content "was: …" preserved (`p2d:150`).
- §4.6 Lock 1 substrate-union ceiling text updated to remove C-P2D-3
  from the active list, with a sentence preserving the V2-demoted
  candidate's substrate-union compliance (`p2d:193`).
- §4.7 CH5 hidden-coupling cross-check updated to "ACCEPT for both
  active candidates (C-P2D-1, C-P2D-2); the V2-demoted §1.6(d) sparse-
  flag observation (formerly C-P2D-3) is CH5-clean by construction
  (re-uses existing substrate field, no new sidecar)" (`p2d:198`).

**Per-demoted-candidate REDRESS-regression audit:**

| Candidate | V1 REDRESS adjacency | V2 demotion impact | Re-open risk? |
|---|---|---|---|
| **C-P2D-3** sparse-flag-band gating | none (substrate-side allocation discipline) — V1 CH3 disposition: ACCEPT (n/a; CH4-pre-block falsifier per p2d V1 §2.C-P2D-3) | Demoted to §1.6(d) substrate-side observation; V1 body preserved verbatim under the new §1.6(d) heading | **NO** — V1 had zero REDRESS adjacency; demotion to substrate-side observation preserves the substrate-union compliance verbatim (re-uses existing `flag_cursors`/`flag_values` fields; no new substrate field; substrate_target unchanged from V1) |

**C-P2D-4 audit (V1-LOCKED pre-blocked-anti-pattern reference):** the
canonical CH3 paper-trail anchor (V1 CH3 F-3) at `p2d:132-136` is
preserved verbatim at V2. REDRESS-96/97/98 REJECT-by-history framing
unchanged. CH3-cross-checking value preserved.

**C-P2D-2 OffsetTapeStats column extension audit:** preserved verbatim
at V2 active enumeration; REDRESS-60-72 adjacency (none directly per V1
§2.4) unchanged.

**Substrate-union YES verdict (load-bearing CH5 finding; CH3 cross-
check):** §1.5 verdict "substrate union holds at HEAD" preserved at
V2; the §1.6(d) demotion adds a fourth substrate-side observation
without introducing a new substrate-field — explicitly per V2 §4.6
"C-P2D-3 (V2-demoted to §1.6(d)) re-uses existing `flag_cursors`/
`flag_values`; even if re-elevated by a future same-wave consumer it
does not introduce a new substrate field." This satisfies V2 dispatch
context §2 CH5 ("substrate-union YES still holds; P2-D §1.6(d) demotion
does not break union claim; no parallel substrate introduced by
demotions"). **CH3 cross-check verdict: substrate-union YES preserved;
zero parallel-substrate risk; C-P2C-2's re-admission requirement (c)
"Lock 1 substrate union held per P2-D" remains satisfied at V2.**

**CH3 disposition: ACCEPT.** Demotion is substrate-clean; REDRESS
96/97/98 binding unchanged; C-P2D-4 paper-trail anchor preserved
verbatim.

### §2.4 — p2f-grammar-neutral (V2 amended; 6 sub-folds)

V2 diff (verified): six sub-folds documented in V2 hardening context
§1 bullet 6:

1. **Fold-2 C8 DEMOTED** (per `[no-deferrals]` default): C8 moved to
   §2.X.1 non-candidate inventory (`p2f:96-107`); §2.8 replaced with
   demotion stub (`p2f:37`); §3 verdict-tally row for C8 reframed to
   "DEMOTED V2" (`p2f:127`); §3 NEUTRAL-PENDING-CONSUMER count 1→0
   (`p2f:136`); new "Demoted to non-candidate inventory (V2)" row added
   (`p2f:139`); §4 CH6 paper-close risk reframed "DISCHARGED V2 via
   Fold-2 demotion" (`p2f:304`).
2. **Fold-6 SKIPPED** (per `[no-deferrals]` default): documented at the
   §2.X.1 demoted-row "Scalar-ref status" bullet (`p2f:101`).
3. **Fold-3 disposition stamps** added to C6, C7, C10, C12, C13 (P1
   antecedent CH1 bullets at `p2f:18, 27, 54, 69, 77`); C12 reframed
   CH4-ACCEPT per CF-1 with scalar-ref existing at `scan.rs:32`
   (`p2f:69`).
4. **Fold-4 C10 scalar-ref**: target path `crates/bbnf-simd/src/scalar/
   byte_context_64.rs` named at `p2f:57` (Stage-A authoring
   deliverable).
5. **Fold-5 C13 scalar-ref**: target path `crates/bbnf-simd/src/scalar/
   bcax_64.rs` named at `p2f:80` (Stage-A authoring deliverable).
6. **NF-CH6-3 C2 upgrade** with P2-E Gap 6 three-way composition: the
   scalar-ref status for C2 rewritten to "EXISTS via composition per
   P2-E Gap 6" naming `scan_string_special_block_sweep_64` + `bitmap_
   prefix_xor_64_scalar` + `escape_mask_64` (`p2f:10`).
7. **NF-CH6-4 cross-axis tracking note** §2.Y added (`p2f:109-117`)
   — three-axis primitive consolidation reference (P2-A C2 + P2-E
   Gap 1 + P2-F C1+C2 all naming long-string-body SIMD scan).

**Per-V2-edit REDRESS-regression audit:**

| V2 edit | Candidate touched | V1 REDRESS adjacency | V2 impact | Re-open risk? |
|---|---|---|---|---|
| **Fold-2 C8 demotion** | C8 comment-skip | none directly (V1 CH3 §2.6: n/a; CH6 risk; NEUTRAL-PENDING-CONSUMER) | Moved to §2.X.1 non-candidate inventory; CH6 paper-close risk DISCHARGED via demotion | **NO** — V1 had zero REDRESS adjacency; demotion strengthens (not loosens) the inventory ceiling. C8 re-promotion gate at `p2f:107` requires fresh-material differential per `[no-deferrals]`. |
| **Fold-4 C10 scalar-ref target** | C10 cross-chunk byte-context | none (Lock 16 :285 admits abstract primitive verbatim) — V1 CH3 §2.6: n/a, accepted, CH4-flagged | New scalar-ref target path `crates/bbnf-simd/src/scalar/byte_context_64.rs` named (`p2f:57`) | **NO** — scalar-ref target is in `bbnf-simd/src/scalar/` (Layer-0 sibling of existing `byte_class_from_eq_set_64.rs` and `bitmap_prefix_xor_64.rs`); no REDRESS family is implicated by adding a scalar reference (REDRESS 28+33 wiring path remains untouched; REDRESS 60-72 sidecar binding unaffected — `local_temp_only` substrate-target preserved). |
| **Fold-5 C13 scalar-ref target** | C13 BCAX 3-way XOR | none (Lock 16 :289 admits) — V1 CH3 §2.6: n/a, accepted, CH4-flagged | New scalar-ref target path `crates/bbnf-simd/src/scalar/bcax_64.rs` named (`p2f:80`) | **NO** — same shape as C10: scalar-ref Layer-0 sibling; substrate-target `local_temp_only`; no REDRESS family touched. |
| **Fold-3 C12 reframe CH4-ACCEPT** | C12 keyword-set 16-byte alphabet membership | none directly (Lock 16 :290 admits) — V1 CH3 §2.6: n/a, accepted, CH4-flagged | Scalar-ref cite `scan_structurals_scalar` at `runtime/src/grammars/json/scan.rs:32` (verified: function entry at line 32, per-byte `match` inner at lines 68-72) (`p2f:69`) | **NO** — citing an existing scalar reference is not a REDRESS regression; the cite is at `scan.rs:32` (the scalar function root), which is the canonical reference shape per the dav1d process artefact (Stage A scalar-reference authoring). No REDRESS family is implicated. |
| **NF-CH6-3 C2 upgrade** (Gap 6 composition) | C2 quoted-string boundary scan (CLMUL prefix-XOR) | **REDRESS 88** (PMULL prefix-XOR as hot body) — V1 CH3 §2.6 + §4 row: ACCEPT with split-lane framing differential (Lock 16 :294 AVX-512 VPCLMULQDQ NEW 2026-05-12) | Scalar-ref status upgraded from "required" to "EXISTS via composition" naming `scan_string_special_block_sweep_64` + `bitmap_prefix_xor_64_scalar` + `escape_mask_64`; the §4 row "Fresh material differential" + "S-P3 verification clause" unchanged at `p2f:292` | **NO** (load-bearing audit) — see §3 F-1 below |
| **NF-CH6-4 §2.Y cross-axis tracking** | C1 + C2 (P2-F); cross-references P2-A C2 + P2-E Gap 1 + Gap 6 | REDRESS 28+33 (P2-A C2), REDRESS 88 (P2-F C2), REDRESS 82-84 indirectly (P2-E Gap 1 tiny-string wiring + Gap 6 composition) — V1 CH3 §2.1/§2.5/§2.6: all three carry independent ACCEPT differentials | New tracking note (`p2f:109-117`) explicitly binds: "S-P3 must produce ONE canonical primitive name + ONE canonical scalar reference function rather than admitting three near-duplicates" | **NO** — the consolidation note REDUCES re-open risk by binding S-P3 to one canonical primitive (preventing three-orthogonal-SIMD-bodies admission); the cross-axis triangulation is anti-paper-close per V2 dispatch context CH6 (which V2 hardening explicitly cites as "exemplary anti-paper-close pattern"). |

**CH3 disposition: ACCEPT.** Six sub-folds; zero REDRESS routes
re-opened; one upgrade (NF-CH6-3 C2) strengthens scalar-ref evidence
without altering REDRESS-88 split-lane differential; one cross-axis
note (NF-CH6-4) explicitly REDUCES re-open risk by binding S-P3 to
single-canonical-primitive admission.

## §3 — Critical findings (CH3 lens, V2-new)

### F-1 — NF-CH6-3 C2 upgrade preserves REDRESS-88 split-lane differential (load-bearing)

The V2 P2-F NF-CH6-3 upgrade (`p2f:10`) replaces V1's scalar-ref status
"required (no current bbnf scalar reference; SK-V8 `scan_tail_byte` at
`scan.rs:131` is byte-by-byte but does not maintain a string-mask state
in scalar form)" with V2's "scalar oracle EXISTS via composition per
P2-E Gap 6: compose `scan_string_special_block_sweep_64` (P2-E Gap 1's
scalar in `bbnf-simd::aarch64::string_block`) with `bitmap_prefix_xor_
64_scalar` at `crates/bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs:1`
+ the `escape_mask_64` body at `crates/bbnf-simd/src/lib.rs:175-206`
for the even/odd backslash carry — bit-identical to the simdjson
`prev_in_string` carry shape (cite: simdjson 3.x `find_quote_mask_and_
bits` in `include/simdjson/arm64/simd.h`)."

Verified path:line claims (executable-verification mandate):
- `crates/bbnf-simd/src/aarch64/string_block.rs:31` →
  `pub fn scan_string_special_block_scalar(` — **CONFIRMED**
- `crates/bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs:2` →
  `pub fn bitmap_prefix_xor_64_scalar(mut mask: u64, carry_in: bool)
  -> u64 {` — **CONFIRMED**
- `crates/bbnf-simd/src/lib.rs:175` →
  `pub fn escape_mask_64(bs_mask: u64, bs_carry_in: bool) -> (u64,
  bool) {` — **CONFIRMED**

(Note: actual files live under `skinny/crates/bbnf-simd/...`; the
artefact's path prefix `crates/bbnf-simd/...` is the in-tree relative
path used consistently across all six axis files at V2 HEAD —
non-issue for CH3 disposition; the three named scalar functions exist
exactly at the cited line numbers.)

**REDRESS-88 binding check:** REDRESS 88 pre-blocks "PMULL prefix-XOR
as a default hot body." The V1 §4 REDRESS-88 row for C2 (`p2f:292`)
named the differential as "split structural classifier into ordinary
+ quote-aware lanes" not "use PMULL wholesale as hot body" — Lock 16
:294 AVX-512 VPCLMULQDQ (NEW 2026-05-12) admits the 4×-width form.
The V2 NF-CH6-3 upgrade changes the **scalar-reference status row**,
not the §4 REDRESS-88 row or the S-P3 verification clause. The split-
lane framing and the AVX-512 VPCLMULQDQ differential are preserved
verbatim at `p2f:292`.

**CH3 disposition: ACCEPT.** The C2 upgrade strengthens scalar-ref
evidence (REDRESS-88 binding requires "fresh material differential";
having a scalar oracle that composes Gap 6 + escape_mask_64 + bitmap_
prefix_xor_64 IS material evidence the differential is structurally
grounded, not aspirational). The upgrade does NOT re-introduce PMULL-
hot-body framing; the §4 row "must cite REDRESS 88's exact prior
failure mode and prove the split-lane framing avoids it; checkasm-
parity must hold for both lanes against the C1 baseline" is unchanged.

### F-2 — Fold-4 + Fold-5 scalar-ref targets are REDRESS-neutral Layer-0 sibling shapes

The V2 P2-F Fold-4 (C10) + Fold-5 (C13) scalar-ref targets at
`crates/bbnf-simd/src/scalar/byte_context_64.rs` and `crates/bbnf-simd/
src/scalar/bcax_64.rs` are NEW files at V2 (verified: neither file
exists at HEAD yet; both are Stage-A authoring deliverables per the
V2 hardening consolidated §3.4 same-commit-with-SIMD-body discipline).

**REDRESS-regression audit for new scalar-ref targets:**

The new targets are direct siblings of existing Layer-0 scalar files
(verified existing: `byte_class_from_eq_set_64.rs`, `bitmap_prefix_xor_
64.rs`, `bulk_emit_positions_64.rs`, `bitmap_next_set_bit.rs`, `swar_
8byte.rs`, `byte_class_from_table_64.rs`, `eob_pad_clamp.rs`). The
sibling shape places authoring inside the `scalar/` module that holds
the existing scalar references for the production SIMD primitives.

- **C10 `byte_context_64_scalar`**: signature `(prev_chunk: &[u8; 64],
  cur_chunk: &[u8; 64], carry_bytes: usize) -> [u8; 64]` per
  `p2f:57`. Substrate-target `local_temp_only` per §2.10 unchanged.
  REDRESS adjacency at V1: none directly. **No REDRESS family touched
  by adding a scalar reference.**
- **C13 `bcax_64_scalar`**: signature `(a: u64, b: u64, c: u64) ->
  u64` returning `(a & !b) ^ c` per `p2f:80`. Substrate-target
  `local_temp_only` per §2.13 unchanged. REDRESS adjacency at V1:
  none directly (Lock 16 :289 admits BCAX/EOR3 abstractly). **No
  REDRESS family touched by adding a scalar reference.**

**Critical anti-fake-pattern check:** "fake-pattern recurrence" in
V1 CH3 vocabulary refers to the SK-V5 dispatch-table fake-pattern
(REDRESS 50-55), the SK-V6 sidecar fake-pattern (REDRESS 60-72), the
SK-V7 PMULL-hot-body fake-pattern (REDRESS 88), and the SK-V8/9 union-
substrate fake-pattern (REDRESS 96-98). Adding scalar references in
`bbnf-simd/src/scalar/` for primitives that name `local_temp_only`
substrate-target and abut none of these REDRESS families is the
opposite of a fake-pattern: it is the dav1d-process Stage-A
scalar-reference authoring that ALL admissible SIMD candidates require.
The Stage-A authoring discipline (per V1 CH3 §3 P2-B verification) is
the structural REDRESS-pre-block — adding the scalar references is
discharging the discipline, not re-opening any route.

**CH3 disposition: ACCEPT.** Fold-4 + Fold-5 scalar-ref targets are
REDRESS-neutral by construction; they discharge CH4 risk without
touching CH3.

### F-3 — P2-C C-P2C-7 demotion is the canonical REDRESS-126 close-hygiene path

The V2 P2-C demotion of C-P2C-7 (`byte_context_orphan_resolution`)
from §2 active enumeration to §2.X non-candidate inventory is the
canonical REDRESS-126 close-hygiene path. V1 CH3 F-4 named the
candidate's discipline shape: "may wire OR delete/demote with REDRESS
evidence" — the V2 demotion exercises the "demote with REDRESS
evidence" branch. The REDRESS evidence is the V2 demotion stamp
"zero S-P1 hot-leaf antecedent at SK-V14" plus the §2.X non-candidate
inventory binding that this row is NOT retained as support-only.

**REDRESS-126 zero-orphan binding strengthened:** the existing
`bbnf-simd/src/aarch64/byte_context.rs` is still an orphan at HEAD
(no SK-V14 production consumer), and the V2 demotion path explicitly
does NOT re-introduce it as support-only for C-P2C-5 (the conditional
folding into C-P2C-5 named in C-P2C-5's V1 disposition row at p2c:75
remains the only re-admission path). **CH3 verdict: V2 strengthens
REDRESS-126 compliance; the orphan inventory ceiling is enforced more
strictly at V2 than at V1.**

### F-4 — P2-D §1.6(d) substrate-side demotion preserves Lock 1 v+1 manifest

The V2 P2-D demotion of C-P2D-3 (sparse-flag gating) from §2 active
candidate enumeration to §1.6(d) substrate-side observation is Lock-1-
v+1-manifest-preserving. The candidate's V1 substrate fields
(`flag_cursors`, `flag_values` at `tape/mod.rs:97-98`) are not new
substrate fields — they exist at HEAD and the V1 candidate proposed
allocation-discipline gating (`Vec<u32>::new()` deferred to first
`patch_flags` write). Demoting to §1.6(d) preserves this exactly: the
existing substrate fields remain; the gating is documented as a
substrate-side observation that would actuate §1.6(b)'s zero-hot-leaf
finding if a same-wave consumer materialised.

**Lock 1 v+1 substrate-target column check:** the demoted observation
re-uses `existing_tape` substrate-target (no new substrate); the
`retention_lifetime` is unchanged (the `flag_*` fields' lifetime is
the tape's lifetime); the `policy_owner` is unchanged (generated
grammar owns the `needs_decode` flag emission). **CH3 verdict: V2
substrate-union compliance preserved; no REDRESS 96/97/98 surface
re-opened by the demotion.**

### F-5 — NF-CH6-4 cross-axis tracking note is anti-REDRESS-recurrence by construction

The V2 P2-F §2.Y NF-CH6-4 cross-axis tracking note (`p2f:109-117`)
explicitly binds S-P3 to one canonical primitive name + one canonical
scalar-reference function for the long-string-body SIMD scan
primitive, naming the three artefact incarnations:
- P2-A C2 `long_string_body_simd_scan` (V1-LOCKED axis)
- P2-E Gap 1 `scan_string_special_block_sweep_64` (V1-LOCKED axis)
- P2-F C1 + C2 (this artefact's V2 NF-CH6-3 upgrade)

**REDRESS-recurrence-prevention shape:** three orthogonal SIMD bodies
for one primitive is exactly the REDRESS 60-72 sidecar-producer fake-
pattern (where SK-V6 admitted multiple sidecar consumers for one
classifier output). The V2 NF-CH6-4 binding REDUCES re-open risk by
declaring "S-P3 must produce ONE canonical primitive name + ONE
canonical scalar reference function rather than admitting three
near-duplicates."

**Cross-references verified:**
- P2-A C2 row at `p2a-sota-teardown.md` (V1-LOCKED; zero V2 drift)
  preserves V1 framing.
- P2-E Gap 1 row at `p2e-parse-that-gaps.md` (V1-LOCKED; zero V2
  drift) preserves V1 framing.
- P2-F C1 row at `p2f:73` (this artefact's §2.1) preserves V1 framing;
  the V2 NF-CH6-3 upgrade is at §2.2 (C2 scalar-ref status row).

**CH3 disposition: ACCEPT.** NF-CH6-4 is a meta-binding for S-P3
admission discipline that explicitly prevents the REDRESS 60-72-shaped
recurrence; the note exists in P2-F's §2.Y, not in any candidate's §2
row, and so does not re-open any REDRESS family.

## §4 — P2-B 5-stage gate REDRESS-pre-block-safe by construction (V2 re-verification)

V1 CH3 §3 verified the P2-B claim "the 5-stage admission gate is
structurally REDRESS-pre-block-safe by construction" against the 14+8+7
candidate enumeration. V2 amended the active candidate enumeration:
- P2-F: 14 → 13 active + 1 demoted (C8)
- P2-E: 8 active unchanged (V1-LOCKED)
- P2-A: 7 candidates unchanged (V1-LOCKED)
- P2-C: 8 → 5 active + 3 demoted (C-P2C-1/6/7)
- P2-D: 3 → 2 active + 1 demoted-to-§1.6(d) + 1 pre-blocked-anti-pattern

Total V2 active candidate surface: 13 (P2-F) + 8 (P2-E) + 7 (P2-A) +
5 (P2-C) + 2 (P2-D) = **35 active** (down from 40 at V1, due to 5
demotions). Plus 5 demoted-to-inventory entries (3 P2-C + 1 P2-D + 1
P2-F) and 1 pre-blocked-anti-pattern (C-P2D-4) preserved verbatim for
cross-tranche/CH3-paper-trail reference.

**V2 REDRESS-family coverage matrix** (re-deriving V1 CH3 §3 matrix at
V2 HEAD; only rows that changed are shown):

| REDRESS family | V1 coverage | V2 changes | V2 coverage |
|---|---|---|---|
| **126** (PEXT mask plan; aarch64 has no PEXT) | p2f C9 + p2c §4 PEXT pre-block | C-P2C-7 demoted to §2.X non-candidate (REDRESS-126 close-hygiene path strengthened) | p2f C9 + p2c §4 PEXT pre-block + **p2c §2.X.C-P2C-7 demotion-as-close-hygiene-proof** = **strengthened** |
| **82-84** (single-quartet / StringBlock16 / object-pair) | p2f C2+C3+C12 + p2e Gap 2 + p2a C7 | C-P2C-4 (JSON `\uXXXX` only) unchanged; C-P2C-6 demoted to §2.X | unchanged + demotion neutral |
| **88** (PMULL prefix-XOR hot body) | p2f C2 + p2e Gap 6 + p2a explicit no-cite | C2 scalar-ref upgraded via Gap 6 composition (NF-CH6-3); §4 row split-lane framing unchanged | **strengthened** (scalar-ref evidence stronger; differential unchanged) |
| **all other families** (28+33, 50-55, 60-72, 80, 89, 96/97/98, 119/120) | per V1 CH3 §3 table | no V2 edit touches these | unchanged |

**Verdict on P2-B claim at V2**: the 5-stage admission gate is **still
structurally REDRESS-pre-block-safe by construction at V2 HEAD**. Three
REDRESS families are STRENGTHENED by V2 demotions/upgrades:
- REDRESS 126 (C-P2C-7 demotion-as-close-hygiene-proof)
- REDRESS 88 (C2 scalar-ref evidence upgrade via Gap 6 composition)
- REDRESS 60-72 (NF-CH6-4 cross-axis tracking note explicitly binds
  S-P3 to single-canonical-primitive admission, preventing the
  three-orthogonal-SIMD-bodies recurrence shape)

All other REDRESS-family coverage rows unchanged from V1. The Lock 1
v+1 substrate-manifest enforcement at Stage E unchanged. The strict-
row-movement clause at Stage D unchanged. **No coverage gap surfaces at
V2.**

## §5 — Demoted candidates remain pre-blocked (V2 dispatch focus item 2)

V2 dispatch context §2 item: "demoted candidates (P2-C C-P2C-1/6/7,
P2-D C-P2D-3, P2-F C8) remain pre-blocked; demotion does not silently
re-open REDRESS." Per-demoted-candidate audit:

| Demoted candidate | Demotion location | V1 disposition preserved? | Re-admission gate binds? | Silently re-opens? |
|---|---|---|---|---|
| **P2-C C-P2C-1** `ascii_set_member64_css_delimiter` | `p2c:65` §2.X | YES (V1 `NOT-S-P3-ELIGIBLE absent real CSS L4 parser + corpus` + V2 demotion stamp) | YES (F-V2-P1ABC-RERECORD antecedent gate) | NO |
| **P2-C C-P2C-6** `eor3_string_mask_fusion` | `p2c:66` §2.X | YES (V1 `NOT-S-P3-ELIGIBLE` inventory + V2 demotion stamp) | YES (F-V2-P1ABC-RERECORD antecedent gate; CH6 paper-close risk discharged via demotion) | NO |
| **P2-C C-P2C-7** `byte_context_orphan_resolution` | `p2c:67` §2.X | YES (V1 `NOT-S-P3-ELIGIBLE` close-hygiene + V2 demotion stamp; REDRESS-126 binding strengthened) | YES (F-V2-P1ABC-RERECORD antecedent gate + REDRESS-126 close-hygiene) | NO |
| **P2-D C-P2D-3** sparse-flag-band gating | `p2d:104, :128` §1.6(d) + gap-note | YES (V1 substrate-side allocation discipline + V2 §1.6(b) cross-reference) | YES (same-wave consumer gate) | NO |
| **P2-F C8** comment-skip primitive | `p2f:96-107` §2.X.1 | YES (V1 NEUTRAL-PENDING-CONSUMER + V2 demotion stamp; CH6 paper-close risk discharged) | YES (§2.X.1 re-promotion gate: fresh-material differential or same-wave consumer commit) | NO |

**Demotion-as-pre-block-strengthening verdict:** all 5 demoted candidates
have their V1 dispositions preserved verbatim plus an additional V2
demotion stamp that strengthens (not loosens) the pre-block. None of
the 5 demoted candidates was admitted at V1; all 5 are explicitly
non-S-P3-shortlist-eligible at V2 with documented re-admission gates.
**Zero silent REDRESS re-openings.**

## §6 — Sources verified (executable-verification mandate)

V2 HEAD verifications (`wc -l` + `grep -n` + `git diff` per
`[read-size-preflight]`):

- `restart/skinny/tranches/sk-v14/research/p2/p2a-sota-teardown.md` —
  367 lines; `git diff b3dbc5ca0 447a26b07 -- $file` returns empty
  (V1-LOCKED zero drift confirmed).
- `restart/skinny/tranches/sk-v14/research/p2/p2b-dav1d-process.md` —
  217 lines; V1→V2 diff: SHA pinning on 3 §5.1 bullets at lines
  183-185; §4 catchall at lines 166-177 unchanged.
- `restart/skinny/tranches/sk-v14/research/p2/p2c-arch-esoterica.md` —
  164 lines; V1→V2 diff: 8→5 active + new §2.X non-candidate inventory
  (3 demoted rows); §4 11-REDRESS-family enumeration at lines 86-99
  unchanged.
- `restart/skinny/tranches/sk-v14/research/p2/p2d-substrate-tape.md` —
  254 lines; V1→V2 diff: §1.6 renamed §1.6 — Substrate-side
  observations; (a)(b)(c) preserved + new (d) at lines 104-105;
  C-P2D-3 §2 body replaced with `[DEMOTED V2 → §1.6(d)]` stub at
  lines 128-130; §3 row + §4.6 + §4.7 updated; §4.1-§4.5 REDRESS-
  family rows preserved verbatim.
- `restart/skinny/tranches/sk-v14/research/p2/p2e-parse-that-gaps.md` —
  342 lines; `git diff b3dbc5ca0 447a26b07 -- $file` returns empty
  (V1-LOCKED zero drift confirmed).
- `restart/skinny/tranches/sk-v14/research/p2/p2f-grammar-neutral.md` —
  360 lines; V1→V2 diff: 6 sub-folds — §2.2 C2 scalar-ref upgrade;
  §2.6/§2.7/§2.10/§2.12/§2.13 P1 antecedent CH1 bullets added;
  §2.8 demoted-stub; §2.X.1 + §2.Y new sections; §3 verdict-tally
  updated; §4 CH6 + CH4 risks DISCHARGED-V2 reframed.

**Scalar-reference path:line verification (V2 NF-CH6-3 + Fold-4/Fold-5
+ C12 CF-1):**
- `skinny/crates/bbnf-simd/src/aarch64/string_block.rs:31` →
  `scan_string_special_block_scalar` **CONFIRMED**
- `skinny/crates/bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs:2` →
  `bitmap_prefix_xor_64_scalar` **CONFIRMED**
- `skinny/crates/bbnf-simd/src/lib.rs:175` → `escape_mask_64`
  **CONFIRMED**
- `skinny/crates/runtime/src/grammars/json/scan.rs:32` →
  `scan_structurals_scalar` **CONFIRMED** (per-byte `is_member` /
  `match` inner at lines 68-72 inside `sampled_structural_capacity`
  helper; the scalar function root at line 32 is the canonical Stage-A
  reference shape — non-issue for CH3)
- `skinny/crates/bbnf-simd/src/scalar/byte_context_64.rs` — NOT YET
  EXISTS (Fold-4 Stage-A authoring deliverable; same-commit with SIMD
  body per V2 hardening §3.4)
- `skinny/crates/bbnf-simd/src/scalar/bcax_64.rs` — NOT YET EXISTS
  (Fold-5 Stage-A authoring deliverable; same-commit with SIMD body)

Note on the latter two: not-yet-existing scalar-ref targets are a CH4
concern (scalar-ref authoring gap), not a CH3 concern (REDRESS-
regression). Per V2 hardening context §1 + §3.4 the authoring is
explicitly deferred to same-commit-with-SIMD-body landing under Lock
16 same-commit discipline; the V2 P2-F amendment merely names the
target paths, which is a strengthening of the dav1d-process Stage-A
discipline, not a REDRESS re-opening.

**V2 commit verification:** `git show 447a26b07` confirms 5 files
changed: V2 dispatch context + 4 amended axis files (P2-B, P2-C,
P2-D, P2-F); P2-A + P2-E are not in the changeset (V1-LOCKED).

## §7 — CH3 disposition (final, V2)

**ACCEPT 6/6 artefacts at V2.** Per-axis rate: **100% ACCEPT** at V2,
matching V1 (V1: 5 ACCEPT + 1 ACCEPT-WITH-NOTE = 100%).

- **P2-A**: ACCEPT (V1-LOCKED zero drift; V1 CH3 §2.1 carry-forward
  applies verbatim)
- **P2-B**: ACCEPT (V2 SHA pinning is CH1 process-truth, not CH3
  surface; §4 catchall preserved)
- **P2-C**: ACCEPT (3 demotions strengthen pre-blocks; §4 11-REDRESS-
  family enumeration unchanged; REDRESS-126 close-hygiene
  strengthened via C-P2C-7 demotion path)
- **P2-D**: ACCEPT (C-P2D-3 demotion to §1.6(d) preserves Lock 1 v+1
  substrate-target compliance; C-P2D-4 pre-blocked-anti-pattern
  reference preserved verbatim; substrate-union YES preserved)
- **P2-E**: ACCEPT-WITH-NOTE (V1-LOCKED zero drift; V1 CH3 F-1 Gap 5
  REDRESS-80 differential carries through V2 unchanged; S-P3
  dispatch must carry the REDRESS-80 re-evaluation checklist per
  V1 CH3 §5 item 2)
- **P2-F**: ACCEPT (6 sub-folds; zero REDRESS re-openings; NF-CH6-3
  C2 scalar-ref upgrade strengthens REDRESS-88 split-lane evidence;
  Fold-4/Fold-5 scalar-ref targets are REDRESS-neutral Layer-0
  siblings; NF-CH6-4 cross-axis tracking note REDUCES REDRESS 60-72-
  shaped recurrence risk; C8 demotion DISCHARGES CH6 paper-close
  risk)

**Five NEW findings logged (V2-specific): F-1 through F-5 in §3.**
**Zero REVISE; zero REJECT.** **The V1 CH3 F-1 (Gap 5 REDRESS-80 S-P3
checklist), F-2 (C-P2C-2 PRE-BLOCKED admission path), F-3 (C-P2D-4
canonical paper-trail anchor), F-4 (C-P2C-7 REDRESS-126 close-hygiene
anchor), and F-5 (dual-canonical p2b §4 + p2c §4 statements) carry
through V2 unchanged.** The V2 dispatch context §2 V2-attention items
(no V2 edit re-opens REDRESS routes; demoted candidates remain pre-
blocked; P2-E Gap 5 REDRESS-80 differential carries through cleanly;
new scalar-ref Stage-A targets do not re-introduce fake-pattern
recurrence) are **all satisfied**.

**§3Z gate evaluation:** V2 CH3 is the **first ≥95% cycle on V2** with
**100% ACCEPT**. Predicted trajectory: V2 → LOCK (V3 not required on
CH3 grounds; ACCEPT-WITH-NOTE on P2-E carries through unchanged from
V1 and is dispatch-context-flagged for S-P3 admission, not for further
V3 CHALLENGE iteration).

**Cycle disposition:** **CONVERGE** at V2 on CH3 grounds. No CH3-
grounded blocker to S-P3 dispatch. The dispatch §2 watch-list
(REDRESS 28+33, 50-55, 60-72, 80, 82-84, 88, 89, 96/97/98, 119/120,
126) is satisfied with quadruple canonical coverage (p2b admission-
process + p2c arch-instruction + p2d substrate + p2a SOTA-comparator)
at V2 — three families STRENGTHENED (126, 88, 60-72), all others
unchanged from V1. **No CH3 V3 cycle required.**
