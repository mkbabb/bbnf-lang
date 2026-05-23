# SK-V14 S-P2 V3 CH1: Correctness (confirming cycle)

Pass: S-P2 CHALLENGE V3 (confirming cycle per `ORCHESTRATOR.md §3Z`).
Date: 2026-05-23.
Lens: CH1 (CORRECTNESS) — per `restart/prompts/skinny/PASS-2-RESEARCH.md:95-100`.
Disposition vocabulary: ACCEPT / REVISE / REJECT, per artefact + per
candidate. Header verdict per artefact is the maximum-severity disposition
across the artefact's candidate pool plus its prose claims (SOTA-comparator
strictness, ISA citation, P1 antecedent grounding).

V2 baseline (per `research/p2/hardening/V2/CH1.md` + V2 consolidated
aggregator): artefact-ACCEPT 6/6 (100 %); candidate-ACCEPT 38/38 (100 %);
**ACCEPT-WITH-NOTE** at §2.4 / §4.4 / §5.1 for trailing per-row Fold-N
attribution verb-tense imprecision at `p2f:164` (C10) + `p2f:197` (C13)
— "Authoring landed as Fold-N V2 deliverable" while V2 commit `447a26b07`
did not execute the scalar-reference authoring (Stage-A files
`byte_context_64.rs` + `bcax_64.rs` not at HEAD). V3 fold scope per V3
CHALLENGE-CONTEXT §1: P2-F micro-amend (2 cells only); P2-A/B/C/D/E
V2-LOCKED.

---

## §0 — V3 disposition focus restated

Per V3 CHALLENGE-CONTEXT §2 CH1 (`research/p2/hardening/V3/CHALLENGE-CONTEXT.md:24`):

1. **Confirm V3 verb-tense fix at `p2f:164` (C10) + `p2f:197` (C13)
   discharges V2 ACCEPT-WITH-NOTE** — the "landed"→"queued for S-P3
   same-wave Lock 16 same-commit admission … function body lands
   same-commit with SIMD body at S-P3" reframing.
2. **Verify zero V3 drift on P2-A/B/C/D/E** via
   `git diff 4c70b6f19..ebe84954b -- <axis file>` returning empty
   per axis.
3. **Re-execute executable verification on all V2 cites:** `byte_context_64.rs`
   + `bcax_64.rs` Stage-A NOT-PRESENT at HEAD; `scan_structurals_scalar`
   live at `scan.rs:32`; sibling templates `byte_class_from_eq_set_64.rs`
   + `bitmap_prefix_xor_64.rs` present.
4. **§3Z second-consecutive-cycle accounting:** V1 81.6/33.3 →
   V2 100 % → V3 expected 100 % completes the "≥95 % × 2 cycles"
   convergence rule for CH1 → cohort LOCK eligibility.

---

## §1 — Per-artefact verdict summary at V3

| Artefact | V3 cycle scope | V2 verdict | V3 verdict | Headline |
|---|---|---|---|---|
| `p2a-sota-teardown.md` | V2-LOCKED (367 L; no V3 edits) | ACCEPT (no drift) | **ACCEPT (no drift)** | Zero V3 edits confirmed via `git diff 4c70b6f19..ebe84954b -- p2a-sota-teardown.md` (empty). Gold-standard four-HEAD pinning + strict-vs-strict comparator discipline at §1.4 + §5.3 carries through V3 verbatim. |
| `p2b-dav1d-process.md` | V2-LOCKED (217 L; no V3 edits) | ACCEPT (Fold-1) | **ACCEPT (no drift)** | Zero V3 edits confirmed via `git diff 4c70b6f19..ebe84954b -- p2b-dav1d-process.md` (empty). FFmpeg + dav1d HEAD pins at §5.1 lines 183-185 carry through V3 verbatim with §5.3 register-correction attribution intact. |
| `p2c-arch-esoterica.md` | V2-LOCKED (164 L; no V3 edits) | ACCEPT (Fold-2) | **ACCEPT (no drift)** | Zero V3 edits confirmed via `git diff 4c70b6f19..ebe84954b -- p2c-arch-esoterica.md` (empty). §2.X non-candidate inventory (C-P2C-1/-6/-7) with explicit demotion stamps + full technical content preservation carries through V3 verbatim. |
| `p2d-substrate-tape.md` | V2-LOCKED (254 L; no V3 edits) | ACCEPT (Fold-2) | **ACCEPT (no drift)** | Zero V3 edits confirmed via `git diff 4c70b6f19..ebe84954b -- p2d-substrate-tape.md` (empty). C-P2D-3 demoted to §1.6(d) with identifier-stub continuity at §2 carries through V3 verbatim. |
| `p2e-parse-that-gaps.md` | V1+V2-LOCKED (342 L; no V3 edits) | ACCEPT (no drift) | **ACCEPT (no drift)** | Zero V3 edits confirmed via `git diff 4c70b6f19..ebe84954b -- p2e-parse-that-gaps.md` (empty). Layer-1-primitive-discipline exemplar carries through V3 verbatim. |
| `p2f-grammar-neutral.md` | V3 amended (2 cells — §2.10 C10 + §2.13 C13 verb-tense polish) | ACCEPT (with NOTE) | **ACCEPT** | V3 micro-fold at `p2f:164` (C10) + `p2f:197` (C13) replaces "Authoring landed as Fold-N V2 deliverable per HARDENING-S-P2-V1-CONSOLIDATED §3.4." with "Authoring queued for S-P3 same-wave Lock 16 same-commit admission per HARDENING-S-P2-V1-CONSOLIDATED §3.4 Fold-N; function body lands same-commit with SIMD body at S-P3." Discharges V2 §2.4 / §4.4 / §5.1 ACCEPT-WITH-NOTE convergently. All other content unchanged. |

**Aggregate V3 ACCEPT rate: 6/6 artefacts = 100 %.**
**Per-candidate aggregate V3: 38/38 = 100 % (zero new REVISEs; the
V2 ACCEPT-WITH-NOTE is now fully discharged to ACCEPT).**

Cycle disposition: **ACCEPT.**

---

## §2 — V3 fold-discharge audit per V2 ACCEPT-WITH-NOTE

### §2.1 — V3 micro-fold at `p2f:164` (C10) — DISCHARGED

**V2 finding** (V2 CH1 §2.4 + §4.4): the per-row trailing Fold-N
attribution at `p2f:164` read "Authoring landed as Fold-4 V2 deliverable
per HARDENING-S-P2-V1-CONSOLIDATED §3.4" while V2 commit `447a26b07` did
not execute the scalar-reference authoring (only docs amendments). The
verb "landed" was literally untrue at V2 cycle close.

**V3 evidence (executable diff):**

```
git diff 4c70b6f19..ebe84954b -- restart/skinny/tranches/sk-v14/research/p2/p2f-grammar-neutral.md
```

yields exactly two unified-diff hunks (lines 161-167 + lines 194-200 in
the new file). The C10 hunk at line 164:

```
-... Authoring landed as Fold-4 V2 deliverable per HARDENING-S-P2-V1-CONSOLIDATED §3.4.
+... Authoring queued for S-P3 same-wave Lock 16 same-commit admission per HARDENING-S-P2-V1-CONSOLIDATED §3.4 Fold-4; function body lands same-commit with SIMD body at S-P3.
```

**V3 cite framing audit at `p2f:164` (verified `Read p2f:158-169`):**

- Status-prefix preserved verbatim: "**Scalar-ref status**: required …
  **Scalar-reference target path:line** (Stage-A authoring under
  same-wave Lock 16 same-commit discipline): `crates/bbnf-simd/src/scalar/byte_context_64.rs`
  — `byte_context_64_scalar(prev_chunk: &[u8; 64], cur_chunk: &[u8; 64], carry_bytes: usize) -> [u8; 64]`
  producing the same cross-chunk byte-context as the candidate SIMD
  primitive via byte-by-byte loop with no chunk boundary (sibling of
  existing `crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs:1`
  shape)."
- Trailing attribution V3-corrected: "Authoring **queued for S-P3
  same-wave Lock 16 same-commit admission** per HARDENING-S-P2-V1-CONSOLIDATED
  §3.4 Fold-4; **function body lands same-commit with SIMD body at S-P3**."
- The "function body lands same-commit with SIMD body at S-P3" clause
  is explicit about the deferral target (S-P3) AND the same-commit
  bundling discipline (the scalar reference and the SIMD body land
  together, per Lock 16 same-commit-pairing rule). This is strictly
  more precise than V2 because it surfaces the bundling discipline
  inline at the per-row attribution.

**Verdict:** ACCEPT (V3 verb-tense fix discharges V2 §2.4 ACCEPT-WITH-NOTE
on C10 cleanly; status-prefix Stage-A framing preserved verbatim).

### §2.2 — V3 micro-fold at `p2f:197` (C13) — DISCHARGED

**V2 finding** (V2 CH1 §2.4 + §4.4): identical verb-tense imprecision
on C13 at `p2f:197` ("Authoring landed as Fold-5 V2 deliverable…").

**V3 evidence (executable diff):** the second hunk in the V3 diff
matches C13 at line 197:

```
-... Authoring landed as Fold-5 V2 deliverable per HARDENING-S-P2-V1-CONSOLIDATED §3.4.
+... Authoring queued for S-P3 same-wave Lock 16 same-commit admission per HARDENING-S-P2-V1-CONSOLIDATED §3.4 Fold-5; function body lands same-commit with SIMD body at S-P3.
```

**V3 cite framing audit at `p2f:197` (verified `Read p2f:191-202`):**

- Status-prefix preserved verbatim: "**Scalar-ref status**: required
  (scalar reference is the trivial 2-op form `(a & !b) ^ c`).
  **Scalar-reference target path:line** (Stage-A authoring under Lock 16
  same-commit discipline): `crates/bbnf-simd/src/scalar/bcax_64.rs` —
  `bcax_64_scalar(a: u64, b: u64, c: u64) -> u64` returning `(a & !b) ^ c`
  over u8x16 / u64 masks; sibling of existing
  `crates/bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs:1` shape."
- Trailing attribution V3-corrected: "Authoring **queued for S-P3
  same-wave Lock 16 same-commit admission** per HARDENING-S-P2-V1-CONSOLIDATED
  §3.4 Fold-5; **function body lands same-commit with SIMD body at S-P3**."
- Same-commit bundling discipline surfaced inline (the BCAX primitive
  pairs to `vbcaxq_u8` + `veor3q_u8` per Lock 16 :289; the V3 wording
  preserves that pairing requirement at the per-row attribution).

**Verdict:** ACCEPT (V3 verb-tense fix discharges V2 §2.4 ACCEPT-WITH-NOTE
on C13 cleanly; status-prefix Stage-A framing preserved verbatim).

### §2.3 — Source-anchor lines at `p2f:167` (C10) + `p2f:200` (C13) — UNCHANGED, CORRECTLY FRAMED

Per V2 CH1 §2.4, the "Source anchor" line on each cell reads "N/A (new
primitive in `bbnf-simd/src/aarch64/`); scalar reference target
`crates/bbnf-simd/src/scalar/byte_context_64.rs` per Fold-4" (C10) /
"scalar reference target `crates/bbnf-simd/src/scalar/bcax_64.rs` per
Fold-5" (C13). The V3 diff does NOT touch these lines — they retain
their V2 wording, which is CH1-correct (they say "target", not "landed",
already). No V3 cosmetic drift required; the V2 wording on the source-
anchor line was always accurate. Only the per-row trailing Fold-N
attribution carried the imprecise verb tense.

**Verdict:** ACCEPT (V3 diff scope is surgical; no collateral edits on
the adjacent source-anchor lines, which were already CH1-correct).

### §2.4 — V2-LOCKED axis drift audit (P2-A + P2-B + P2-C + P2-D + P2-E) — ZERO DRIFT CONFIRMED

**Executable verification:**

```
git diff 4c70b6f19..ebe84954b -- \
  restart/skinny/tranches/sk-v14/research/p2/p2a-sota-teardown.md \
  restart/skinny/tranches/sk-v14/research/p2/p2b-dav1d-process.md \
  restart/skinny/tranches/sk-v14/research/p2/p2c-arch-esoterica.md \
  restart/skinny/tranches/sk-v14/research/p2/p2d-substrate-tape.md \
  restart/skinny/tranches/sk-v14/research/p2/p2e-parse-that-gaps.md
```

returns **empty** (zero bytes of diff output). Zero V2→V3 edits across
the five V2-LOCKED axes.

`wc -l` at HEAD (commit `ebe84954b`):

- `p2a-sota-teardown.md` — 367 lines (V2 367 — match)
- `p2b-dav1d-process.md` — 217 lines (V2 217 — match)
- `p2c-arch-esoterica.md` — 164 lines (V2 164 — match)
- `p2d-substrate-tape.md` — 254 lines (V2 254 — match)
- `p2e-parse-that-gaps.md` — 342 lines (V2 342 — match)
- `p2f-grammar-neutral.md` — 360 lines (V2 360 — match; V3 edits are
  in-place line replacements with no net line-count change)

**Verdict:** ACCEPT (zero V2→V3 drift on the five V2-LOCKED axes; V2
ACCEPT 6/6 + 38/38 carries verbatim into V3 with the §2.4 ACCEPT-WITH-NOTE
now fully discharged on C10 + C13).

### §2.5 — Re-execution of V2 executable cites — ALL HOLD

**`scan_structurals_scalar` at `runtime/src/grammars/json/scan.rs:32`
(C12 CH4-ACCEPT scalar reference):**

```
grep -n "scan_structurals_scalar" skinny/crates/runtime/src/grammars/json/scan.rs
```

returns:
- line 29: call site
- **line 32: `pub fn scan_structurals_scalar(input: &[u8]) -> StructuralIndex {`** — definition site, matches the V2 cite verbatim
- line 39: call site
- line 280: test-module re-export
- line 297: test call site

The V2 CH1 §2.3 + V2 P2-F §2.12 ACCEPT reframing for C12 holds at V3.

**Stage-A scalar-ref targets NOT PRESENT at HEAD (C10 + C13):**

```
ls skinny/crates/bbnf-simd/src/scalar/byte_context_64.rs \
   skinny/crates/bbnf-simd/src/scalar/bcax_64.rs
```

returns:
```
ls: skinny/crates/bbnf-simd/src/scalar/bcax_64.rs: No such file or directory
ls: skinny/crates/bbnf-simd/src/scalar/byte_context_64.rs: No such file or directory
```

Stage-A authoring targets NOT-PRESENT at V3 HEAD — corroborates the V3
verb-tense correction ("queued for S-P3" rather than "landed").

**Live `scalar/` directory contents at V3 HEAD:**

```
ls skinny/crates/bbnf-simd/src/scalar/
```

returns 8 files (identical to V2 census):
- `bitmap_next_set_bit.rs`
- `bitmap_prefix_xor_64.rs` ← sibling template named in §2.13 C13
- `bulk_emit_positions_64.rs`
- `byte_class_from_eq_set_64.rs` ← sibling template named in §2.10 C10
- `byte_class_from_table_64.rs`
- `eob_pad_clamp.rs`
- `mod.rs`
- `swar_8byte.rs`

Both sibling templates explicitly named in the V3 P2-F §2.10 / §2.13
cells are PRESENT at HEAD. The shape-claim ("sibling of existing
`crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs:1` shape" /
"sibling of existing `crates/bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs:1`
shape") is verifiable at HEAD.

**Verdict:** ACCEPT (all V2 executable cites hold at V3 HEAD verbatim;
the V3 verb-tense correction is the only V2→V3 change and it is
disposition-language refinement only — no source-anchor or path:line
cite changed).

---

## §3 — V3 per-candidate verdict table

V3 candidate pool (unchanged from V2 — verb-tense fold does not alter
candidate counts):

- **P2-A:** 7 candidates (C1–C7) — ACCEPT 7/7 (no V3 edits).
- **P2-B:** 5 process stages (§2.A–§2.E) — ACCEPT 5/5 (no V3 edits).
- **P2-C:** 5 active (C-P2C-2/-3/-4/-5/-8) + 3 demoted (C-P2C-1/-6/-7)
  — ACCEPT 5/5 active; 3/3 demoted carry full technical content +
  explicit disposition stamps (no V3 edits).
- **P2-D:** 2 active (C-P2D-1, C-P2D-2) + 1 demoted (C-P2D-3 at §1.6(d))
  + 1 pre-blocked (C-P2D-4) — ACCEPT 2/2 active (no V3 edits).
- **P2-E:** 9 gaps (Gap 1–8 + Gap 7.5) — ACCEPT 9/9 (no V3 edits).
- **P2-F:** 13 active (C1–C7, C9–C14) + 1 demoted (C8 at §2.X.1) —
  ACCEPT 13/13 active. C10 + C13 trailing per-row Fold-N attribution
  verb-tense corrected at V3; status-prefix Stage-A framing preserved
  verbatim. C12 CH4-ACCEPT reframing intact at `p2f:184` (no V3 edits
  to §2.12); 1/1 demoted carries full technical content + explicit
  disposition stamp + re-promotion gate.

**V3 candidate ACCEPT count:** 7 (P2-A) + 5 (P2-B stages) + 5 (P2-C
active) + 2 (P2-D active) + 9 (P2-E) + 13 (P2-F active) = **41 active +
process-stage rows ACCEPT**. Plus 5 demoted candidates (P2-C C-P2C-1/-6/-7;
P2-D C-P2D-3; P2-F C8) — all correctly framed as non-candidates with
full technical content + explicit disposition stamps. Plus 1
documented-as-pre-blocked (P2-D C-P2D-4) carrying explicit anti-pattern
framing.

**V2 ACCEPT-WITH-NOTE at C10 + C13 fully discharged at V3 → strict
ACCEPT.**

---

## §4 — CH1 cross-cuts at V3

### §4.1 — F-V2-P1ABC-RERECORD packet remains the CH2/CH4 dual-gating prerequisite

The V2 cross-cut at V2 CH1 §4.1 (F-V2-P1ABC-RERECORD as S-P3 Stage 0
deliverable) holds at V3 unchanged. The V3 fold is a 2-cell verb-tense
polish; it does NOT deliver the rerun packet (the rerun is the S-P3
Stage 0 deliverable per V1 consolidator §2.1 binding entry). Every
indirect-/envelope-antecedent disposition stamp at V3 (C6/C7/C10/C12/C13
+ P2-A C6 + P2-C C-P2C-3/-8 + P2-E Gap 1/3/4) continues to carry the
"F-V2-P1ABC-RERECORD" identifier verbatim — the cohort is internally
consistent on the cross-axis 12-candidate dependency list.

### §4.2 — `sonic_rs::from_slice::<Value>` audit-falsification holds at V3

V2 CH1 §4.2 finding: every artefact in the cohort that names a sonic-rs
anchor cites the strict struct-deser path, not `from_slice::<Value>`
(eager-DOM). V3 audit: zero new sonic-rs anchors introduced by V3 edits
(the V3 diff is exactly 2 line replacements at `p2f:164` + `p2f:197`,
neither of which touches sonic-rs anchors). The V2 strict-vs-strict
discipline carries through V3 verbatim.

### §4.3 — ISA citations carry through V3 verbatim

V2 CH1 §4.3 finding: aarch64 ISA claims cite Arm ACLE / Arm Neon
Intrinsics Reference / Apple Silicon sysctl / Lock 16 lock-prose; x86
secondary citations cite Intel Intrinsics Guide / WikiChip /
BranchFree.org. V3 audit: zero new ISA claims introduced (the V3 verb-
tense edits do NOT touch the `**Arch**` lines at `p2f:165` (C10 — `vextq_u8`
per Lock 16 :285) or `p2f:198` (C13 — `vbcaxq_u8` + `veor3q_u8` per
Lock 16 :289)). The V2 ISA-citation discipline carries through V3
verbatim.

### §4.4 — V2 §4.4 verb-tense ACCEPT-WITH-NOTE — FULLY DISCHARGED

Per §2.1 + §2.2 above, the V3 micro-fold replaces the V2 "Authoring
landed as Fold-N V2 deliverable" wording with "Authoring queued for
S-P3 same-wave Lock 16 same-commit admission per
HARDENING-S-P2-V1-CONSOLIDATED §3.4 Fold-N; function body lands
same-commit with SIMD body at S-P3" on both C10 + C13. The V2 §4.4
ACCEPT-WITH-NOTE is fully discharged at V3 → strict ACCEPT.

The V3 wording carries an additional precision improvement beyond
just verb-tense: the explicit "function body lands same-commit with
SIMD body at S-P3" clause surfaces the Lock 16 same-commit-pairing
discipline inline at the per-row attribution. This is a CH1-positive
refinement (the bundling discipline is now visible at the candidate
row, not buried in the consolidator).

### §4.5 — V3 cycle disposition aggregate

V3 fold-discharge audit (per §2.1–§2.5 above):

- V3 micro-fold C10 `p2f:164` ("landed"→"queued for S-P3") — DISCHARGED.
- V3 micro-fold C13 `p2f:197` ("landed"→"queued for S-P3") — DISCHARGED.
- V3 collateral-edit audit (source-anchor lines `p2f:167` + `p2f:200`
  unchanged; already CH1-correct at V2) — CONFIRMED.
- V2-LOCKED axes (P2-A + P2-B + P2-C + P2-D + P2-E) — ZERO V3 DRIFT
  CONFIRMED via empty `git diff 4c70b6f19..ebe84954b -- <axis>`.
- All V2 executable cites re-verified at V3 HEAD: `scan_structurals_scalar`
  at `scan.rs:32` LIVE; `byte_context_64.rs` + `bcax_64.rs` NOT-PRESENT
  (Stage-A targets); 8-file `scalar/` directory inventory unchanged;
  both sibling templates PRESENT.

V3 aggregate ACCEPT rate per CH1:
- **Artefact-ACCEPT: 6/6 = 100 %.**
- **Candidate-ACCEPT: 38/38 = 100 %** (V2 ACCEPT-WITH-NOTE on C10 + C13
  fully discharged to strict ACCEPT).
- **Per-cohort discharge rate: 1/1 = 100 %** on V2 orphan ACCEPT-WITH-NOTE
  (the only V2 CH1 carry-forward into V3).

V3 satisfies the ≥95 % §3Z threshold for CH1 with strict 100 % ACCEPT
across all artefacts + candidates.

---

## §5 — §3Z two-cycle-stable convergence + cohort LOCK accounting

Per `ORCHESTRATOR.md §3W + §3Z` two-cycle-stable convergence threshold
(≥95 % ACCEPT for two consecutive cycles + zero orphan REVISEs):

| Cycle | Artefact-ACCEPT | Candidate-ACCEPT | Orphan REVISEs | ≥95 % threshold |
|---|---|---|---|---|
| V1 | 2/6 = 33.3 % | 31/38 = 81.6 % | 7 (3 fold packets) | NOT MET |
| V2 | 6/6 = 100 % | 38/38 = 100 % | 0 (ACCEPT-WITH-NOTE only) | MET (first cycle) |
| V3 | 6/6 = 100 % | 38/38 = 100 % | 0 (ACCEPT-WITH-NOTE discharged) | **MET (second consecutive)** |

**V3 closes the §3Z two-consecutive-cycle convergence rule for CH1.**

Per V3 CHALLENGE-CONTEXT §0/§3 + cohort-wide accounting:

- V2 cohort §3Z LOCK status (per V2 CHALLENGE-CONTEXT line 5): 5/7 lenses
  achieved 2-cycle LOCK (CH2/CH3/CH5/CH6/CH7).
- V3 closes CH1 (this report) + CH4 (parallel lens) — the remaining 2/7.
- V3 cohort-wide §3Z LOCK is achievable PROVIDED CH4 also confirms its
  second consecutive ≥95 % cycle.

**CH1 cohort LOCK confirmation: ACHIEVED at V3** (subject to aggregator
audit that CH4 V3 also confirms its second consecutive ≥95 % cycle).
The §3Z gate condition for CH1 alone is satisfied.

**Predicted close path:** V2 → V3 → **LOCK on CH1**. No further CH1
folds required. S-P3 dispatch gate opens for CH1 once the cohort-wide
§3Z aggregator audit completes (CH4 V3 + cohort consolidated commit).

---

## §6 — CH1 final disposition + V3 ACCEPT rate

**Artefact ACCEPT rate at V3: 6 / 6 = 100 %.**
**Candidate ACCEPT rate at V3: 38 / 38 = 100 %.**

V2 → V3 net: artefact ACCEPT 100 % → 100 % (no regression); candidate
ACCEPT 100 % → 100 % (no regression); the V2 ACCEPT-WITH-NOTE on C10 +
C13 is fully discharged to strict ACCEPT via the V3 verb-tense micro-fold.

**CH1 V3 cycle disposition: ACCEPT.**

**§3Z two-cycle-stable convergence: MET (V2 first cycle ≥95 %; V3 second
consecutive cycle ≥95 %; zero orphan REVISEs across both cycles).**

**Cohort LOCK eligibility (CH1): ACHIEVED.**

---

## §7 — Sources (CH1 V3 lens-internal citations)

### §7.1 — Authority

- `restart/prompts/skinny/PASS-2-RESEARCH.md:95-100` (CH1 contract)
- `restart/prompts/ORCHESTRATOR.md §3W + §3Z` (lens registry; convergence)
- `restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md §CH7` (overfit cross-cut)
- `restart/skinny/tranches/sk-v14/research/p2/S-P2-DISPATCH-CONTEXT.md
  §0-§5` (S-P2 binding)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V3/CHALLENGE-CONTEXT.md
  §0-§4` (V3 dispatch binding; confirming cycle)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V2/CH1.md` (V2
  CH1 carry-forward — ACCEPT-WITH-NOTE source at §2.4 + §4.4 + §5.1)
- `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md`
  (V2 aggregator + V3 fold-packet authority)
- `restart/locks/LOCKS.md` (Lock 1 substrate-union; Lock 14
  grammar-neutrality; Lock 15 i-cache budget; Lock 16 SIMD/ASM allowlist)

### §7.2 — Artefacts under review at V3 cycle (HEAD = commit `ebe84954b1a6c31bb6183ca8f5e68d88647d9df7`)

- `restart/skinny/tranches/sk-v14/research/p2/p2a-sota-teardown.md`
  (367 L; V2-LOCKED; zero V3 edits per `git diff 4c70b6f19..ebe84954b`)
- `restart/skinny/tranches/sk-v14/research/p2/p2b-dav1d-process.md`
  (217 L; V2-LOCKED; zero V3 edits)
- `restart/skinny/tranches/sk-v14/research/p2/p2c-arch-esoterica.md`
  (164 L; V2-LOCKED; zero V3 edits)
- `restart/skinny/tranches/sk-v14/research/p2/p2d-substrate-tape.md`
  (254 L; V2-LOCKED; zero V3 edits)
- `restart/skinny/tranches/sk-v14/research/p2/p2e-parse-that-gaps.md`
  (342 L; V1+V2-LOCKED; zero V3 edits)
- `restart/skinny/tranches/sk-v14/research/p2/p2f-grammar-neutral.md`
  (360 L; V3 amended — 2 cells at §2.10 line 164 + §2.13 line 197;
  status-prefix Stage-A framing preserved verbatim on both)

### §7.3 — P1 antecedent ledger (binding; carry-forward from V2)

- `restart/skinny/tranches/sk-v14/research/p1/p1e-hot-leaf-attribution.md
  §1.3 + §2.1-§2.4 + §4.1 + §4.4-§4.7` (binding hot-leaf census)
- `restart/skinny/tranches/sk-v14/research/p1/hardening/HARDENING-S-P1-V2-CONSOLIDATED.md`
  (S-P1 V2 close authorising S-P2)

### §7.4 — Bbnf source anchors (HEAD-verified for V3 CH1)

- `skinny/crates/runtime/src/grammars/json/scan.rs:32` (`scan_structurals_scalar`
  — C12 scalar reference; verified live at V3 HEAD per executable grep:
  definition at line 32; call sites at lines 29, 39, 280, 297)
- `skinny/crates/bbnf-simd/src/scalar/` directory contents at V3 HEAD: 8
  files present (`bitmap_next_set_bit.rs`, `bitmap_prefix_xor_64.rs`,
  `bulk_emit_positions_64.rs`, `byte_class_from_eq_set_64.rs`,
  `byte_class_from_table_64.rs`, `eob_pad_clamp.rs`, `mod.rs`,
  `swar_8byte.rs`); `byte_context_64.rs` + `bcax_64.rs` **NOT PRESENT**
  at V3 HEAD (Stage-A authoring targets per P2-F §2.10 + §2.13 — correctly
  framed in V3 amendment as "queued for S-P3 same-wave Lock 16
  same-commit admission … function body lands same-commit with SIMD
  body at S-P3").
- Sibling templates named in V3 cells PRESENT:
  `byte_class_from_eq_set_64.rs` (C10 sibling shape) +
  `bitmap_prefix_xor_64.rs` (C13 sibling shape).

### §7.5 — V3 commit + diff verification

- V3 commit `ebe84954b` — `git show --stat ebe84954b`: 2 files changed
  (V3 CHALLENGE-CONTEXT + P2-F); 42 insertions + 2 deletions; zero Rust
  source files modified.
- `git diff 4c70b6f19..ebe84954b -- p2a-sota-teardown.md p2b-dav1d-process.md
  p2c-arch-esoterica.md p2d-substrate-tape.md p2e-parse-that-gaps.md`
  returns **empty** (5 V2-LOCKED axes zero-drift confirmation).
- `git diff 4c70b6f19..ebe84954b -- p2f-grammar-neutral.md` returns 2
  unified-diff hunks (lines 161-167 + lines 194-200) — exactly the
  C10 + C13 verb-tense fix; no other content drift.

### §7.6 — External (SOTA + ISA) citations cross-checked at V3

V2 four-HEAD pinning at P2-A §5.3 (simdjson `168ef580...`, sonic-rs
`03545a95...`, yyjson `95f4c61b...`, asmjson 0.2.5) + dav1d
`1718ff9a...` + FFmpeg `08571418...` carries through V3 verbatim (zero
V3 edits to P2-A or P2-B). The V3 verb-tense fix at `p2f:164` + `p2f:197`
does NOT touch any ISA-citation line (`vextq_u8` per Lock 16 :285 on
C10 unchanged; `vbcaxq_u8` + `veor3q_u8` per Lock 16 :289 on C13
unchanged).
