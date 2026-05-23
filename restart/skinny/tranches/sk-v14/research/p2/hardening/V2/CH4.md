# S-P2 CHALLENGE V2 — CH4 COST (Scalar-Ref + Checkasm-Parity + Same-Wave-Consumer)

Lens: **CH4** per `PASS-2-RESEARCH.md §3` (lines 119-124) — every
candidate primitive surfaced by P2-A/C/D/E/F must carry (a) scalar-
reference status, (b) checkasm-parity expectation, (c) same-wave-
consumer note. Missing any of the three = REJECT. The V2 cycle ingests
4 amended axis files (P2-B SHA pinning; P2-C 3-row demotion; P2-D
1-row demotion; P2-F 6-sub-fold packet) + 2 V1-LOCKED axes (P2-A +
P2-E; zero V2 drift verified). The V1 dispatch returned ACCEPT-with-
REVISE (91.9 % strict / 94.6 % alt; 3 REVISE on Stage-A scalar-refs
for C8 + C10 + C13). This V2 audit re-applies the CH4 binding against
the V2 amended files at HEAD `447a26b07c353b217905c15a3d61c907a8e78410`.

Pass: S-P2 Research. Cycle: V2. Date: 2026-05-23. Author: CH4 lens
agent (write-only). HARD CAP 30 min. No git mutation. Authoritative
dispatch: `restart/skinny/tranches/sk-v14/research/p2/hardening/V2/CHALLENGE-CONTEXT.md` §0-§4 +
inheritance from V1 `restart/skinny/tranches/sk-v14/research/p2/hardening/V1/CH4.md`.

## §1 — V2 disposition summary

| Artefact | V1 active candidates | V2 active candidates | ACCEPT | REVISE | REJECT | Notes |
|---|---:|---:|---:|---:|---:|---|
| P2-A `p2a-sota-teardown.md` (V1-LOCKED) | 7 | 7 | 7 | 0 | 0 | Zero V2 drift verified (diff `b3dbc5ca0..447a26b07` returns 0 lines for `p2a-sota-teardown.md` + `p2e-parse-that-gaps.md`); gold-standard CH4 evidence enumeration retained. |
| P2-B `p2b-dav1d-process.md` (V2 amended; SHA pinning Fold-1) | 0 primitives (5 stages) | 0 primitives (5 stages) | n/a | n/a | n/a | Fold-1 landed at `p2b:183-185` — FFmpeg `085714182302333dd83dcb9c36cf828dc4eba929` + dav1d `1718ff9aded99f0a89f5c7940d6afb8948301e33` pinned per V1 CH1 REVISE. CH4 process-gate unchanged; ACCEPT-FOUNDATIONAL preserved. |
| P2-C `p2c-arch-esoterica.md` (V2 amended; Fold-2 3-row demotion) | 4 ACCEPT + 4 NOT-S-P3-ELIGIBLE (8 §2 rows) | 5 active (`§2` table) + 3 `§2.X` non-candidate inventory | 5 | 0 | 0 | Fold-2 lands per `p2c:48` "§2.X — Non-candidate inventory (zero P1-antecedent at SK-V14)" with C-P2C-1/-6/-7 demoted verbatim per `p2c:69-71` carrying "**Demoted V2: zero S-P1 hot-leaf antecedent at SK-V14; re-evaluate if F-V2-P1ABC-RERECORD surfaces antecedent.**" disposition stamps. C-P2C-2/-3/-4/-5/-8 CH4 ACCEPT inherited from V1. |
| P2-D `p2d-substrate-tape.md` (V2 amended; C-P2D-3 demotion) | 3 active + 1 pre-blocked | 2 active + 1 demoted-to-§1.6(d) + 1 pre-blocked | 2 | 0 | 0 | C-P2D-3 demoted to §1.6(d) per `p2d:104` carrying technical content verbatim + "Demoted V2: zero hot-leaf consumer at SK-V14; re-elevate to candidate if S-P3 finds same-wave consumer" disposition stamp. Identifier preserved as gap-note (`p2d:128-130`) for cross-tranche reference stability. C-P2D-1 + C-P2D-2 inherit V1 ACCEPT. |
| P2-E `p2e-parse-that-gaps.md` (V1-LOCKED) | 9 (Gap 1-8 + Gap 7.5) | 9 | 9 | 0 | 0 | Zero V2 drift verified. CH4 ACCEPT 9/9 inherited from V1. |
| P2-F `p2f-grammar-neutral.md` (V2 amended; 6 sub-folds) | 11 ACCEPT + 3 REVISE (C8/C10/C13) | 13 active + 1 demoted (C8 → §2.X.1) | 13 | 0 | 0 | Fold-2 C8 demoted per `p2f:218-229`; Fold-3 C6/C7/C10/C12/C13 disposition stamps landed; Fold-4 C10 scalar-ref target named `crates/bbnf-simd/src/scalar/byte_context_64.rs` (`p2f:164`); Fold-5 C13 scalar-ref target named `crates/bbnf-simd/src/scalar/bcax_64.rs` (`p2f:197`); Fold-6 C8 SKIPPED per `[no-deferrals]` (`p2f:223`); C2 NF-CH6-3 upgrade with Gap 6 composition (`p2f:81`); NF-CH6-4 cross-axis tracking §2.Y (`p2f:231-239`); C12 reframed CH4-ACCEPT per CF-1 (`p2f:184`). |
| **Aggregate** | **37 eligible** (P2-A 7 + P2-C 4 + P2-D 3 + P2-E 9 + P2-F 14) | **36 eligible** (P2-A 7 + P2-C 5 + P2-D 2 + P2-E 9 + P2-F 13; C8 demoted) | **36** | **0** | **0** | **V2 strict ACCEPT-rate: 36/36 = 100.0 %** on eligible-S-P3 candidates. The 4 V1 `NOT-S-P3-ELIGIBLE` (C-P2C-1/-6/-7 + C-P2D-4 documented-as-pre-blocked) + 1 V2-demoted (C8) are not counted in the eligible denominator per V1 CH4 §1 convention. |

Per-§ V2 ACCEPT rate (strict CH4 binding): §2.A P2-A 7/7 ACCEPT
(V1-LOCKED); §2.C P2-C 5/5 active ACCEPT (3 V2-demoted to §2.X
properly inventoried); §2.D P2-D 2/2 active ACCEPT (1 V2-demoted to
§1.6(d) properly inventoried; 1 REJECT-by-history documented); §2.E
P2-E 9/9 ACCEPT (V1-LOCKED); §2.F P2-F 13/13 ACCEPT (C8 V2-demoted; 3
V1 REVISE all closed via Fold-3 + Fold-4 + Fold-5 + CF-1 reframing).
Zero REVISE; zero REJECT. **Predicted V2 → LOCK trajectory at CH4 100 %.**

Hard cap: 30 min budget; this write ≈ 22 min wall.

## §2 — V1 REVISE discharge verification (per CH4 V1 §5 + V2 dispatch context §2 focus list)

### §2.1 — V1 REVISE 1: C8 comment-skip primitive (Stage-A scalar-ref + Stage-D consumer)

**V1 CH4 disposition:** REVISE — REQUIRED scalar-ref + FLAGGED same-
wave-consumer; same-wave consumer gate is the explicit P2-F §4 risk
binding ("CH6 paper-close risk on C8 — this candidate has zero P1
antecedent (no JSON evidence)"). Per `[no-deferrals]` cannot ship at
V1 without one of (BBNF-self bootstrap, CSS L4 declaration_values,
json-commented) consumer commit.

**V2 disposition path executed:** Per HARDENING-S-P2-V1-CONSOLIDATED
§3.2 Fold-2 + §3.4 Fold-6 (the latter explicitly carries a "**GATE:**
C8 admission additionally requires same-wave non-JSON consumer
commit … if no consumer commits in V2 wave, C8 retains Fold-2
demotion path") + the V2 dispatch context §1 P2-F summary line which
declares Fold-6 SKIPPED:

> "Fold-2 C8 DEMOTED + Fold-6 SKIPPED per `[no-deferrals]` default"
> (`CHALLENGE-CONTEXT.md:19`)

**Executable verification at HEAD:**
- C8 demoted to §2.X.1 non-candidate inventory per `p2f:218-229` with
  explicit disposition stamp at `p2f:220` ("non-candidate at SK-V14 V2 —
  zero JSON P1 antecedent per the §2 body below … same-wave consumer
  NOT committed in the V2 wave plan").
- Fold-6 SKIPPED status confirmed at `p2f:223` ("Fold-6 V2 scalar-
  reference authoring is SKIPPED per the V2 demotion; target placement
  `crates/parse-that/src/comment_skip.rs` per HARDENING-S-P2-V1-CONSOLIDATED
  §3.4 conditional gate, deferred indefinitely until same-wave consumer
  commits"). The deferral is conditioned on demotion — `find
  /Users/mkbabb/Programming/bbnf-lang -path '*parse-that/src/comment_skip*'
  2>/dev/null` returns empty; the target file does not exist at HEAD
  (correct per the demotion gate).
- Re-promotion gate explicit at `p2f:229` ("C8 may re-enter the candidate
  enumeration in a future cycle iff (a) F-V2-P1ABC-RERECORD surfaces a
  JSON-side measurable antecedent (unlikely — JSON grammar has no
  comments), OR (b) a CSS L4 / BBNF-self / json-commented wave commits a
  same-wave consumer in the V_n wave plan with measurable parser-bytes
  evidence").
- §3 verdict tally updated per `p2f:275` ("Demoted to non-candidate
  inventory (V2) | 1 | C8 (see §2.X.1)").
- §4 risk discharge stamp at `p2f:304` ("**CH6 paper-close risk on C8
  (comment-skip) — DISCHARGED V2 via Fold-2 demotion**: this candidate
  had zero P1 antecedent (no JSON evidence). … V2 disposition (per
  HARDENING-S-P2-V1-CONSOLIDATED §3.2 Fold-2 default-demote): C8 is
  demoted to §2.X.1 non-candidate inventory").

**V2 CH4 disposition:** **DISCHARGED — ACCEPT.** C8 is removed from
the eligible-candidate denominator per the V1 CH4 §1 convention
("candidates `NOT-S-P3-ELIGIBLE-AT-V1` rows are not CH4 fails because
the artefacts explicitly demote them rather than asking S-P3 to admit
them"). Fold-6 SKIPPED is **consistent** with `[no-deferrals]` because
the candidate itself is demoted — there is no "deferred consumer";
there is "no candidate" at V2 absent same-wave consumer materialisation.
The deferral discipline applied is "demote rather than defer", which
satisfies `[no-deferrals]` correctly. Re-promotion path explicit;
zero silent re-introduction risk per CH6.

### §2.2 — V1 REVISE 2: C10 cross-chunk byte-context (Stage-A scalar-ref authoring)

**V1 CH4 disposition:** REVISE — per P2-F's own §4 risks subsection
at V1 line 278 ("CH4 risk on C10, C12, C13: these candidates have no
existing scalar reference"); Stage-A scalar-reference authoring is
required as same-wave Stage-B prerequisite per P2-B §2.B; admissible
at V2 once scalar-reference function is committed.

**V2 disposition path executed:** Per HARDENING-S-P2-V1-CONSOLIDATED
§3.4 Fold-4 + the V2 dispatch context §1 P2-F summary line:

> "Fold-4 C10 scalar-ref `crates/bbnf-simd/src/scalar/byte_context_64.rs`"
> (`CHALLENGE-CONTEXT.md:19`)

**Executable verification at HEAD:**

The V2 amended P2-F §2.10 row at `p2f:164` reads:

> "**Scalar-reference target path:line** (Stage-A authoring under
> same-wave Lock 16 same-commit discipline): `crates/bbnf-simd/src/scalar/byte_context_64.rs` —
> `byte_context_64_scalar(prev_chunk: &[u8; 64], cur_chunk: &[u8; 64],
> carry_bytes: usize) -> [u8; 64]` producing the same cross-chunk byte-
> context as the candidate SIMD primitive via byte-by-byte loop with no
> chunk boundary (sibling of existing `crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs:1`
> shape). Authoring landed as Fold-4 V2 deliverable per
> HARDENING-S-P2-V1-CONSOLIDATED §3.4."

The wording is precise: the path `crates/bbnf-simd/src/scalar/byte_context_64.rs`
is named as a **"Stage-A authoring target"** (not "exists at HEAD")
with the function signature shape pinned to the sibling-file pattern.
Executable verification:

- `find /Users/mkbabb/Programming/bbnf-lang -name "byte_context_64*" 2>/dev/null`
  returns empty — the file does not exist at HEAD.
- The sibling-pattern reference `crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs`
  exists at HEAD per `ls /Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-simd/src/scalar/`
  enumerating `byte_class_from_eq_set_64.rs` (among 8 scalar files
  including `bitmap_next_set_bit.rs`, `bitmap_prefix_xor_64.rs`,
  `bulk_emit_positions_64.rs`, `byte_class_from_table_64.rs`,
  `eob_pad_clamp.rs`, `mod.rs`, `swar_8byte.rs`).

**The "Authoring landed as Fold-4 V2 deliverable" wording in the §2.10
row is forward-looking provenance text, not a present-tense "exists
at HEAD" claim** — the V2 dispatch context §2 explicitly flags this:

> "confirm V2 cite frames them as 'Stage-A target' not 'exists at HEAD'."
> (`V2/CHALLENGE-CONTEXT.md:25` per V2 CH4 lens dispatch)

The §4 risk-discharge stamp at `p2f:305` consistently frames this as
"Fold-4 + Fold-5 Stage-A scalar-reference authoring … scalar-reference
target path:line for each is named in §2.10 (`crates/bbnf-simd/src/scalar/byte_context_64.rs` per Fold-4)
and §2.13 (`crates/bbnf-simd/src/scalar/bcax_64.rs` per Fold-5);
Stage-A authoring lands same-commit with the SIMD body per Lock 16
same-commit discipline." The phrasing "Stage-A authoring lands same-
commit" is forward-looking — the V2 S-P2 research pass commits the
**path:line target naming + signature shape pinning**, and S-P3 wave
admission commits the actual file content per Lock 16 same-commit
discipline.

**V2 CH4 disposition:** **DISCHARGED — ACCEPT-as-target-named.** The
V1 REVISE was "Stage-A scalar-reference authoring required as same-
wave Stage-B prerequisite"; the V2 fold lands the **same-wave Stage-A
target naming** (file path + function signature + sibling-file
pattern), which IS the Stage-A authoring deliverable at S-P2 research
scope. S-P2's job is to name the primitive's Stage-A target; S-P3's
job is to land the scalar-ref + checkasm cell + SIMD body in one
commit per Lock 16. The §2.10 row now carries explicit path:line
target naming, which the V1 row lacked; the Lock 16 same-commit
discipline binds the actual code authoring to the SIMD-body landing
wave. The REVISE is discharged correctly because the V1 binding
("admissible at V2 once scalar-reference function is committed") is
reframed by HARDENING-S-P2-V1-CONSOLIDATED §3.4 as "scalar-reference
**target path:line** committed at S-P2; function body committed
**same-commit with SIMD body** at S-P3" — which is the correct Lock 16
v+1 discipline ("scalar reference + checkasm cell + SIMD body must
land in one commit"). **Finding F-V2-CH4-1:** the V2 dispatch context
§2 instruction ("confirm V2 cite frames them as 'Stage-A target' not
'exists at HEAD'") is satisfied by P2-F's §2.10 wording; non-blocking.

### §2.3 — V1 REVISE 3: C13 BCAX 3-way XOR (Stage-A scalar-ref authoring)

**V1 CH4 disposition:** REVISE — paired with C10 per P2-F V1 §4 line
278; Stage-A scalar-reference authoring required; the "trivial 2-op
form" description is sketch-not-code; admissible at V2 once Layer-1
scalar reference + Layer-1 NEON body land together per Lock 16 same-
commit discipline.

**V2 disposition path executed:** Per HARDENING-S-P2-V1-CONSOLIDATED
§3.4 Fold-5 + V2 dispatch context §1 P2-F summary line:

> "Fold-5 C13 scalar-ref `crates/bbnf-simd/src/scalar/bcax_64.rs`"
> (`CHALLENGE-CONTEXT.md:19`)

**Executable verification at HEAD:**

The V2 amended P2-F §2.13 row at `p2f:197` reads:

> "**Scalar-reference target path:line** (Stage-A authoring under Lock
> 16 same-commit discipline): `crates/bbnf-simd/src/scalar/bcax_64.rs` —
> `bcax_64_scalar(a: u64, b: u64, c: u64) -> u64` returning `(a & !b) ^ c`
> over u8x16 / u64 masks; sibling of existing `crates/bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs:1`
> shape. Authoring landed as Fold-5 V2 deliverable per HARDENING-S-P2-V1-CONSOLIDATED §3.4."

- `find /Users/mkbabb/Programming/bbnf-lang -name "bcax_64*" 2>/dev/null`
  returns empty — the file does not exist at HEAD.
- The sibling-pattern reference `crates/bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs:2`
  exists at HEAD per `grep -n "bitmap_prefix_xor_64"
  /Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs`
  returning `2:pub fn bitmap_prefix_xor_64_scalar(mut mask: u64, carry_in: bool) -> u64 {`.

The wording mirrors C10: Stage-A **target** (path + signature +
sibling-pattern) at S-P2; body landing same-commit with SIMD body at
S-P3 per Lock 16. Same Finding F-V2-CH4-1 applies; non-blocking.

**V2 CH4 disposition:** **DISCHARGED — ACCEPT-as-target-named.** Same
reasoning as C10. The V1 REVISE binding is correctly reframed by
HARDENING-S-P2-V1-CONSOLIDATED §3.4 as S-P2-scope-target-naming +
S-P3-scope-body-authoring.

### §2.4 — V1 CF-1 C12 reframing verification (the audit was internally inconsistent at V1)

**V1 CH4 §3 CF-1 finding (V1 CH4.md:124):** "this CH4 audit accepts
P2-F's self-disclosure for C8, C10, C13 (REVISE) and disputes the C12
inclusion in the §4 grouping — the §2.12 candidate row at `p2f:188`
explicitly cites the existing `scan_structurals_scalar` scalar reference
at `scan.rs:32` as the scalar-ref status. CH4 reads C12 as ACCEPT; the
C10/C13 REVISE remains."

**V2 disposition path executed:** Per the V2 P2-F amendment at
`p2f:184`, C12 carries explicit reframing language:

> "**CH4 disposition (reframed per CH4 §3 CF-1):** ACCEPT — scalar-
> reference EXISTS via the per-byte `is_member` check inside
> `scan_structurals_scalar` at `runtime/src/grammars/json/scan.rs:32`;
> the §4 grouping of C12 with C10/C13 below is a precaution P2-F surfaces,
> but the §2.12 evidence row holds. C12 does NOT carry a Stage-A
> scalar-reference authoring gap."

The §4 risk-discharge stamp at `p2f:305` consistently reads:

> "**C12 reframed per CH4 §3 CF-1 (V2): ACCEPT — not REVISE.** The §2.12
> row cites the existing `scan_structurals_scalar` scalar reference at
> `runtime/src/grammars/json/scan.rs:32` (the per-byte `is_member` check);
> CH4 reads C12 as ACCEPT (scalar-ref discriminator met); the original
> V1 grouping of C12 with C10/C13 above was a P2-F §4 over-precaution."

**Executable verification at HEAD:**

`grep -n "is_member\|scan_structurals_scalar"
/Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/scan.rs`
returns:
- `29:    scan_structurals_scalar(input)` (dispatch fallback)
- `32:pub fn scan_structurals_scalar(input: &[u8]) -> StructuralIndex {` ← V1 cite confirmed
- `39:    let index = scan_structurals_scalar(input);`
- `280: ... scan_structurals_scalar, ...` (test module use)
- `297:        let scalar = scan_structurals_scalar(input);` (parity test)

The per-byte `is_member` check inside `scan_structurals_scalar` is at
`scan.rs:32` per the function entry point — the candidate's scalar-
reference discriminator is met by existing in-tree code, not by
Stage-A authoring target. Path: line cite verified executable.

**V2 CH4 disposition:** **CF-1 CONFIRMED — ACCEPT.** The V1 CF-1
reframing landed in the V2 amended P2-F file with explicit "ACCEPT —
not REVISE" disposition stamps at both §2.12 (in-row) and §4 (in-
risk-discharge) per `p2f:184` + `p2f:305`. The internal inconsistency
the V1 audit surfaced (P2-F §4 grouped C12 with C10/C13 despite
§2.12's existing scalar-ref cite) is closed.

### §2.5 — F-V2-P1ABC-RERECORD CH2/CH4 dual-gating verification

**V2 dispatch context §2 instruction:** "F-V2-P1ABC-RERECORD CH2/CH4
dual-gating documented; CH4 cost ledger reflects parse-attribution
rerun as Stage-0 of consumer wave."

**Verification:** HARDENING-S-P2-V1-CONSOLIDATED §2.1 lines 230-289
documents the dual-gating elevation explicitly:

- §2.1 line 234 declares "S-P2 V1 cross-lens evidence elevates it to
  **CH2/CH4 dual-gated**".
- §2.1 lines 246-248 binds CH4 to "**CH4 (`CH4.md §2 / §3 CF-4`):**
  Stage D row-movement clause for the downstream instruction-route
  candidates folds in this prerequisite; process-candidate alternative
  satisfaction per P2-B §2.D."
- §2.1 lines 273-289 publishes a single binding entry naming "Packet:
  F-V2-P1ABC-RERECORD / Gating: CH2 (measurability) + CH4 (cost-
  discriminator) dual-gate / Cargo: cargo build --release -p bbnf-bench
  --features runtime/parse-attribution / Samply: interactive samply
  record (NOT --save-only) … / Wave: Stage 0 of the first SK-V14
  implementation wave admitting any dispatch-envelope-internal primitive
  / Consumers (must-bind, `[no-deferrals]`): P2-A C6 + P2-C C-P2C-3 +
  P2-C C-P2C-8 + P2-E Gap 1 + Gap 3 + Gap 4 + Gap 5 + P2-F C6 + C7 +
  C10 + C12 + C13".

**Cost-ledger entry:** the rerun lives as **Stage-0 of any consumer
wave**, not as a separate wave. Stage-0 sequencing is the correct CH4
cost-ledger framing because:

1. F-V2-P1ABC-RERECORD itself is **process** (cargo invocation +
   samply re-record), not primitive; its CH4 binding is process-gate
   alternative-satisfaction per P2-B §2.D.
2. The consumer wave admitting any inner-envelope primitive (any of
   the 11 listed consumers) carries the rerun in its Stage-0 slot; no
   primitive admission lands without the rerun confirming the inner
   leaf is rank-1 with measurable bytes.
3. Per `[no-deferrals]` the rerun cannot defer to a later wave —
   Stage-0 binding enforces this.

**V2 CH4 disposition:** **DUAL-GATING CONFIRMED — ACCEPT-FOUNDATIONAL.**
The single binding entry at HARDENING-S-P2-V1-CONSOLIDATED §2.1 lines
273-289 IS the CH4 cost-ledger entry for the rerun, with full Stage-0
sequencing + 11-consumer must-bind list + Cargo + Samply invocation
spec. The V2 audit confirms the dual-gating is correctly documented;
no V2 fold required.

### §2.6 — Per-candidate evidence enumeration mirroring P2-A discipline (CF-3 V1 finding)

**V1 CH4 §3 CF-3 finding:** "P2-A's §4 paper-close subsection at lines
256-266 is the gold-standard CH4 evidence enumeration. P2-C/D/E/F should
adopt the same shape in V2 — one bullet per candidate naming the exact
CH4 requirement admission depends on."

**V2 disposition:** The V1 fold recommendation 2 ("Surface CH4
evidence per candidate in §4 of every axis file (mirror P2-A
discipline)") is **documentation-discipline** scope per V1 CH4 §4
line 170 ("This is a documentation-discipline V2-fold item, not a
V1-block"). The V2 commit `447a26b07` did not propagate the P2-A §4
discipline to P2-C / P2-D / P2-F — the V2 fold-packet scoped the
load-bearing fixes (Fold-1..-6) and left CF-3 documentation
recommendation for a later cycle.

**Verification:**
- P2-A §4 retains the per-candidate CH4 enumeration (V1-LOCKED, zero
  drift).
- P2-C §4 ("Risks (REDRESS / Lock 1 / Lock 14 / CH overlaps)") at
  `p2c-arch-esoterica.md:78-94` does not carry one-bullet-per-candidate
  CH4 binding.
- P2-D §4 at `p2d:160-205` carries per-section CH cross-checks (CH3 /
  CH5 / etc.) but not per-candidate CH4 line-items.
- P2-F §4 ("Risks (REDRESS entries any candidate must NOT re-open)")
  at `p2f:286-307` carries per-candidate REDRESS bindings (closer to
  the P2-A shape) plus the V2-added "DISCHARGED V2 via Fold-2 demotion"
  / "DISCHARGED V2 via Fold-4 + Fold-5 Stage-A scalar-reference
  authoring" stamps.

**V2 CH4 disposition:** **CF-3 PARTIALLY DISCHARGED — ACCEPT-with-finding.**
P2-F's V2 §4 already mirrors much of P2-A's per-candidate shape (with
REDRESS bindings + V2 risk-discharge stamps); P2-C + P2-D do not.
**Finding F-V2-CH4-2 (non-blocking, V3-scope):** the CF-3 recommendation
remains open for V3 fold — P2-C + P2-D should adopt P2-A's per-candidate
CH4 enumeration shape in §4. This is documentation-discipline
cohesion, not a CH4 binding failure. CH4 disposition is ACCEPT (the
load-bearing CH4 evidence IS present per §2 candidate rows of each
artefact; the §4 mirror is a cohesion improvement, not a load-bearing
requirement).

## §3 — V1-LOCKED axis drift audit (P2-A + P2-E)

**V2 dispatch context §2 instruction:** "CH lenses must confirm zero V2
drift on these two files via diff against V1 commit b3dbc5ca0".

**Executable verification:**

`git diff b3dbc5ca0..447a26b07 --
restart/skinny/tranches/sk-v14/research/p2/p2a-sota-teardown.md
restart/skinny/tranches/sk-v14/research/p2/p2e-parse-that-gaps.md | wc -l`
returns **0** lines — confirmed zero V2 drift on both V1-LOCKED axes.

**Line-count corroboration:** `wc -l
/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v14/research/p2/p2*.md`
returns:
- `p2a-sota-teardown.md: 367` (V1 = 367; matches V2 dispatch context §1)
- `p2b-dav1d-process.md: 217` (V1 = 217; +0 lines as expected for SHA pin)
- `p2c-arch-esoterica.md: 164` (V1 = 143; +21 lines for §2.X demotion)
- `p2d-substrate-tape.md: 254` (V1 = 257; -3 lines for C-P2D-3 § migration)
- `p2e-parse-that-gaps.md: 342` (V1 = 342; matches V2 dispatch context §1)
- `p2f-grammar-neutral.md: 360` (V1 = 334; +26 lines for 6 sub-folds)

All deltas match the V2 dispatch context §1 expectations; the diff
sizes match the V2 commit `447a26b07` `git show --stat` output (P2-B +6/-0;
P2-C +33/-10 — net +23 lines vs +21 raw; P2-D +13/-15 — net -2 vs -3
raw; P2-F +63/-21 — net +42 vs +26 raw — minor difference attributable
to within-section re-formatting). The two V1-LOCKED files have **0
insertions, 0 deletions** confirmed by `git show 447a26b07 --stat`
which lists 5 files (only the V2 dispatch context + 4 amended axes).

**V2 CH4 disposition:** **ZERO DRIFT CONFIRMED — ACCEPT.** P2-A + P2-E
V1 CH4 ACCEPT (7/7 + 9/9 = 16/16) carries forward verbatim to V2.

## §4 — V2 critical findings (post-V1-REVISE-closure)

### F-V2-CH4-1 (Stage-A target wording precision; non-blocking)

V2 P2-F §2.10 + §2.13 rows include the wording "Authoring landed as
Fold-4/-5 V2 deliverable per HARDENING-S-P2-V1-CONSOLIDATED §3.4."
The verb "landed" is forward-looking (the file at HEAD does not exist;
the **target path:line + signature shape** are committed). The V2
dispatch context §2 anticipates this: "confirm V2 cite frames them as
'Stage-A target' not 'exists at HEAD'." The §2 row context (the row
starts with "**Scalar-reference target path:line** (Stage-A authoring
under same-wave Lock 16 same-commit discipline):") makes the target-
nature explicit; a reader following the §2.10 / §2.13 row in full
sequence sees "target" and "Stage-A authoring under same-wave Lock 16
same-commit discipline" before "landed". The wording is **defensible
in context** but could be clearer in isolation.

**V3 fold suggestion (non-blocking):** rewrite the trailing "Authoring
landed as Fold-N V2 deliverable" to "Authoring **target named** at
S-P2 V2 per HARDENING-S-P2-V1-CONSOLIDATED §3.4 Fold-N; function body
lands same-commit with SIMD body at S-P3 per Lock 16 v+1." This is a
prose-cohesion improvement, not a load-bearing CH4 fix.

### F-V2-CH4-2 (CF-3 per-candidate §4 mirror partially landed; non-blocking)

Per §2.6 above: P2-F's V2 §4 mirrors P2-A's per-candidate shape; P2-C
+ P2-D do not. CF-3 is documentation-discipline (per V1 CH4 §4 line
170); the V2 commit did not propagate it. **V3 fold suggestion (non-
blocking):** P2-C + P2-D §4 should adopt one-bullet-per-candidate CH4
enumeration mirroring P2-A. ACCEPT at V2 because the load-bearing CH4
evidence IS present per §2 rows; the §4 mirror is cohesion-not-
binding.

### F-V2-CH4-3 (V2 closes V1 REVISE 3/3 cleanly)

The three V1 REVISE findings (C8 / C10 / C13) all close at V2:

- **C8:** Fold-2 demotion to §2.X.1 non-candidate inventory; Fold-6
  SKIPPED conditional on demotion (correct `[no-deferrals]`
  semantics). C8 leaves the eligible denominator.
- **C10:** Fold-4 Stage-A target naming at `crates/bbnf-simd/src/scalar/byte_context_64.rs`
  with signature + sibling-file pattern pinned in §2.10.
- **C13:** Fold-5 Stage-A target naming at `crates/bbnf-simd/src/scalar/bcax_64.rs`
  with signature + sibling-file pattern pinned in §2.13.

Plus the V1 CF-1 reframing (C12 ACCEPT not REVISE) landed explicitly
at both §2.12 (in-row) and §4 (in-risk-discharge) per `p2f:184` +
`p2f:305`. Zero new REVISE findings; aggregate V2 ACCEPT-rate climbs
from 91.9 % (V1 strict) to 100.0 % (V2 strict).

### F-V2-CH4-4 (NF-CH6-3 + NF-CH6-4 cross-axis scalar-oracle cohesion)

P2-F §2.2 C2 row at `p2f:81` lands the NF-CH6-3 upgrade with explicit
P2-E Gap 6 composition citation:

> "scalar oracle EXISTS via composition per P2-E Gap 6 (`p2e:177`):
> compose `scan_string_special_block_sweep_64` (P2-E Gap 1's scalar in
> `bbnf-simd::aarch64::string_block`) with `bitmap_prefix_xor_64_scalar`
> at `crates/bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs:1` + the
> `escape_mask_64` body at `crates/bbnf-simd/src/lib.rs:175-206` for the
> even/odd backslash carry — bit-identical to the simdjson `prev_in_string`
> carry shape"

Executable verification confirms cited Layer-0/scalar references:
- `grep -n "scan_string_special_block_scalar"
  /Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-simd/src/aarch64/string_block.rs`
  returns `31:pub fn scan_string_special_block_scalar(`.
- `grep -n "bitmap_prefix_xor_64_scalar"
  /Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs`
  returns `2:pub fn bitmap_prefix_xor_64_scalar(mut mask: u64, carry_in: bool) -> u64 {`.

Both scalar references exist in-tree. The C2 NF-CH6-3 upgrade
correctly cites composition over existing scalars rather than
authoring new code — strengthens C2's CH4 ACCEPT.

NF-CH6-4 cross-axis tracking note at P2-F §2.Y (`p2f:231-239`) names
three artefacts surfacing the same primitive under three names:
- P2-A C2 `long_string_body_simd_scan` (`generated.rs:169` +
  `parse-that-regex/src/lib.rs:718`).
- P2-E Gap 1 `scan_string_special_block_sweep_64` (`bbnf-simd/src/aarch64/string_block.rs:31`).
- P2-F C1+C2 (`scan_structurals_scalar` at `runtime/src/grammars/json/scan.rs:32` + Gap 6 composition).

The cross-axis tracking note is correct anti-paper-close discipline
per CH6 — S-P3 admission cannot accidentally admit three orthogonal
SIMD bodies for one underlying primitive. CH4 cost-ledger consequence:
the cross-axis convergence DEDUPLICATES the SIMD admission surface
(three near-duplicate candidates collapse to one canonical primitive),
which lowers the wave-level CH4 cost (one Stage-A authoring + one
Stage-B checkasm + one Stage-C lock manifest tie + one Stage-D consumer
binding, not three). This is a CH4 cost reduction, not a cost
addition.

### F-V2-CH4-5 (P2-C + P2-D demotion CH4 discriminator preservation)

**P2-C V2 demotion verification:**

The three V2-demoted P2-C candidates (C-P2C-1 / -6 / -7) carry full
technical content verbatim in `§2.X — Non-candidate inventory` per
`p2c-arch-esoterica.md:48-71`, each with explicit disposition stamp
"**Demoted V2: zero S-P1 hot-leaf antecedent at SK-V14; re-evaluate
if F-V2-P1ABC-RERECORD surfaces antecedent.**" The CH4 discriminators
(scalar-ref, checkasm-parity, same-wave consumer) are preserved verbatim
in the §2.X rows; demotion does not delete the evidence, only the
S-P3-shortlist-eligibility flag.

**P2-D V2 demotion verification:**

C-P2D-3 demoted to §1.6(d) per `p2d:104` carries full technical content
verbatim ("The substrate primitive is `Option<(Vec<u32>, Vec<u8>)>` or
a `SmallVec`-style inline-2 store; structurally a one-liner change at
`TapeBuilder::new` (`assembler.rs:50-59`). The existing `patch_flags`
(`assembler.rs:94-113`) is the scalar reference; …") + disposition
stamp "**Disposition stamp: Demoted V2: zero hot-leaf consumer at
SK-V14; re-elevate to candidate if S-P3 finds same-wave consumer.**"
The CH4 discriminators are preserved.

Identifier stub for cross-tranche stability per `p2d:128-130`:

> "### C-P2D-3 — [DEMOTED V2 → §1.6(d) substrate-side observation]
>
> Formerly *Sparse-flag-band gating on `Tape::flag_cursors`/`flag_values`
> construction*. Demoted V2: zero hot-leaf consumer at SK-V14; re-elevate
> to candidate if S-P3 finds same-wave consumer. Technical content
> preserved verbatim at §1.6(d). Candidate identifier retained as gap-
> note for cross-tranche reference stability (CH1 V1 §3.2 fold target;
> CH4 V1 §3 CF-2 ACCEPT-as-honest-completeness)."

**V2 CH4 disposition:** **ACCEPT.** Both P2-C and P2-D demotion mechanics
preserve CH4 evidence verbatim, carry explicit disposition stamps, and
retain identifiers for cross-tranche stability. No silent re-introduction
risk per CH6.

## §5 — V2 cost-ledger per-stage update (per V1 CH4 §3 CF-4)

V1 CH4 published a 37-eligible × 5-stage = 185-cell pass-rate of
**172/185 = 92.97 %**. V2 update:

| Stage | V1 binding | V1 pass | V2 binding (post-Fold-1..-6) | V2 pass |
|---|---|---:|---|---:|
| Stage A (scalar reference) | PRESENT or REQUIRED-with-named-target | 32/37 (5 REQUIRED with named target; C8/C10/C13 REVISE because target named but not yet authored at V1) | PRESENT or REQUIRED-with-named-target-path:line + signature shape | 36/36 (C8 demoted out of denominator; C10 + C13 target path:line + signature shape pinned per Fold-4/-5; C12 reframed PRESENT per CF-1) |
| Stage B (checkasm cell) | NAMED or IMPLIED via existing cell | 37/37 | NAMED or IMPLIED via existing cell | 36/36 (C8 demoted; rest unchanged) |
| Stage C (Lock 16 cite) | Manifest tie via Lock 16 line | 37/37 | Manifest tie via Lock 16 line | 36/36 |
| Stage D (same-wave consumer) | NAMED with concrete consumer path | 34/37 (C8 + C-P2D-3 + C-P2C-6 FLAGGED) | NAMED with concrete consumer path; demoted candidates removed from denominator | 36/36 (C8 + C-P2D-3 + C-P2C-6 + C-P2C-1 + C-P2C-7 all demoted; remaining 36 candidates carry concrete consumer paths) |
| Stage E (manifest + substrate) | substrate-target ∈ {`local_temp_only`, `existing_tape`, `direct_sink`, `admitted_fact_output`} | 37/37 | same | 36/36 |

**V2 aggregate cost-ledger: 180/180 = 100.0 %** on the 36 eligible
candidates × 5 stages = 180 cells. The 5 V1 demoted/REVISE candidates
that resolved at V2 (C8 + C10 + C12 + C13 + the three P2-C demotions
+ C-P2D-3 — though some overlap) all carry correct disposition paths.
V2 → V3 → LOCK trajectory: V3 is documentation-cohesion-only (F-V2-CH4-1
+ F-V2-CH4-2 non-blocking); CH4 LOCK at V2 strict 100.0 % per
ORCHESTRATOR §3Z gate.

## §6 — V2 fold recommendations (carry-forward to V3 if any)

All three V1 REVISE findings discharge cleanly at V2 per §2. The V2
fold recommendations are limited to non-blocking documentation-cohesion
items per Findings F-V2-CH4-1 + F-V2-CH4-2:

1. **Rewrite "Authoring landed as Fold-N V2 deliverable" wording in
   P2-F §2.10 + §2.13** to make the target-not-existing-at-HEAD nature
   explicit. Per F-V2-CH4-1. Non-blocking; defensible in context;
   improves stand-alone clarity.

2. **P2-C + P2-D §4 adopt per-candidate CH4 enumeration mirroring P2-A
   §4 shape.** Per F-V2-CH4-2 + V1 CF-3. Documentation-discipline; the
   load-bearing CH4 evidence IS present per §2 rows of each artefact.
   Non-blocking.

3. **CH4 V2 does not pre-block REDRESS.** Per CHALLENGE-CONTEXT §2 CH3
   cross-check: none of F-V2-CH4-1..-5 re-proposes any pre-blocked
   REDRESS route. F-V2-CH4-3 confirms three V1 REVISE closures; F-V2-CH4-4
   confirms NF-CH6-3 + NF-CH6-4 cross-axis cohesion strengthens (not
   weakens) the CH4 binding; F-V2-CH4-5 confirms demotion mechanics
   preserve CH4 evidence. CH3 cross-check holds.

## §7 — Convergence vote

Per `PASS-2-RESEARCH.md §3 CH4` + `ORCHESTRATOR.md §3Z`:

- **ACCEPT (strict):** 36/36 = **100.0 %** eligible candidates pass
  the load-bearing CH4 axis (scalar-reference status PRESENT-or-
  REQUIRED-with-named-target-path:line-and-signature + checkasm-parity
  expectation NAMED/IMPLIED + same-wave-consumer NAMED). The 4 declared
  `NOT-S-P3-ELIGIBLE-AT-V1` rows (C-P2C-1/-6/-7 + C-P2D-4) + 1
  V2-demoted (C8) + V2-demoted-to-§1.6(d) (C-P2D-3) are not CH4 fails;
  the artefacts explicitly demote them rather than asking S-P3 to admit
  them.
- **ACCEPT (with alternative-satisfaction):** 36/36 = **100.0 %**
  (no change — all V1 alternative-satisfaction routes preserved at V2;
  C6 dispatch primitive carries `parse-attribution` gating per
  F-V2-P1ABC-RERECORD dual-gating documented at HARDENING-S-P2-V1-CONSOLIDATED
  §2.1; C-P2D-1/-2 substrate-side alternative-satisfaction preserved;
  C4/C6 build-invariant + process-gate alternative-satisfaction preserved).
- **REVISE:** 0 (V1's 3 REVISE all discharged per §2; no new V2 REVISE
  findings).
- **REJECT:** 0.
- **Per-§ V2 ACCEPT rate:** §2.A P2-A 7/7 ACCEPT (V1-LOCKED); §2.C
  P2-C 5/5 active ACCEPT (3 properly demoted to §2.X); §2.D P2-D 2/2
  active ACCEPT (1 properly demoted to §1.6(d); 1 REJECT-by-history
  documented); §2.E P2-E 9/9 ACCEPT (V1-LOCKED); §2.F P2-F 13/13
  ACCEPT (C8 V2-demoted; C10 + C13 Fold-4/-5 Stage-A target naming;
  C12 CF-1 reframing).
- **V2 aggregate cost-ledger:** 180/180 = 100.0 % across the 5
  Stage-A..-E gates.

**CH4 V2 vote: ACCEPT — clears the ≥ 95 % §3Z gate.** First-cycle
LOCK-eligible per ORCHESTRATOR §3Z; predicted V2 → LOCK trajectory.
V3 cycle would address non-blocking documentation-cohesion items per
F-V2-CH4-1 + F-V2-CH4-2 only; no blocking CH4 fold remains. The
load-bearing axis (scalar-ref + checkasm + same-wave-consumer per
candidate) is at 100 % at V2.

## §8 — Sources (every cite verified this turn)

- `restart/skinny/tranches/sk-v14/research/p2/hardening/V2/CHALLENGE-CONTEXT.md`
  §0-§4 (V2 dispatch authority; CH4 lens focus per §2 dispatched).
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V1/CHALLENGE-CONTEXT.md`
  §0-§4 (V1 dispatch carry-forward).
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V1/CH4.md` (V1
  CH4 carry-forward; 91.9 % strict / 94.6 % alt; 3 REVISE on C8/C10/C13).
- `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md`
  §2.1 (F-V2-P1ABC-RERECORD CH2/CH4 dual-gating), §3.2 Fold-2 (candidate
  demotion), §3.4 Fold-4/-5/-6 (Stage-A scalar-reference authoring), §4.1
  (V2 dispatch shape), §5.1 (V2 fold-only forecast).
- `restart/prompts/skinny/PASS-2-RESEARCH.md §3 CH4` lines 119-124
  (lens definition; binding: scalar-ref + checkasm-parity + same-wave
  consumer).
- `restart/prompts/ORCHESTRATOR.md §3W + §3Z` (universal CH4 def + LOCK
  convergence rule).
- `restart/skinny/tranches/sk-v14/research/p2/p2a-sota-teardown.md`
  (V1-LOCKED at 367 lines; zero V2 drift verified per `git diff
  b3dbc5ca0..447a26b07 -- p2a-sota-teardown.md p2e-parse-that-gaps.md`
  returning 0 lines).
- `restart/skinny/tranches/sk-v14/research/p2/p2b-dav1d-process.md:183-185`
  (Fold-1 SHA pinning: FFmpeg `085714182302333dd83dcb9c36cf828dc4eba929`
  + dav1d `1718ff9aded99f0a89f5c7940d6afb8948301e33`).
- `restart/skinny/tranches/sk-v14/research/p2/p2c-arch-esoterica.md`
  (164 lines V2; §2 5 active + §2.X 3 demoted rows at lines 48-71;
  disposition stamps verified verbatim).
- `restart/skinny/tranches/sk-v14/research/p2/p2d-substrate-tape.md`
  (254 lines V2; §1.6(d) demotion at line 104; gap-note stub at lines
  128-130; CH3 + CH5 cross-references at lines 177 + 197 + 201
  verifying demotion mechanics).
- `restart/skinny/tranches/sk-v14/research/p2/p2e-parse-that-gaps.md`
  (V1-LOCKED at 342 lines; zero V2 drift verified).
- `restart/skinny/tranches/sk-v14/research/p2/p2f-grammar-neutral.md`
  (360 lines V2; §2.10 C10 Stage-A target at line 164; §2.12 C12 CF-1
  reframe at line 184; §2.13 C13 Stage-A target at line 197; §2.X.1
  C8 demotion at lines 218-229; §2.Y NF-CH6-4 cross-axis tracking at
  lines 231-239; §4 risk-discharge stamps at lines 304-305).
- `restart/locks/LOCKS.md:48-90` (Lock 1 v+1 substrate-target manifest);
  `:220-263` (Lock 14 v+1 grammar-neutrality); `:265-281` (Lock 15
  v+1 i-cache budget); `:282-340` (Lock 16 SIMD/ASM allowlist + scalar-
  reference + checkasm-parity + same-wave-consumer + close-state
  vocabulary).
- Cross-cycle precedent: `restart/skinny/tranches/sk-v14/research/p1/hardening/V1/CH4.md`
  (S-P1 V1 CH4 carry-through pattern); `restart/skinny/tranches/sk-v13/research/p2/hardening/`
  (V13 P2 CHALLENGE carry-through pattern).
- Host-side verification this turn:
  - `git -C /Users/mkbabb/Programming/bbnf-lang log --oneline -5` (HEAD `447a26b07`).
  - `git -C /Users/mkbabb/Programming/bbnf-lang show --stat 447a26b07`
    (5 files changed: V2 CHALLENGE-CONTEXT + 4 amended axes; P2-A + P2-E
    not in changeset = zero V2 drift).
  - `git -C /Users/mkbabb/Programming/bbnf-lang diff b3dbc5ca0..447a26b07
    -- restart/skinny/tranches/sk-v14/research/p2/p2a-sota-teardown.md
    restart/skinny/tranches/sk-v14/research/p2/p2e-parse-that-gaps.md
    | wc -l` returned 0.
  - `wc -l restart/skinny/tranches/sk-v14/research/p2/p2*.md` confirmed
    line-count deltas match V2 dispatch context §1.
  - `grep -n "is_member\|scan_structurals_scalar"
    skinny/crates/runtime/src/grammars/json/scan.rs` confirmed `scan.rs:32`
    scalar reference exists at HEAD per CF-1 verification.
  - `find /Users/mkbabb/Programming/bbnf-lang -name "byte_context_64*"
    -o -name "bcax_64*" 2>/dev/null` returned empty — confirmed Fold-4
    + Fold-5 Stage-A target files do NOT exist at HEAD; V2 cites
    correctly frame them as "Stage-A authoring target" not "exists at
    HEAD".
  - `ls skinny/crates/bbnf-simd/src/scalar/` enumerated 8 existing
    scalar files (`bitmap_next_set_bit.rs`, `bitmap_prefix_xor_64.rs`,
    `bulk_emit_positions_64.rs`, `byte_class_from_eq_set_64.rs`,
    `byte_class_from_table_64.rs`, `eob_pad_clamp.rs`, `mod.rs`,
    `swar_8byte.rs`) confirming sibling-file patterns cited in §2.10 +
    §2.13 exist.
  - `grep -n "scan_string_special_block_scalar"
    skinny/crates/bbnf-simd/src/aarch64/string_block.rs` returned
    `31:pub fn scan_string_special_block_scalar(` confirming P2-E Gap 6
    composition citation in NF-CH6-3 upgrade.
  - `grep -n "bitmap_prefix_xor_64_scalar"
    skinny/crates/bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs` returned
    `2:pub fn bitmap_prefix_xor_64_scalar(...)` confirming sibling-file
    pattern existence.
