# S-P2 CHALLENGE V3 — CH4 COST (Confirming-Cycle Lens; Scalar-Ref + Checkasm-Parity + Same-Wave-Consumer)

Lens: **CH4** per `PASS-2-RESEARCH.md §3` (lines 119-124) — every
candidate primitive surfaced by P2-A/C/D/E/F carries (a) scalar-
reference status, (b) checkasm-parity expectation, (c) same-wave-
consumer note. Missing any of the three = REJECT. V3 is the **second
consecutive ≥95% cycle** for CH4 (V1 91.9% strict → V2 100.0% strict →
V3 100.0% strict expected) per `ORCHESTRATOR.md §3Z` cohort LOCK gate.
The V3 cycle ingests **1 amended axis** (P2-F: 2 verb-tense cells at
§2.10 C10 + §2.13 C13) + **5 V2-LOCKED axes** (P2-A/B/C/D/E; zero V3
drift verified). The V2 dispatch returned ACCEPT-strict 36/36 = 100.0 %
with two non-blocking findings F-V2-CH4-1 (verb-tense precision) +
F-V2-CH4-2 (CF-3 §4 mirror partial). This V3 audit re-applies the CH4
binding at HEAD `ebe84954b1a6c31bb6183ca8f5e68d88647d9df7`.

Pass: S-P2 Research. Cycle: V3 (confirming cycle). Date: 2026-05-23.
Author: CH4 lens agent (write-only). HARD CAP 25 min. No git mutation.
Authoritative dispatch: `restart/skinny/tranches/sk-v14/research/p2/hardening/V3/CHALLENGE-CONTEXT.md` §0-§4 +
inheritance from V2 `restart/skinny/tranches/sk-v14/research/p2/hardening/V2/CH4.md`.

## §1 — V3 disposition summary (confirming cycle)

| Artefact | V2 active candidates | V3 active candidates | ACCEPT | REVISE | REJECT | Notes |
|---|---:|---:|---:|---:|---:|---|
| P2-A `p2a-sota-teardown.md` (V1+V2-LOCKED; V3 zero-drift) | 7 | 7 | 7 | 0 | 0 | Zero V3 drift verified (`git diff 4c70b6f19..ebe84954b -- p2a-sota-teardown.md` returns 0 lines). 7/7 V1+V2 ACCEPT carries through verbatim. |
| P2-B `p2b-dav1d-process.md` (V2-LOCKED; V3 zero-drift) | 0 primitives (5 stages) | 0 primitives (5 stages) | n/a | n/a | n/a | Zero V3 drift. Process-gate ACCEPT-FOUNDATIONAL preserved. |
| P2-C `p2c-arch-esoterica.md` (V2-LOCKED; V3 zero-drift) | 5 active + 3 §2.X demoted | 5 active + 3 §2.X demoted | 5 | 0 | 0 | Zero V3 drift. 5/5 active ACCEPT preserved; 3 demoted properly inventoried with disposition stamps. |
| P2-D `p2d-substrate-tape.md` (V2-LOCKED; V3 zero-drift) | 2 active + 1 §1.6(d) demoted + 1 REJECT-by-history | 2 active + 1 §1.6(d) demoted + 1 REJECT-by-history | 2 | 0 | 0 | Zero V3 drift. C-P2D-1 + C-P2D-2 ACCEPT preserved. |
| P2-E `p2e-parse-that-gaps.md` (V1+V2-LOCKED; V3 zero-drift) | 9 (Gap 1-8 + Gap 7.5) | 9 | 9 | 0 | 0 | Zero V3 drift. 9/9 ACCEPT preserved. |
| P2-F `p2f-grammar-neutral.md` (V3 amended; 2 verb-tense cells §2.10 + §2.13) | 13 active + 1 §2.X.1 demoted | 13 active + 1 §2.X.1 demoted | 13 | 0 | 0 | V3 amendment: `p2f:164` (C10) + `p2f:197` (C13) verb-tense "Authoring landed as Fold-N V2 deliverable" → "Authoring queued for S-P3 same-wave Lock 16 same-commit admission per HARDENING-S-P2-V1-CONSOLIDATED §3.4 Fold-N; function body lands same-commit with SIMD body at S-P3." Discharges F-V2-CH4-1 + (convergently) F-V2-CH1-1. C8 demotion + C12 CF-1 reframing inherited from V2. |
| **Aggregate** | **36 eligible** | **36 eligible** | **36** | **0** | **0** | **V3 strict ACCEPT-rate: 36/36 = 100.0 %** — second consecutive ≥95% cycle → CH4 cohort §3Z LOCK-eligible. |

Per-§ V3 ACCEPT rate (strict CH4 binding): §2.A P2-A 7/7 ACCEPT
(V1+V2+V3 chain LOCK); §2.C P2-C 5/5 active ACCEPT (V2+V3 chain
LOCK); §2.D P2-D 2/2 active ACCEPT (V2+V3 chain LOCK); §2.E P2-E 9/9
ACCEPT (V1+V2+V3 chain LOCK); §2.F P2-F 13/13 ACCEPT (V2 fold landed
3 V1 REVISE closures; V3 fold landed verb-tense precision).
Zero REVISE; zero REJECT. **V3 → CH4 cohort LOCK at 100.0 %.**

Hard cap: 25 min budget; this write ≈ 18 min wall.

## §2 — V2 finding discharge verification (per V3 dispatch context §2 focus)

### §2.1 — F-V2-CH4-1 discharge: Stage-A target wording verb-tense fix

**V2 CH4 §4 F-V2-CH4-1 finding (non-blocking; V2/CH4.md:391-410):**

> "V2 P2-F §2.10 + §2.13 rows include the wording 'Authoring landed as
> Fold-4/-5 V2 deliverable per HARDENING-S-P2-V1-CONSOLIDATED §3.4.'
> The verb 'landed' is forward-looking (the file at HEAD does not exist;
> the **target path:line + signature shape** are committed). … The
> wording is **defensible in context** but could be clearer in
> isolation. **V3 fold suggestion (non-blocking):** rewrite the
> trailing 'Authoring landed as Fold-N V2 deliverable' to 'Authoring
> **target named** at S-P2 V2 per HARDENING-S-P2-V1-CONSOLIDATED §3.4
> Fold-N; function body lands same-commit with SIMD body at S-P3 per
> Lock 16 v+1.'"

**V3 disposition path executed:** Per the V3 atomic micro-fold commit
`ebe84954b` "docs(sk-v14-p2-V3): atomic micro-fold (P2-F verb-tense 2
cells)" the V3 dispatch context §1 P2-F summary line declares:

> "2 verb-tense polish cells: §2.10 C10 + §2.13 C13 'Authoring landed
> as Fold-N V2 deliverable' → 'Authoring queued for S-P3 same-wave
> Lock 16 same-commit admission'"
> (`V3/CHALLENGE-CONTEXT.md:21`)

**Executable verification at HEAD `ebe84954b`:**

V3 amended P2-F §2.10 row at `p2f:164` reads (full sentence verified
verbatim):

> "**Scalar-reference target path:line** (Stage-A authoring under
> same-wave Lock 16 same-commit discipline): `crates/bbnf-simd/src/scalar/byte_context_64.rs` —
> `byte_context_64_scalar(prev_chunk: &[u8; 64], cur_chunk: &[u8; 64],
> carry_bytes: usize) -> [u8; 64]` producing the same cross-chunk byte-
> context as the candidate SIMD primitive via byte-by-byte loop with no
> chunk boundary (sibling of existing `crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs:1`
> shape). **Authoring queued for S-P3 same-wave Lock 16 same-commit
> admission per HARDENING-S-P2-V1-CONSOLIDATED §3.4 Fold-4; function
> body lands same-commit with SIMD body at S-P3.**"

V3 amended P2-F §2.13 row at `p2f:197` reads (full sentence verified
verbatim):

> "**Scalar-reference target path:line** (Stage-A authoring under Lock
> 16 same-commit discipline): `crates/bbnf-simd/src/scalar/bcax_64.rs` —
> `bcax_64_scalar(a: u64, b: u64, c: u64) -> u64` returning `(a & !b) ^ c`
> over u8x16 / u64 masks; sibling of existing `crates/bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs:1`
> shape. **Authoring queued for S-P3 same-wave Lock 16 same-commit
> admission per HARDENING-S-P2-V1-CONSOLIDATED §3.4 Fold-5; function
> body lands same-commit with SIMD body at S-P3.**"

Both cells now carry the V2 F-V2-CH4-1 recommended phrasing essentially
verbatim. The verb "landed" (the load-bearing ambiguity) is eliminated;
"queued for S-P3 same-wave Lock 16 same-commit admission" + "function
body lands same-commit with SIMD body at S-P3" unambiguously frames
the target-not-existing-at-HEAD nature and the Lock 16 v+1 same-commit
discipline.

**Diff verification:**

`git diff 4c70b6f19..ebe84954b -- restart/skinny/tranches/sk-v14/research/p2/p2f-grammar-neutral.md`
returns exactly 4 changed lines (2 deletions + 2 insertions at `p2f:164`
and `p2f:197`); `git show --stat ebe84954b` confirms `p2f-grammar-
neutral.md | 4 +-- 2 insertions(+), 2 deletions(-)` (the +4/-2 differential
reflects the 2-cell V3 micro-fold).

**File-at-HEAD verification:**

`find /Users/mkbabb/Programming/bbnf-lang -name "byte_context_64*" -o
-name "bcax_64*" 2>/dev/null` returns empty — both Stage-A target
files DO NOT exist at HEAD (correct per the "queued for S-P3"
framing). `ls /Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-simd/src/scalar/`
enumerates 8 sibling-pattern files including `byte_class_from_eq_set_64.rs`
(C10 sibling) and `bitmap_prefix_xor_64.rs` (C13 sibling) — both
sibling-pattern citations are HEAD-verifiable as required.

**Convergent F-V2-CH1-1 discharge:** The V3 fold also discharges the V2
CH1 finding F-V2-CH1-1 (per V3 dispatch context §1 "Discharges
F-V2-CH1-1 + F-V2-CH4-1 convergently"). The same 2-cell verb-tense
fix resolves both lenses' verb-precision concerns simultaneously; no
divergent edit path.

**V3 CH4 disposition for F-V2-CH4-1:** **DISCHARGED — ACCEPT.** The V2
fold recommendation (rewrite "landed" → "queued for S-P3 same-wave
admission; function body lands at S-P3 per Lock 16") landed verbatim
in the V3 micro-fold. Both §2.10 and §2.13 rows now carry the precise
target-not-existing-at-HEAD framing in isolation as well as in context.
The Lock 16 v+1 same-commit discipline binding is preserved. CH4
discharge is complete; no V4 fold required.

### §2.2 — F-V2-CH4-2 disposition: CF-3 partial §4 mirror

**V2 CH4 §4 F-V2-CH4-2 finding (non-blocking; V2/CH4.md:412-420):**

> "Per §2.6 above: P2-F's V2 §4 mirrors P2-A's per-candidate shape;
> P2-C + P2-D do not. CF-3 is documentation-discipline (per V1 CH4 §4
> line 170); the V2 commit did not propagate it. **V3 fold suggestion
> (non-blocking):** P2-C + P2-D §4 should adopt one-bullet-per-candidate
> CH4 enumeration mirroring P2-A. ACCEPT at V2 because the load-bearing
> CH4 evidence IS present per §2 rows; the §4 mirror is cohesion-not-
> binding."

**V3 disposition path executed:** The V3 dispatch context §1 P2-F
summary scopes V3 to "2 cells in P2-F: §2.10 C10 verb-tense + §2.13
C13 verb-tense" — F-V2-CH4-2 is explicitly outside V3 scope per
"P2-A/B/C/D/E LOCKED at V2 (zero V3 edits)" (`V3/CHALLENGE-CONTEXT.md:3`).
The V3 dispatch context §2 CH4 focus instruction (`V3/CHALLENGE-CONTEXT.md:27`)
poses the disposition choice:

> "F-V2-CH4-2 partial §4 mirror disposition: confirm OK-to-defer to
> S-P3 wave-program work OR flag for V3 fold (V2 aggregator marked it
> non-blocking)."

**Executable verification of current §4 state at HEAD:**

P2-C §4 at `p2c-arch-esoterica.md:86-99` carries 13 REDRESS-binding
bullets (88, 89, 90/W10c, 96/97/98, 82-84, 60-72, 50-55, 28+33, 80,
119/120, 126, plus PEXT pre-block + ISA arch-incompatible notes) — the
shape is **REDRESS-pre-block-keyed**, NOT per-candidate-CH4-keyed.
There are 5 active candidates (C-P2C-2/-3/-4/-5/-8) and 3 demoted
(C-P2C-1/-6/-7); no §4 sub-bullet enumerates per-candidate the
(scalar-ref / checkasm / same-wave-consumer) CH4 admission requirement
in the P2-A §4 enumeration shape.

P2-D §4 at `p2d-substrate-tape.md:160-201` carries 7 REDRESS-and-Lock
sub-sections (§4.1 REDRESS 96/97/98, §4.2 REDRESS 50-55, §4.3 REDRESS
60-72, §4.4 REDRESS 80/82-84, §4.5 REDRESS 88/89, §4.6 Lock 1
substrate-union ceiling, §4.7 CH5 hidden-coupling cross-check) — the
shape is **REDRESS-pre-block-keyed plus Lock-keyed**, NOT per-candidate-
CH4-keyed. Both active candidates (C-P2D-1, C-P2D-2) are addressed by
each sub-section as a pair ("P2-D mitigation: …") but the §4 does not
carry one-bullet-per-candidate CH4 admission enumeration.

P2-A §4 at `p2a-sota-teardown.md:256-266` (V1+V2+V3-LOCKED; zero drift
confirmed) carries the gold-standard one-bullet-per-candidate CH4
enumeration that CF-3 sought as the model.

**V3 CH4 disposition for F-V2-CH4-2:** **CONFIRM OK-TO-DEFER — ACCEPT.**
Per the V3 dispatch context §2 disposition-choice framing, this CH4
lens recommends **OK-to-defer to S-P3 wave-program work** with the
following reasoning:

1. **Load-bearing CH4 evidence is present per §2 rows** of every
   axis file (P2-A §2 rows; P2-C §2 + §2.X rows; P2-D §2 + §1.6(d)
   rows; P2-E §2 rows; P2-F §2 + §2.X.1 rows). The §4-mirror enumeration
   is documentation-cohesion, not CH4-load-bearing.

2. **V3 confirming-cycle discipline binds V3 to verb-tense-only edits**
   per `V3/CHALLENGE-CONTEXT.md:3` ("P2-A/B/C/D/E LOCKED at V2 (zero
   V3 edits)"). Adding §4 per-candidate enumeration to P2-C + P2-D
   would (a) break the V2 LOCK on those axes, (b) trigger a non-
   trivial diff (rather than the V3 atomic micro-fold actually
   committed), and (c) re-open the §3Z chain by mutating a V2-LOCKED
   surface.

3. **The S-P3 wave plan is the natural locus** for per-candidate
   admission-gate enumeration. S-P3's job is to admit/sequence
   candidates per the candidate pool; the per-candidate (scalar-ref /
   checkasm / same-wave-consumer) admission checklist is materially
   the S-P3 wave-program admission-gate manifest, not an S-P2 §4
   risk-discharge stamp. CF-3 was a P2-A documentation-discipline
   recommendation; the natural promotion is to the S-P3 wave-program
   admission manifest where every shortlisted candidate carries the
   3-gate cell explicitly.

4. **The V2 aggregator explicitly marked it non-blocking** (V2 CH4 §4
   F-V2-CH4-2: "ACCEPT at V2 because the load-bearing CH4 evidence IS
   present per §2 rows; the §4 mirror is cohesion-not-binding"). V3
   confirms the non-blocking status: the CH4 strict ACCEPT-rate is
   36/36 = 100.0 % WITHOUT the §4 mirror landing; the §4 mirror
   would polish presentation, not change the verdict.

5. **No CH4 cohort LOCK risk.** Per `ORCHESTRATOR.md §3Z` ("≥95 %
   ACCEPT for two consecutive cycles"), CH4 has now achieved 100.0 %
   strict ACCEPT at V2 AND 100.0 % strict ACCEPT at V3 — the cohort
   LOCK is unconditional, not contingent on F-V2-CH4-2 landing at V3.
   Deferring to S-P3 does not jeopardise the §3Z gate.

**Recommended disposition path:** F-V2-CH4-2 (CF-3 §4 mirror) **defers
to S-P3 wave-program admission manifest** as a per-shortlisted-
candidate (scalar-ref / checkasm / same-wave-consumer) cell. The S-P3
dispatch context should carry this forward as an admission-gate
discipline note ("every shortlisted candidate's admission manifest
carries the 3-gate CH4 cell explicitly per S-P2 CH4 V2 CF-3"). The
S-P2 axis-file §4 sections are correctly REDRESS-pre-block-keyed and
should remain that way; the CH4 per-candidate enumeration belongs in
the wave-program manifest, not the research artefact.

### §2.3 — V1 REVISE chain final discharge confirmation (carry-through verification)

The three V1 REVISE findings (C8 / C10 / C13) all closed at V2; V3
confirms the closure carries through:

- **C8** (V1 REVISE → V2 demoted to §2.X.1; Fold-6 SKIPPED conditional
  on demotion). V3 zero-drift on P2-F's §2.X.1 (verified via the V3
  diff scoped to lines 164 + 197 only); C8's V2 demotion + disposition
  stamp at `p2f:218-229` carries through unchanged. **V3 CH4: ACCEPT —
  carries through V2 discharge.**

- **C10** (V1 REVISE → V2 Stage-A target named at `crates/bbnf-simd/src/scalar/byte_context_64.rs`).
  V3 amendment refines the verb-tense per F-V2-CH4-1 + F-V2-CH1-1
  convergent fix; the Stage-A target path + signature + sibling-file
  pattern are preserved verbatim. **V3 CH4: ACCEPT — carries through
  V2 discharge with V3 precision polish.**

- **C13** (V1 REVISE → V2 Stage-A target named at `crates/bbnf-simd/src/scalar/bcax_64.rs`).
  Same V3 amendment shape as C10. **V3 CH4: ACCEPT — carries through
  V2 discharge with V3 precision polish.**

- **C12** (V1 audit CF-1 reframe: ACCEPT not REVISE; V2 fold landed
  explicit reframe stamps at `p2f:184` + `p2f:305`). V3 zero-drift
  on §2.12 + §4 (verified). **V3 CH4: ACCEPT — V2 reframe carries
  through.**

### §2.4 — F-V2-P1ABC-RERECORD CH2/CH4 dual-gating verification (V3 carry-through)

The V2 CH4 §2.5 confirmed the dual-gating discipline is documented in
HARDENING-S-P2-V1-CONSOLIDATED §2.1 lines 273-289 (Stage-0 of consumer
wave; Cargo invocation `cargo build --release -p bbnf-bench --features
runtime/parse-attribution`; samply interactive record; 11 must-bind
consumers). V3 zero-drift on the consolidated dispatch document is
confirmed: `git diff 4c70b6f19..ebe84954b -- restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md`
returns 0 lines. The dual-gating CH4 cost-ledger entry carries through
V3 unchanged.

**V3 CH4 disposition for §2.4:** **DUAL-GATING CARRIES THROUGH — ACCEPT-FOUNDATIONAL.**

## §3 — V2-LOCKED axis V3-cycle drift audit (all 5 axes)

**V3 dispatch context §2 instruction:** "V3 is verification-only for 5
of 7 axes. Each lens should confirm zero drift via `git diff
4c70b6f19..<V3 SHA> -- <axis file>` returns empty (or only the 2 P2-F
cells for CH1/CH4 verification)."

**Executable verification:**

```
git diff 4c70b6f19..ebe84954b --
  restart/skinny/tranches/sk-v14/research/p2/p2a-sota-teardown.md
  restart/skinny/tranches/sk-v14/research/p2/p2b-dav1d-process.md
  restart/skinny/tranches/sk-v14/research/p2/p2c-arch-esoterica.md
  restart/skinny/tranches/sk-v14/research/p2/p2d-substrate-tape.md
  restart/skinny/tranches/sk-v14/research/p2/p2e-parse-that-gaps.md
| wc -l
```
returns **0** lines — confirmed zero V3 drift across all 5 V2-LOCKED
axes.

`git show --stat ebe84954b` confirms 2 files changed:
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V3/CHALLENGE-CONTEXT.md` (new file, +40 lines)
- `restart/skinny/tranches/sk-v14/research/p2/p2f-grammar-neutral.md` (+2 lines, -2 lines)

P2-A/B/C/D/E ARE NOT IN THE CHANGESET — zero drift confirmed atomically.

**Line-count corroboration:** `wc -l
/Users/mkbabb/Programming/bbnf-lang/restart/skinny/tranches/sk-v14/research/p2/p2*.md`
returns:
- `p2a-sota-teardown.md: 367` (V2 = 367; matches V3 dispatch context §1 expected)
- `p2b-dav1d-process.md: 217` (V2 = 217; matches)
- `p2c-arch-esoterica.md: 164` (V2 = 164; matches)
- `p2d-substrate-tape.md: 254` (V2 = 254; matches)
- `p2e-parse-that-gaps.md: 342` (V2 = 342; matches)
- `p2f-grammar-neutral.md: 360` (V2 = 360; V3 amendment is in-place
  same-line-count edit at lines 164 + 197 — verified)

**V3 CH4 disposition for §3:** **ZERO DRIFT CONFIRMED — ACCEPT.** The
5 V2-LOCKED axes (P2-A 7/7; P2-B process-gate; P2-C 5/5 active; P2-D
2/2 active; P2-E 9/9) carry their V2 CH4 ACCEPT through to V3 verbatim.
The aggregate `7 + 0 + 5 + 2 + 9 = 23` V2-LOCKED CH4 ACCEPTs are
preserved; the V3 P2-F amendment moves 0 of P2-F's 13 ACCEPT cells
(the verb-tense polish refines §2.10 + §2.13 wording without changing
the underlying ACCEPT disposition).

## §4 — V3 critical findings (post-V2-finding-discharge)

### F-V3-CH4-1 (V3 verb-tense fix lands cleanly; F-V2-CH4-1 fully discharged)

The V3 micro-fold at `p2f:164` (C10) + `p2f:197` (C13) lands the
verb-tense correction recommended by F-V2-CH4-1. The new phrasing
("Authoring queued for S-P3 same-wave Lock 16 same-commit admission
per HARDENING-S-P2-V1-CONSOLIDATED §3.4 Fold-N; function body lands
same-commit with SIMD body at S-P3") is precise in isolation as well
as in context, eliminates the "landed" ambiguity, and explicitly binds
the body-authoring to the Lock 16 same-commit discipline at S-P3.

**Cross-cell consistency check:** The V3 dispatch context §1 P2-F
summary line declares Fold-4 + Fold-5 wording converges; verified that
§2.10 (Fold-4, C10) and §2.13 (Fold-5, C13) both carry identical
verb-tense shape ("queued for S-P3 same-wave Lock 16 same-commit
admission per HARDENING-S-P2-V1-CONSOLIDATED §3.4 Fold-N; function
body lands same-commit with SIMD body at S-P3" — modulo Fold-4 vs
Fold-5 identifier). Symmetric and uniform.

**§4 risk-discharge stamp consistency check:** P2-F §4 at `p2f:305`
retains the V2 wording "**CH4 risk on C10, C13 — DISCHARGED V2 via
Fold-4 + Fold-5 Stage-A scalar-reference authoring**" which is
RETROSPECTIVELY ACCURATE — the V2 fold DID land the Stage-A target
naming; V3's edit refines the verb-tense, not the disposition stamp.
The §4 stamp remains accurate at V3.

**V3 CH4 disposition for F-V3-CH4-1:** **ACCEPT — clean discharge.**

### F-V3-CH4-2 (F-V2-CH4-2 OK-to-defer disposition)

Per §2.2: F-V2-CH4-2 (CF-3 §4 per-candidate mirror) is recommended
OK-to-defer to S-P3 wave-program admission manifest. Five reasons
above. V3 confirms the deferral does NOT jeopardise the §3Z LOCK
because the load-bearing CH4 strict ACCEPT-rate is 36/36 = 100.0 %
WITHOUT the §4 mirror landing.

**Recommended S-P3 carry-forward note:** S-P3 dispatch context should
include the line "Every shortlisted candidate's admission manifest
carries the 3-gate CH4 cell explicitly (scalar-ref status / checkasm-
parity expectation / same-wave-consumer NAMED) per S-P2 CH4 V2 CF-3."
This promotes CF-3 from the documentation-discipline scope of an S-P2
axis-file §4 section to the load-bearing scope of the S-P3 wave-program
manifest where it is most useful.

**V3 CH4 disposition for F-V3-CH4-2:** **ACCEPT — OK-to-defer to S-P3.**

### F-V3-CH4-3 (NF-CH6-3/4/5 cross-axis cohesion V3 carry-through)

V2 CH4 §F-V2-CH4-4 documented the NF-CH6-3 + NF-CH6-4 cross-axis
scalar-oracle cohesion via P2-E Gap 6 composition citation at `p2f:81`
+ §2.Y at `p2f:231-239`. V3 zero-drift on these cells confirmed (the
V3 amendment is scoped to `p2f:164` + `p2f:197` only; the V3 diff
shows no edit at `p2f:81` or `p2f:231-239`). The cross-axis cohesion
CH4 cost-reduction (three orthogonal candidates collapse to one
canonical primitive → one Stage-A authoring + one Stage-B checkasm +
one Stage-C lock manifest tie + one Stage-D consumer binding) carries
through V3 unchanged.

**V3 CH4 disposition for F-V3-CH4-3:** **CARRIES THROUGH — ACCEPT.**

### F-V3-CH4-4 (P2-C + P2-D demotion CH4 discriminator preservation V3 carry-through)

V2 CH4 §F-V2-CH4-5 confirmed both P2-C (3 demoted; C-P2C-1/-6/-7) and
P2-D (1 demoted to §1.6(d); C-P2D-3) demotion mechanics preserve CH4
discriminator evidence verbatim, carry explicit disposition stamps,
and retain identifiers for cross-tranche stability. V3 zero-drift on
P2-C + P2-D confirmed (per §3 above). The CH4 evidence preservation
+ identifier-stability discipline carries through V3 unchanged.

**V3 CH4 disposition for F-V3-CH4-4:** **CARRIES THROUGH — ACCEPT.**

## §5 — V3 cost-ledger per-stage update (preservation cycle)

V2 published 180/180 = 100.0 % across 36 eligible candidates × 5 stages.
V3 update:

| Stage | V2 binding | V2 pass | V3 binding (post-V3 verb-tense fix) | V3 pass |
|---|---|---:|---|---:|
| Stage A (scalar reference) | PRESENT or REQUIRED-with-named-target-path:line + signature shape | 36/36 (C10 + C13 Stage-A target named per Fold-4/-5) | PRESENT or REQUIRED-with-target-path:line + signature + sibling-pattern + "queued for S-P3 same-commit admission" framing | 36/36 (C10 + C13 verb-tense refined; underlying target+signature+sibling-pattern unchanged) |
| Stage B (checkasm cell) | NAMED or IMPLIED via existing cell | 36/36 | NAMED or IMPLIED via existing cell | 36/36 (V3 no-op) |
| Stage C (Lock 16 cite) | Manifest tie via Lock 16 line | 36/36 | Manifest tie via Lock 16 line + explicit Lock 16 same-commit binding for C10 + C13 | 36/36 (V3 strengthens the Lock 16 same-commit binding language for C10 + C13 explicitly) |
| Stage D (same-wave consumer) | NAMED with concrete consumer path; demoted candidates removed from denominator | 36/36 | NAMED with concrete consumer path; demoted candidates removed from denominator | 36/36 (V3 no-op) |
| Stage E (manifest + substrate) | substrate-target ∈ {`local_temp_only`, `existing_tape`, `direct_sink`, `admitted_fact_output`} | 36/36 | same | 36/36 (V3 no-op) |

**V3 aggregate cost-ledger: 180/180 = 100.0 %** on the 36 eligible
candidates × 5 stages = 180 cells. The V3 verb-tense fix at Stage A
strengthens the cell's clarity-in-isolation without changing the
underlying disposition; the C and Stage A binding now carries explicit
Lock 16 same-commit S-P3 admission language for C10 + C13.

V2 → V3 → LOCK trajectory: V3 is the second consecutive ≥95% cycle;
F-V2-CH4-1 discharged via V3 atomic micro-fold; F-V2-CH4-2 OK-to-
defer to S-P3 wave-program; CH4 LOCK at V2 + V3 strict 100.0 %.

## §6 — V3 fold recommendations + S-P3 carry-forward

CH4 has converged at V3 with two consecutive ≥95% cycles (V2 100.0 %
+ V3 100.0 %). No V4 CH4 fold is required. Two S-P3 carry-forward
notes:

1. **S-P3 wave-program admission manifest carries the 3-gate CH4 cell
   explicitly per shortlisted candidate** (scalar-ref status / checkasm-
   parity expectation / same-wave-consumer NAMED). This promotes
   F-V2-CH4-2 / CF-3 from S-P2 §4 documentation-discipline to S-P3
   wave-program admission-gate manifest, the natural locus.

2. **Lock 16 same-commit discipline binding for C10 + C13 body-authoring
   is now explicit in P2-F §2.10 + §2.13** (V3 fold lands "function
   body lands same-commit with SIMD body at S-P3"). S-P3's first
   SK-V14 implementation wave admitting C10 or C13 must land the
   scalar-ref + checkasm cell + SIMD body in one commit per Lock 16
   v+1; the V3 P2-F wording binds this discipline at the candidate-
   row scope.

3. **CH4 V3 does not pre-block any REDRESS route.** Per V3 dispatch
   context §2 CH3 cross-check: F-V3-CH4-1/2/3/4 all preserve V2's
   REDRESS-safe discipline; no V3 finding re-opens any pre-blocked
   route. CH3 cross-check holds at V3.

## §7 — Convergence vote (V3 confirming cycle)

Per `PASS-2-RESEARCH.md §3 CH4` + `ORCHESTRATOR.md §3Z`:

- **ACCEPT (strict):** 36/36 = **100.0 %** eligible candidates pass
  the load-bearing CH4 axis (scalar-reference status PRESENT or
  REQUIRED-with-named-target-path:line + signature + sibling-pattern +
  "queued for S-P3 same-commit admission" framing + checkasm-parity
  expectation NAMED/IMPLIED + same-wave-consumer NAMED).
- **ACCEPT (with alternative-satisfaction):** 36/36 = **100.0 %** (no
  change from V2; all alternative-satisfaction routes preserved).
- **REVISE:** 0 (V2's 2 non-blocking findings: F-V2-CH4-1 DISCHARGED
  by V3 atomic micro-fold; F-V2-CH4-2 OK-TO-DEFER to S-P3 wave-program
  admission manifest per §2.2).
- **REJECT:** 0.
- **Per-§ V3 ACCEPT rate:** §2.A P2-A 7/7 (V1+V2+V3 chain LOCK);
  §2.C P2-C 5/5 active (V2+V3 chain LOCK); §2.D P2-D 2/2 active
  (V2+V3 chain LOCK); §2.E P2-E 9/9 (V1+V2+V3 chain LOCK); §2.F P2-F
  13/13 (V2 fold landed 3 V1 REVISE closures; V3 fold landed verb-
  tense precision; F-V2-CH4-1 discharged; F-V2-CH4-2 OK-to-defer).
- **V3 aggregate cost-ledger:** 180/180 = 100.0 % across 5 Stage-A..-E gates.
- **§3Z convergence:** **V2 100.0 % + V3 100.0 % = two consecutive
  ≥95% cycles for CH4 → cohort §3Z LOCK eligible.**

**CH4 V3 vote: ACCEPT — clears the ≥ 95% §3Z gate for the second
consecutive cycle.** **CH4 COHORT LOCK CONFIRMED.** No V4 fold
required. Two S-P3 carry-forward notes for the wave-program admission
manifest. The load-bearing axis (scalar-ref + checkasm + same-wave-
consumer per candidate) is at 100% at both V2 and V3.

**Cohort LOCK chain summary:**
- V1 CH4: 91.9 % strict / 94.6 % alternative — below §3Z threshold.
- V2 CH4: 100.0 % strict — first ≥95% cycle (LOCK candidate).
- V3 CH4: 100.0 % strict — second consecutive ≥95% cycle → **LOCK CONFIRMED**.

## §8 — Sources (every cite verified this turn)

- `restart/skinny/tranches/sk-v14/research/p2/hardening/V3/CHALLENGE-CONTEXT.md`
  §0-§4 (V3 dispatch authority; confirming-cycle binding; CH4 V3 focus per §2 dispatched).
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V2/CHALLENGE-CONTEXT.md`
  §0-§4 (V2 dispatch carry-forward).
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V1/CHALLENGE-CONTEXT.md`
  §0-§4 (V1 dispatch carry-forward).
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V2/CH4.md` (V2
  CH4 carry-forward; 100.0 % strict; F-V2-CH4-1 + F-V2-CH4-2 non-
  blocking findings).
- `restart/skinny/tranches/sk-v14/research/p2/hardening/V1/CH4.md` (V1
  CH4 carry-forward; 91.9 % strict / 94.6 % alt; 3 REVISE on C8/C10/C13 + CF-1 reframe).
- `restart/skinny/tranches/sk-v14/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md`
  §2.1 (F-V2-P1ABC-RERECORD CH2/CH4 dual-gating), §3.2 Fold-2 (C8 demotion),
  §3.4 Fold-4/-5/-6 (Stage-A scalar-reference authoring).
- `restart/prompts/skinny/PASS-2-RESEARCH.md §3 CH4` lines 119-124
  (lens definition; binding: scalar-ref + checkasm-parity + same-wave consumer).
- `restart/prompts/ORCHESTRATOR.md §3W + §3Z` (universal CH4 def + LOCK
  convergence rule: ≥95 % × 2 consecutive cycles).
- `restart/skinny/tranches/sk-v14/research/p2/p2a-sota-teardown.md`
  (V1+V2+V3-LOCKED at 367 lines; zero V3 drift verified per `git
  diff 4c70b6f19..ebe84954b -- p2a-sota-teardown.md` returning 0 lines).
- `restart/skinny/tranches/sk-v14/research/p2/p2b-dav1d-process.md`
  (V2+V3-LOCKED at 217 lines; zero V3 drift verified).
- `restart/skinny/tranches/sk-v14/research/p2/p2c-arch-esoterica.md`
  (V2+V3-LOCKED at 164 lines; zero V3 drift verified; §4 REDRESS-pre-
  block-keyed shape at `:86-99` retained — F-V2-CH4-2 deferred to S-P3).
- `restart/skinny/tranches/sk-v14/research/p2/p2d-substrate-tape.md`
  (V2+V3-LOCKED at 254 lines; zero V3 drift verified; §4 REDRESS-+-
  Lock-keyed shape at `:160-201` retained — F-V2-CH4-2 deferred to S-P3).
- `restart/skinny/tranches/sk-v14/research/p2/p2e-parse-that-gaps.md`
  (V1+V2+V3-LOCKED at 342 lines; zero V3 drift verified).
- `restart/skinny/tranches/sk-v14/research/p2/p2f-grammar-neutral.md`
  (360 lines V3; §2.10 C10 verb-tense fix at `:164`; §2.13 C13 verb-
  tense fix at `:197`; §4 risk-discharge stamps at `:305` retain
  V2 wording — retrospectively accurate; §2.12 CF-1 reframe at `:184`
  + §2.X.1 C8 demotion at `:218-229` + §2.Y NF-CH6-4 cross-axis at
  `:231-239` all V2-state preserved at V3).
- `restart/locks/LOCKS.md:48-90` (Lock 1 v+1 substrate-target manifest);
  `:282-340` (Lock 16 SIMD/ASM allowlist + scalar-reference + checkasm-
  parity + same-wave-consumer + close-state vocabulary).
- Cross-cycle precedent: `restart/skinny/tranches/sk-v14/research/p1/hardening/V3/CH4.md`
  (S-P1 V3 CH4 carry-through pattern at sister-track sk-v14).
- Host-side verification this turn:
  - `git -C /Users/mkbabb/Programming/bbnf-lang log --oneline -10` (HEAD `ebe84954b`).
  - `git -C /Users/mkbabb/Programming/bbnf-lang show --stat ebe84954b`
    (2 files changed: V3 CHALLENGE-CONTEXT + P2-F amendment 4 lines
    +/-2 each direction; P2-A/B/C/D/E not in changeset = zero V3 drift).
  - `git -C /Users/mkbabb/Programming/bbnf-lang diff 4c70b6f19..ebe84954b
    -- restart/skinny/tranches/sk-v14/research/p2/p2{a,b,c,d,e}-*.md
    | wc -l` returned 0 — confirms 5 V2-LOCKED axes carry through V3 unchanged.
  - `git -C /Users/mkbabb/Programming/bbnf-lang diff 4c70b6f19..ebe84954b
    -- restart/skinny/tranches/sk-v14/research/p2/p2f-grammar-neutral.md`
    returned the 2-cell V3 verb-tense diff verbatim (§2.10 + §2.13).
  - `wc -l restart/skinny/tranches/sk-v14/research/p2/p2*.md` confirmed
    line-counts match V3 dispatch context §1 expectations (367/217/164/254/342/360).
  - `find /Users/mkbabb/Programming/bbnf-lang -name "byte_context_64*"
    -o -name "bcax_64*" 2>/dev/null` returned empty — confirmed
    Fold-4 + Fold-5 Stage-A target files DO NOT exist at HEAD; V3
    cites correctly frame them as "queued for S-P3 same-wave Lock 16
    same-commit admission" not "exists at HEAD" or "landed at V2".
  - `ls /Users/mkbabb/Programming/bbnf-lang/skinny/crates/bbnf-simd/src/scalar/`
    enumerated 8 existing scalar files (`bitmap_next_set_bit.rs`,
    `bitmap_prefix_xor_64.rs`, `bulk_emit_positions_64.rs`,
    `byte_class_from_eq_set_64.rs`, `byte_class_from_table_64.rs`,
    `eob_pad_clamp.rs`, `mod.rs`, `swar_8byte.rs`) confirming
    sibling-file patterns cited in §2.10 + §2.13 exist at HEAD.
  - Cell-content verification at HEAD via `grep -n
    "Stage-A\|landed\|queued for S-P3\|F-V2-CH4" p2f-grammar-neutral.md`
    confirmed §2.10 (`:164`) + §2.13 (`:197`) both carry "queued for
    S-P3" verb-tense; no occurrences of "landed as Fold-N V2
    deliverable" wording remain in C10/C13 §2 rows.
