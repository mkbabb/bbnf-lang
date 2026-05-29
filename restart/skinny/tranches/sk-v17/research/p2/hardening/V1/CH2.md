# SK-V17 S-P2 CHALLENGE — CH2 GENERALITY (V1)

Lens: CH2 GENERALITY. Cycle: V1. Date: 2026-05-29.
Reviewer scope: PASS-2-RESEARCH §3 CH2 + ORCHESTRATOR §3W. Per-candidate Lock-14
grammar-neutral verdict; a JSON/CSS-only primitive with no grammar-neutral
byte-set / classifier / tape expression is REVISE (re-express) or REJECT (drop);
the NEON class leaf must be a SHARED kernel (JSON + CSS witnessed; Sheets/BBNF-self
asserted-by-construction, proof deferred to SK-V18).
Inputs reviewed end-to-end: `p2f-grammar-neutral.md` (the load-bearing P2-F verdict
source), `p2b-dav1d-process.md` §2/§3, `p2c-arch-esoterica.md` §2/§3,
`p2d-substrate-tape.md` §2/§3, `p2e-parse-that-gaps.md` §2/§3.
Master HEAD `0ae1caa52`; S-P1 profile locked `0ae1caa52`.

## §0 — Verdict summary

CH2 reviews **23 candidate/section rows** across five artefacts. Every candidate in
the pool carries a grammar-neutral verdict somewhere in the S-P2 set, and P2-F (the
designated Lock-14 owner) carries a verdict for every CF-row. The pool is
**grammar-neutral by construction** — there is NO JSON/CSS-overfit primitive that
lacks a byte-set / classifier / tape expression. The shared-NEON-leaf requirement is
SATISFIED by `byte_class_from_eq_set_64_neon` (a real NEON body parameterised by a
`set: &[u8]`, verified at `aarch64/byte_class_from_eq_set_64.rs:33`) reached through
the `select_classifier(alphabet)` Lock-14 vehicle (`dispatch.rs:42`).

CH2 issues **3 REVISE** dispositions, all "re-express / tighten the framing"
(no candidate is dropped on generality grounds), and **0 REJECT-on-generality**.
The REVISEs are: (1) a verified factual inconsistency in P2-B's named primitive vs
the admissible backend P2-F establishes; (2) a missing per-grammar parameterisation
constraint that P2-E itself asks P2-F to enforce; (3) P2-C's verdict scope being
narrower than its own candidate list. Two candidates (CF-4a/CF-4b ≡ C5/C6 ≡ G4) are
grammar-neutral-in-shape but CSS-orphan — their generality is sound but moot pending
a re-profile; CH2 ACCEPTs the generality verdict and defers the orphan-block to CH1/CH4.

| Disposition | Count |
|---|---|
| ACCEPT | 17 |
| REVISE | 3 |
| REJECT | 0 |
| **Total candidate/section rows** | **23** (4 are non-candidate / negative-space rows recorded for completeness; all ACCEPT) |

ACCEPT-rate on candidate-bearing rows (excluding the 4 negative-space rows that
require no generality work): 17 / 20 = **85%**. Counting all 23 rows: 20 / 23 = **87%**.

## §1 — The two CH2 gates, applied

### Gate A — does every candidate carry a P2-F grammar-neutral verdict?

YES for every CF-row in P2-F §2/§3 (CF-1..CF-4b + CF-0). The candidate-bearing
artefacts P2-B/C/D/E each ALSO carry their own §3 grammar-neutrality block, and each
defers the binding verdict to P2-F (P2-B §3 "Defer the full cross-grammar mapping to
P2-F"; P2-C §3 "the CH2 / Lock 14 disposition"; P2-D D5 "Flagged for P2-F"; P2-E §3
"P2-F should verify G1/G2's parameterisation"). The cross-references resolve: P2-F
CF-2 covers C-B1 / C1 / C2 / G3; CF-1 covers C-B2 / D1 / D2; CF-3 covers D3's
placement note; CF-4a covers C5 / G4; CF-4b covers C6. The one structural gap is that
P2-F's CF-row set does NOT separately name G1 (`comment_body_mask_64`) and G2
(`bracket_depth_mask_64`) — it folds them into CF-2 as "the parse-that gap P2-E/P2-C
own" (P2-F §1.2). P2-E supplies the G1/G2 verdicts itself (§3, both GRAMMAR-NEUTRAL).
This is acceptable folding, not an orphaned candidate — see §2 REVISE-2 for the one
tightening it needs.

### Gate B — is the NEON class leaf shared (JSON + CSS + Sheets + BBNF-self)?

The shared leaf is `byte_class_from_eq_set_64` via `select_classifier(alphabet)`. CH2
verified at source:
- `byte_class_from_eq_set_64_neon(src: &[u8;64], set: &[u8]) -> u64` takes the byte
  set as a **runtime parameter** — the alphabet is the ONLY grammar-specific datum
  (`aarch64/byte_class_from_eq_set_64.rs:33`, verified: four `vld1q_u8` stripes,
  `vceqq_u8`-fan, `vorrq_u8`-reduce). The kernel carries no grammar role.
- `select_classifier(alphabet: &'static [u8; 64])` (`dispatch.rs:42`) is the Lock-14
  vehicle; the alphabet is caller data, matching `LOCKS.md:393-395`.
- JSON's structural set `b"{}[],:\""` (`json/scan.rs:5`) and the CSS delimiter sets
  (`b";{}"` / `b"{};"` / `b":{};"`, ≤8 bytes each) are different `set` arguments to
  the SAME kernel — JSON+CSS witnessed.
- Sheets/BBNF-self: asserted-by-construction (the kernel is grammar-parametric by
  type), proof deferred to SK-V18 per Lock 14 phrase #2 (`LOCKS.md:386-387`). NO
  candidate uses fleet-wide/four-grammar wording — P2-F §1.5 and P2-D D2 / P2-E G3
  all scope to "JSON+CSS-witnessed". This is correct CH2 hygiene; the shared-leaf
  requirement is met on the witnessed pair and honestly scoped beyond it.

**CH2 verified the load-bearing neutrality split (P2-F §1.2):** the lo6-table NEON
backend (`classify_tbl4`, the JSON path) is NOT transferable to CSS. Independently
computed mod-0x3f slots: `;`(0x3b)→59, `{`(0x7b)→59 — **collision confirmed**, so
`lo6_table_admissible` (`dispatch.rs:101-113`, verified) returns false for every CSS
structural alphabet containing the `;{` pair, and `select_classifier` correctly falls
back. The admissible shared CSS leaf is therefore the **eq-set fan**
(`byte_class_from_eq_set_64`, real NEON body) NOT the lo6 table
(`byte_class_from_table_64_neon`, which is a **scalar passthrough today** —
`aarch64/byte_class_from_table_64.rs:1-4`, verified: it calls `..._scalar`). This
distinction is the crux of CH2's REVISE-1.

## §2 — Per-candidate dispositions (path:line + concrete fix)

### P2-F (the Lock-14 owner) — `p2f-grammar-neutral.md`

**CF-1 (tape-append + `ValueRef`) — ACCEPT.** `p2f:149-160`. Grammar-neutral
(JSON+CSS-witnessed), conditional on §1.4 routing-derived-from-grammar. The append
op is grammar-free; `ValueRef<G: EventGrammar>` is generic over the grammar
(`mod.rs:175`, verified the type is grammar-parametric). The condition (retire
`W5C_REQUEST_FACT_PROFILES`, no relocated per-rule branching into projection DATA) is
the correct Lock-14 trap fence and is named precisely. The conditional-REVISE escape
hatch ("else REVISE") is exactly the CH2-compliant framing.

**CF-2 (structural-membership classifier) — ACCEPT.** `p2f:186-197`. Grammar-neutral
at the interface; the BACKEND-choice neutrality crux (§1.2) is correctly identified and
the candidate is held to the admissible eq-set/256-table primitive, NOT lo6-reuse. This
is the strongest CH2 row in the pool — it pre-empts the exact JSON-overfit trap
(silently scalar-falling-back lo6 on CSS) that a naive candidate would carry. The
shared-leaf requirement (Gate B) is met here.

**CF-3 (commit-by-construction Alt-mode) — ACCEPT (generality).** `p2f:218-223`.
Grammar-neutral codegen property derived from `BackendRule` Alt shape, JSON-witnessed.
The weak/post-CF-1 P1 antecedent is a CH1/CH4 concern, not CH2 — generality is sound.

**CF-4a (udot 4-digit decode) — ACCEPT (generality); orphan to CH1.** `p2f:242-252`.
Grammar-neutral shape (4-ASCII-digit→u32 is the `byte_class_from_range_64` digit-run
family, `LOCKS.md:426-431`) — NOT CSS-overfit. The CSS-orphan status is correctly
flagged for CH1/CH4 (no benched antecedent). CH2's only concern — is the generality
verdict honest? — is satisfied: "GRAMMAR-NEUTRAL ... BUT CURRENTLY ORPHAN" is the
correct disposition. No CH2 action.

**CF-4b (i8mm dimension decode) — ACCEPT (generality); REJECT-on-evidence to CH1.**
`p2f:267-274`. Grammar-neutral in shape (digit-run family); the no-P1-antecedent
REJECT is CH1's, not CH2's. Generality verdict honest.

**CF-0 (negative space) — ACCEPT.** `p2f:276-288`. The no-unicode / no-dispatch-self /
no-FNV findings are correctly framed; the UTF-8-continuation classifier is correctly
noted as "grammar-neutral in the abstract, but JSON/other-grammar-only here" — exactly
the CH2 distinction between abstract neutrality and witnessed antecedent. ACCEPT.

### P2-B (dav1d process) — `p2b-dav1d-process.md`

**C-B1 (`byte_class_from_eq_set_64`) — REVISE-1.** `p2b:115-143` + §3 `p2b:`(C-B1 §3
bullet). The grammar-neutral verdict is correct and well-grounded (byte-set membership
over arbitrary `set: &[u8]`, JSON+CSS+Sheets+BBNF-self all "just different `set`
arguments"). CH2 ACCEPTs the GENERALITY content. The REVISE is a **factual framing
defect that bears directly on the shared-NEON-leaf claim**: C-B1's §2 shape line names
the primitive as the recognition-scan kernel and its §2 "Same-wave consumer" says it is
"`wired` for JSON (`json/scan.rs:219` lineage)". But JSON's wired NEON path is the **lo6
`classify_tbl4` / `byte_class_from_table_64`** backend (`json/scan.rs:9-30`, the
`STRUCTURAL_CLASS_TABLE_LO6` const, verified), NOT `byte_class_from_eq_set_64`. P2-F
§1.2 establishes — and CH2 independently verified — that JSON rides lo6 (admissible)
while CSS must ride eq-set (lo6-inadmissible). C-B1 conflates the two into one "wired
for JSON" claim, which would let an S-P3 reader believe the eq-set kernel is already
JSON-exercised when JSON actually exercises the table kernel. **Concrete fix:** in
`p2b:131-138` (the Same-wave-consumer + P1-antecedent bullets), split the claim — state
that the *shared interface* `select_classifier(alphabet)` is JSON-wired, that JSON's
admissible BACKEND is the lo6 table while CSS's admissible backend is `eq_set` (the
lo6 alphabet collides mod 0x3f — cite the `;`/`{`→slot 59 collision and
`dispatch.rs:101`), and that the eq-set NEON body's current non-JSON exercise is its
checkasm corpus-parity test (`checkasm_byte_class_from_eq_set_64.rs:300` twitter
corpus), not a live JSON prod path. This keeps C-B1's generality verdict (correct) but
removes the false "already JSON-wired" implication for the eq-set kernel specifically.
Without the fix, the shared-leaf claim (Gate B) reads as already-discharged when it is
the very thing the CSS wave must wire.

**C-B2 (`push_plain_offset` tape-append) — ACCEPT.** `p2b:145-165` + §3. CH2 note: the
C-B2 §3 grammar-neutral bullet is present in the artefact's §3 (the tape append is
grammar-free, JSON-ridden). Generality sound; matches CF-1. ACCEPT.

**C-B3 (orphan udot) — ACCEPT (generality).** `p2b:167-181`. Process-rejected; the
generality dimension (a 4-digit decode is neutral) is not contested by CH2; the orphan
disposition is CH1/CH4. ACCEPT.

**C-B0 (admission process, gate table) — ACCEPT, with commendation.** `p2b:183-210`.
The G6 gate row ("grammar-neutral ... byte-set / classifier / tape op, not a JSON/CSS
role (Lock 14)", with the verdict bound to the P2-F output) institutionalises the CH2
requirement as a per-candidate admission gate S-P3 enforces. This is exactly what CH2
wants: the grammar-neutral verdict is not an after-the-fact review but a structural
admission gate. ACCEPT.

### P2-C (arch esoterica) — `p2c-arch-esoterica.md`

**C1 (lo6 TBL) — ACCEPT.** `p2c:128-159` + §3 `p2c:`(C1 §3 bullet). The §3 verdict
"GRAMMAR-NEUTRAL but ALPHABET-INADMISSIBLE for CSS" is the correct CH2 framing: the
kernel is neutral, the lo6 backend is JSON-alphabet-admissible only, and the honest CSS
answer is the C2 eq-set route. CH2 independently verified the `;`/`{`→slot-59 collision.
ACCEPT.

**C2 (eq-set fan) — ACCEPT.** `p2c:161-189` + §3. The primary CSS shared-NEON-leaf
route; §3 verdict "GRAMMAR-NEUTRAL, JSON+CSS witnessed" with the alphabet as the only
grammar-specific datum, citing `select_classifier(alphabet)`. This is the Gate-B
shared leaf. CH2 verified the NEON body is real (not a passthrough). ACCEPT.

**C3 (shrn movemask) — ACCEPT (generality).** `p2c:191-209`. A sub-task of C1/C2, no
independent grammar surface — movemask bit-packing is grammar-free. No §3 row needed;
folds under C1/C2. ACCEPT.

**C4 (host CTZ extract) — ACCEPT (generality).** `p2c:210-225`. Index-extract on the
shared scan mask; grammar-free. REDRESS-89 bulk-consumer flag is CH3's. ACCEPT.

**C5 (UDOT 4-digit) / C6 (i8mm) — ACCEPT (generality); orphan/REJECT to CH1.**
`p2c:226-260`. Grammar-neutral digit-run shape; orphan-block and net-new-no-antecedent
are CH1/CH4. ACCEPT on generality.

**P2-C §3 scope — REVISE-3.** `p2c:280-291`. P2-C's §3 grammar-neutrality block writes
explicit verdicts for ONLY **C2 and C1** ("Grammar-neutral verdict per candidate", then
two bullets: C2, C1). C3, C4, C5, C6 receive NO §3 grammar-neutral verdict line in P2-C
— they are dispositioned only in the §2 summary table's "Verdict" column (which carries
the SIMD-admission verdict, not the Lock-14 generality verdict). PASS-2-RESEARCH §2.1
makes §3 "Grammar-neutrality (each candidate: JSON-only or CSS/Sheets/BBNF-self
generalisable)" — **each** candidate. C3/C4 are trivially neutral (mask/CTZ ops), C5/C6
are neutral-in-shape-but-orphan; the verdicts exist implicitly and in P2-F, but P2-C's
own §3 must carry one line per candidate or it fails the §2.1 schema for those rows.
**Concrete fix:** in `p2c:280-291` add four one-line §3 verdicts — C3 "grammar-neutral
sub-task (movemask is grammar-free bit-packing; folds under C1/C2)"; C4 "grammar-neutral
fold-only (CTZ index-extract is grammar-free)"; C5/C6 "grammar-neutral digit-run shape
(`byte_class_from_range_64` family) but CSS-orphan / no-antecedent — deferred to CF-4a/
CF-4b verdict". This is a completeness fix, not a content change; the verdicts are not in
dispute, but the §2.1 per-candidate schema requires them present in P2-C's §3.

### P2-D (substrate + tape) — `p2d-substrate-tape.md`

**D1 (`push_plain_offset` emit op) — ACCEPT.** `p2d:186-208`. GENERALISABLE; the op is
grammar-free, per-grammar datum is which positions push (from `BackendRule`,
`lower/offset_tape.rs`). Matches CF-1. ACCEPT.

**D2 (lazy `ValueRef` projection) — ACCEPT.** `p2d:209-236`. GENERALISABLE; the view
emitter walks one `BackendRule` shape for JSON+CSS, kind-from-source-byte is the
grammar-neutral mechanism, per-grammar datum is the byte→kind table derived from the
grammar. Correctly scopes Sheets/BBNF-self as asserted-by-construction. Matches CF-1.
ACCEPT.

**D3 (O(1) checkpoint/truncate) — ACCEPT.** `p2d:237-261`. GENERALISABLE; `offsets.len()`
/`truncate` is a generic `Vec<u32>` op with no grammar knowledge. Correctly separates the
substrate mechanism (D3) from the commit-placement codegen decision (CF-3/alphaE C3).
ACCEPT.

**D4 (one-shot SIMD reservation) — ACCEPT.** `p2d:262-288`. GENERALISABLE; `CapacityPlan`
is grammar-free, the SIMD count reuses the shared `select_classifier(alphabet)` kernel
with the CSS alphabet as the only per-grammar datum. The fallback-on-mod-0x3f-collision
note is consistent with the verified lo6 finding. ACCEPT.

**D5 (sparse-flag side-table) — ACCEPT.** `p2d:289-314`. GENERALISABLE-WITH-GUARD. This
is the SHARPEST CH2-aware row in P2-D: it identifies the exact Lock-14 trap (flag
SEMANTICS becoming a relocated `W5C_REQUEST_FACT_PROFILES` per-rule catalogue in flag
form) and gates admission on each flag bit naming the `.bbnf`/`BackendRule` branch tag it
derives from. The guard IS the CH2 re-express requirement, stated by the author. CH2
ACCEPTs — the candidate is admissible only under the named guard, which is the correct
conditional-neutral framing (isomorphic to CF-1's §1.4 condition). No CH2 fix needed;
the guard already does CH2's work.

**D6 (REJECT-on-sight, no second substrate) — ACCEPT.** `p2d:315-...`. Negative-space
row; the no-second-substrate / no-sidecar / no-cross-call-state assertions are Lock-1
(CH5's domain) but the "Grammar-neutral verdict: N/A — REJECTED by Lock 1" line is
correct. ACCEPT.

### P2-E (parse-that gaps) — `p2e-parse-that-gaps.md`

**G1 (`comment_body_mask_64`) — REVISE-2.** `p2e:102-128` (§2) + `p2e:231-235` (§3).
The §3 verdict is correct ("GRAMMAR-NEUTRAL ... by digraph parameterisation"). But the
§2 SHAPE and the scalar-reference sketch (`p2e:107-124`) **hard-code the `/*`/`*/`
digraph into the primitive body**: the scalar sketch tests `src[i]==b'/' && src[i+1]==b'*'`
and `src[i]==b'*' && src[i+1]==b'/'` as literal CSS bytes, and the signature is
`fn comment_body_mask_64(src: &[u8;64], in_comment_carry: bool)` — it takes NO open/close
digraph parameter. The §3 verdict ASSERTS the neutral framing ("must be parameterised by
the open/close digraph `(open:[u8;2], close:[u8;2])`, NOT hard-coded to `/*`/`*/`") but
the §2 candidate SHAPE — the load-bearing artefact per §2.1 — contradicts it: the
signature has no digraph parameter and the sketch is CSS-pinned. As written, the
*candidate* is CSS-overfit even though the *verdict* claims neutrality. P2-E itself
anticipates this — its §3 closes "P2-F should verify G1/G2's parameterisation is
genuinely per-grammar (digraph for G1...) and not silently CSS-pinned" (`p2e:236-238`) —
and the candidate as drafted is silently CSS-pinned. **Concrete fix:** amend the §2 G1
signature at `p2e:102` to `fn comment_body_mask_64(src:&[u8;64], open:[u8;2], close:[u8;2],
in_comment_carry:bool) -> (u64,bool)` and rewrite the scalar sketch (`p2e:107-124`) to
test `src[i]==open[0] && src[i+1]==open[1]` / `==close[0..2]` instead of literal `/`/`*`.
This makes the §2 shape match the §3 verdict and discharges the per-grammar
parameterisation P2-E asked P2-F to verify. (CF-2 in P2-F folds G1 into "the parse-that
gap P2-E/P2-C own" without separately certifying the digraph parameter, so P2-F does NOT
catch this — CH2 must, since the candidate shape is the load-bearing artefact.)

**G2 (`bracket_depth_mask_64`) — ACCEPT.** `p2e:130-164` (§2) + `p2e:239-244` (§3). The
§2 signature `fn bracket_depth_mask_64(open_mask:u64, close_mask:u64, depth_carry:i32)`
takes the open/close MASKS as parameters — the bracket bytes are already abstracted out
(they are produced upstream by the alphabet-driven `byte_class_from_eq_set_64`). So G2,
unlike G1, is genuinely byte-set-parameterised at the signature level: the primitive sees
only masks, never literal CSS bracket bytes. §3 correctly calls it "the canonical Lock-14
primitive" (nested-bracket balance generalises to JSON arrays/objects, BBNF `()`/`[]`,
Sheets formula parens). The shared-leaf hygiene is exemplary. ACCEPT.

**G3 (`scan_components_to_index`) — ACCEPT.** `p2e:166-184` (§2) + `p2e:225-230` (§3).
The explicit Lock-14 neutrality vehicle via `select_classifier(alphabet)`; the delimiter
alphabet is the only per-grammar datum; lives in the per-grammar consumer
(`runtime/.../scan.rs`), NOT bbnf-simd, so JSON policy never enters the generic crate
(PASS-2 §8.5). Isomorphic to `json/scan.rs:22`. This is the shared assembler that
consumes G1/G2 and produces the Lock-1 `Vec<u32>` index. ACCEPT.

**G4 (`parse_4_digits` checkasm gate) — ACCEPT (generality); orphan to CH1.**
`p2e:186-200` + `p2e:245-249`. Grammar-neutral 4-digit decode; CSS-orphan. CH2 verified
`checkasm_digit_mac` is absent (`ls tests/` shows no `digit` file). Generality sound;
orphan-block is CH1/CH4. ACCEPT.

**G5 (FNV/hex non-candidate) — ACCEPT.** `p2e:201-211` + §3 N/A. Correctly recorded as a
non-candidate that retires with the fact-stream; no NEON hex/FNV primitive proposed.
ACCEPT.

## §3 — Cross-artefact consistency (CH2-relevant)

- **The shared-NEON-leaf identity is consistent and correct across P2-B/C/D/E/F.** All
  five name `byte_class_from_eq_set_64` (via `select_classifier(alphabet)`) as the
  admissible CSS scan kernel and lo6/`classify_tbl4` as the JSON-admissible-only backend.
  The one inconsistency is C-B1's conflation (REVISE-1).
- **The lo6 collision is the single most-cited neutrality fact** (P2-C C1, P2-D D4, P2-F
  §1.2, P2-E implicitly via the eq-set route) and CH2 independently re-derived it:
  `;`(0x3b) and `{`(0x7b) both → lo6 slot 59. It holds.
- **Sheets/BBNF-self scoping is uniformly honest.** No candidate in any artefact uses
  fleet-wide/four-grammar wording; all scope to JSON+CSS-witnessed with SK-V18 deferral.
  Gate B's "must be shared (JSON+CSS+Sheets+BBNF-self)" is satisfied in the
  asserted-by-construction sense the locks permit (`LOCKS.md:386-387`), and CH2 does NOT
  require a live Sheets/BBNF rider in SK-V17 (P2-F §1.5 correctly states `sheets_witness`
  is a 24-LOC byte-classification trait with no `.bbnf`/`BackendRule`, not a projection
  rider).

## §4 — Orphan-REVISE tracking (for §3Z convergence)

Three REVISEs, all with a concrete fix and a named owner artefact — none is an orphan
REVISE (each names path:line + the exact textual change):
1. **REVISE-1** — P2-B C-B1 `p2b:131-138`: split the "wired for JSON" claim into
   shared-interface (JSON-wired) vs eq-set-backend (CSS-bound, checkasm-only non-JSON
   exercise today). Owner: P2-B.
2. **REVISE-2** — P2-E G1 `p2e:102-124`: add `open:[u8;2], close:[u8;2]` digraph
   parameters to the signature + de-CSS-pin the scalar sketch. Owner: P2-E.
3. **REVISE-3** — P2-C §3 `p2c:280-291`: add one §3 grammar-neutral verdict line each
   for C3, C4, C5, C6 (per §2.1 "each candidate"). Owner: P2-C.

All three are framing/completeness tightenings; none changes a candidate's
grammar-neutral STATUS (every candidate remains grammar-neutral or honestly
orphan-flagged). No candidate is REJECTed on generality grounds. The pool is
admissible to S-P3 on the CH2 axis once the three REVISEs fold into V2.
