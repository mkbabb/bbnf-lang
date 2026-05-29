# SK-V17 S-P2 CHALLENGE — CH2 GENERALITY (V2)

Lens: CH2 GENERALITY. Cycle: V2. Date: 2026-05-29.
Reviewer scope: PASS-2-RESEARCH §3 CH2 + ORCHESTRATOR §3W. Per-candidate Lock-14
grammar-neutral verdict; a JSON/CSS-only primitive with no grammar-neutral
byte-set / classifier / tape expression is REVISE (re-express) or REJECT (drop);
the NEON class leaf must be a SHARED kernel (JSON + CSS witnessed; Sheets/BBNF-self
asserted-by-construction, proof deferred to SK-V18).
Inputs reviewed end-to-end this cycle: `p2a-sota-teardown.md` §2/§3 (NEW this cycle —
not reviewed in V1), `p2b-dav1d-process.md` §2/§3, `p2c-arch-esoterica.md` §2/§3,
`p2d-substrate-tape.md` §2/§3, `p2e-parse-that-gaps.md` §2/§3,
`p2f-grammar-neutral.md` (the load-bearing Lock-14 verdict source) end-to-end.
Source verified at master HEAD `0ae1caa52`; S-P1 profile locked `0ae1caa52`.

## §0 — Verdict summary

CH2 reviews **27 candidate/section rows** across six artefacts (V1 reviewed five;
this cycle adds P2-A's four candidates CP-A1..CP-A4 + its non-candidate row, which V1
omitted from its input set). Every candidate in the pool carries a grammar-neutral
verdict somewhere in the S-P2 set, and P2-F (the designated Lock-14 owner) carries a
verdict for every CF-row. The pool is **grammar-neutral by construction** — there is
NO JSON/CSS-overfit primitive that lacks a byte-set / classifier / tape expression.

The three V1 CH2 REVISEs ALL FOLDED into V2 (§4 verifies each at source line):
- **REVISE-1** (P2-B C-B1 lo6-vs-eq-set conflation) — folded at `p2b:136-153`.
- **REVISE-2** (P2-E G1 digraph parameterisation) — folded at `p2e:104-135`.
- **REVISE-3** (P2-C §3 per-candidate verdict completeness) — folded at `p2c §3`.

Zero orphan REVISE carries forward from V1. This cycle CH2 issues **0 REVISE, 0 REJECT**
on the prior pool; the only new disposition surface is P2-A, which is fully clean.

The shared-NEON-leaf requirement is SATISFIED by `byte_class_from_eq_set_64_neon` — a
real NEON body (four `vld1q_u8` stripes, `vceqq_u8`-fan over the runtime `set: &[u8]`
parameter, `vorrq_u8`-reduce; verified `aarch64/byte_class_from_eq_set_64.rs:33-58`)
reached through the `select_classifier(alphabet)` Lock-14 vehicle (`dispatch.rs:42`).

| Disposition | Count |
|---|---|
| ACCEPT | 27 |
| REVISE | 0 |
| REJECT | 0 |
| **Total candidate/section rows** | **27** |

ACCEPT-rate, all 27 rows: 27/27 = **100%**. On candidate-bearing rows only (excluding
6 negative-space / non-candidate rows recorded for completeness — CF-0, CP-NONE,
CP-BLOCKED×2, D6, G5 — which require no generality work): 21/21 = **100%**.

## §1 — The two CH2 gates, applied

### Gate A — does every candidate carry a P2-F grammar-neutral verdict?

YES. Every CF-row in P2-F §2/§3 (CF-1..CF-4b + CF-0) carries an explicit verdict, and
each candidate-bearing artefact (P2-A/B/C/D/E) ALSO carries its own §3 grammar-neutral
block with a per-candidate line. The cross-references resolve cleanly:

- P2-F CF-1 (tape-append + `ValueRef`) covers C-B2 / D1+D2 / CP-A2+CP-A3.
- P2-F CF-2 (structural-membership classifier) covers C-B1 / C1+C2 / G3 / CP-A1 / D4.
- P2-F CF-3 (commit-by-construction Alt-mode) covers D3's placement note.
- P2-F CF-4a covers C-B3 / C5 / G4 / CP-BLOCKED-digit.
- P2-F CF-4b covers C6.
- P2-E supplies the G1/G2 mask-primitive verdicts itself (§3, both GRAMMAR-NEUTRAL);
  P2-F §1.2 folds them as "the parse-that gap P2-E/P2-C own". This folding was the
  subject of V1's REVISE-2, now discharged (the §2 candidate SHAPE — the load-bearing
  artefact per §2.1 — now matches the §3 neutrality verdict; §4 verifies).
- CP-A4 (tokenize-once) carries its own §3 verdict (P2-A §3 table row 4); it is a
  consumption pattern over the neutral CP-A1 index, not a free-standing kernel.

No candidate is orphaned without a verdict. Gate A passes for all 27 rows.

### Gate B — is the NEON class leaf shared (JSON + CSS + Sheets + BBNF-self)?

The shared leaf is `byte_class_from_eq_set_64` via `select_classifier(alphabet)`. CH2
re-verified at source this cycle:

- `byte_class_from_eq_set_64_neon(src: &[u8;64], set: &[u8]) -> u64` takes the byte set
  as a **runtime parameter** — the alphabet is the ONLY grammar-specific datum
  (`aarch64/byte_class_from_eq_set_64.rs:33`, verified: four `vld1q_u8` stripes loaded
  at `:39-42`, a `for &member in set` `vdupq_n_u8`/`vceqq_u8`/`vorrq_u8` fan at `:50-57`).
  The kernel carries no grammar role. `set.len() <= 8` debug_assert at `:34`.
- `select_classifier(alphabet: &'static [u8; 64])` (`dispatch.rs:42`) is the Lock-14
  vehicle; the alphabet is caller data, matching `LOCKS.md:393-395`.
- JSON's structural set `b"{}[],:\""` (`json/scan.rs:6`) and the CSS delimiter sets
  (`b";{}"` / `b"{};"` / `b":{};"`, ≤8 bytes each) are different `set` arguments to the
  SAME kernel — JSON+CSS witnessed.
- Sheets/BBNF-self: asserted-by-construction (the kernel is grammar-parametric by type),
  proof deferred to SK-V18 per Lock 14 phrase #2 (`LOCKS.md:386-387`). NO candidate in
  ANY of the six artefacts uses fleet-wide / four-grammar wording — the only matches for
  those phrases are explicit prohibitions (P2-F §1.5, §3; P2-A §406; P2-B §240). This is
  correct CH2 hygiene; the shared-leaf requirement is met on the witnessed pair and
  honestly scoped beyond it.

**CH2 re-verified the load-bearing neutrality split (P2-F §1.2) at source:**
- `lo6_table_admissible` (`dispatch.rs:101-113`) computes `let slot = (byte & 0x3f)` at
  `:106` — a low-6-bit MASK, not a modulo — and rejects on `seen[slot]` collision.
- Independently computed: `;`(0x3b) & 0x3f = 59; `{`(0x7b) & 0x3f = 59 — **collision
  confirmed**, so the lo6 backend is inadmissible for every CSS alphabet containing the
  `;{` pair. True modulo `0x7b % 0x3f = 60` would NOT collide — confirming the mechanism
  is the bitmask fold, exactly as every artefact states.
- JSON's live NEON scan rides the lo6/table backend, NOT the eq-set kernel: `json/scan.rs`
  uses `STRUCTURAL_CLASS_TABLE_LO6` (`:10`) → `classify_tbl4::load_lo6_table` (`:214`) →
  `classify_structural_terminator_block_from_table` (`:219`). The eq-set kernel is a
  SEPARATE code path, exercised only by its checkasm corpus-parity test today, NOT live
  in JSON prod. This is the exact distinction V1 REVISE-1 demanded and it is now correctly
  stated in every artefact that names the shared leaf.
- The wired table-64 NEON impl `byte_class_from_table_64_neon` IS a scalar passthrough
  today (`aarch64/byte_class_from_table_64.rs:2` tail-calls `..._scalar`) — verified.

Gate B passes: the shared leaf is real, runtime-parameterised by the alphabet, and the
JSON-lo6 / CSS-eq-set backend split is honestly framed across all six artefacts.

## §2 — Per-candidate dispositions (path:line + verdict)

### P2-A (SOTA teardown) — `p2a-sota-teardown.md` (NEW input this cycle)

**CP-A1 (block-wide byte-class classifier) — ACCEPT.** `p2a:228-263` (§2) + `p2a:401`
(§3). GRAMMAR-NEUTRAL by shared-interface / per-grammar-backend. CP-A1 independently
gets the V1-REVISE-1 framing RIGHT without prompting: it states "CSS = eq-set fan, JSON
= lo6 table" (`p2a:401`), explicitly flags that routing CSS through the lo6/table path
"would claim a SIMD win it silently runs scalar" (`p2a:248-252`), and scopes the eq-set
kernel's non-JSON exercise to "checkasm corpus-parity ONLY (it is not live in JSON prod)"
(`p2a:253-256`). The `;`/`{`→slot-59 collision is cited and CH2 re-derived it. The
alphabet is the only grammar-specific datum; output is a policy-free `Vec<u32>` index.
This is the Gate-B shared leaf, correctly framed.

**CP-A2 (tape-append `push_plain_offset`) — ACCEPT.** `p2a:286-313` + `p2a:402`.
GRAMMAR-NEUTRAL; the `TapeBuilder` is a single non-generic offset sink with no
grammar-specific fields (`assembler.rs:42`, verified `push_plain_offset` at `:71` is a
branchless u32 write). Matches CF-1 / C-B2 / D1. ACCEPT.

**CP-A3 (lazy `ValueRef` rider) — ACCEPT.** `p2a:316-349` + `p2a:403`. GRAMMAR-NEUTRAL
by construction (witnessed JSON+CSS only); the cursor walk is `BackendRule`-shaped, not
hand-coded per-rule. `ValueRef` verified generic over `G: EventGrammar` (`mod.rs:175`).
Sheets/BBNF-self correctly scoped to SK-V18 (`sheets_witness` has no `BackendRule`).
Matches CF-1 / D2. ACCEPT.

**CP-A4 (tokenize-once shared-scan reuse) — ACCEPT.** `p2a:350-376` + `p2a:404`.
GRAMMAR-NEUTRAL: the reuse pattern is generic (a generic primitive consumed by a
per-grammar template, Lock 14 phrase #1); which bytes index is grammar-specific data.
The consumption lives in the per-grammar declaration/selector parse, not in a generic
crate. On the CH2 axis this is clean. (The REDRESS-53 bound CP-A4 carries is a CH3
concern, not CH2; the generality verdict is sound.)

**P2-A non-candidate row (CP-NONE / CP-BLOCKED×2) — ACCEPT.** `p2a:378-395`. FNV/hex,
udot digit kernel, and asmjson FSM correctly recorded as dropped (not re-framed), with
no grammar-neutral verdict required. The orphan/host-block dispositions are CH1/CH3/CH4.
ACCEPT on the CH2 axis.

### P2-B (dav1d process) — `p2b-dav1d-process.md`

**C-B1 (`byte_class_from_eq_set_64`) — ACCEPT (V1 REVISE-1 FOLDED).** `p2b:115-153` + §3
`p2b:225-232`. The V1 conflation ("wired for JSON") is RESOLVED: `p2b:136-153` now splits
the claim into three explicit layers — the shared interface `select_classifier` (the
Lock-14 vehicle), the JSON-admissible lo6/table BACKEND (`classify_tbl4`, the live
`json/scan.rs:207` path, `:136-139`), and the CSS-bound eq-set BACKEND (`byte_class_from_eq_set_64`,
a SEPARATE kernel whose "only non-test exerciser is the corpus-parity smoke + checkasm",
`:140-146`). The lo6 admissibility guard and the `;`/`{`→slot-59 collision are cited
(`:147-153`). This is precisely the REVISE-1 fix; the false "already JSON-wired" implication
for the eq-set kernel is removed. GRAMMAR-NEUTRAL verdict (`p2b:225`) is correct: a byte-set
membership classifier over `set: &[u8]`, JSON+CSS+Sheets+BBNF-self all different `set`
arguments. ACCEPT.

**C-B2 (`push_plain_offset`) — ACCEPT.** `p2b:166-186` + §3 `p2b:233`. GRAMMAR-NEUTRAL; a
u32 append into the shared offset tape, JSON-ridden. Matches CF-1. ACCEPT.

**C-B3 (orphan udot) — ACCEPT (generality).** `p2b:188-202`. Process-rejected; the
generality dimension (a 4-digit decode is neutral) is sound, correctly framed as
"orphan-blocked, not JSON-overfit … it is unreached" (`p2b:240`). Orphan disposition is
CH1/CH4. ACCEPT on the CH2 axis.

**C-B0 (admission process / gate table) — ACCEPT, commended.** `p2b:204-219`. The gate
row binding the grammar-neutral verdict to the P2-F output institutionalises the CH2
requirement as a per-candidate S-P3 admission gate. ACCEPT.

### P2-C (arch esoterica) — `p2c-arch-esoterica.md`

**C1 (lo6 TBL) — ACCEPT.** `p2c:130-159` + §3 `p2c §3 C1`. "GRAMMAR-NEUTRAL but
ALPHABET-INADMISSIBLE for CSS" — the correct framing; the `lo6_table_admissible` guard
is the neutral gate, the CSS answer is C2. CH2 re-verified the slot-59 collision. ACCEPT.

**C2 (eq-set fan) — ACCEPT.** `p2c:167-199` + §3. The primary CSS shared-NEON-leaf route;
"GRAMMAR-NEUTRAL, JSON+CSS witnessed" with the alphabet as the only grammar-specific
datum. This is the Gate-B shared leaf; CH2 verified the NEON body is real, not a
passthrough. ACCEPT.

**C3 (shrn movemask) — ACCEPT (V1 REVISE-3 FOLDED).** `p2c:201-218` + §3 verdict line.
The §3 now carries an explicit verdict ("VERDICT: PASS (grammar-neutral)" — movemask
bit-packing is grammar-free, folds under C1/C2), satisfying the §2.1 per-candidate
requirement. ACCEPT.

**C4 (host CTZ extract) — ACCEPT (V1 REVISE-3 FOLDED).** `p2c:220-235` + §3 verdict line.
Explicit §3 verdict present ("VERDICT: PASS (grammar-neutral)" — mask→first-set-index,
grammar-free, REDRESS-89 bulk-form flagged for CH3). ACCEPT.

**C5 (UDOT 4-digit) — ACCEPT (generality; V1 REVISE-3 FOLDED).** `p2c:236-256` + §3 verdict
line. Explicit §3 verdict ("grammar-neutral IN SHAPE, but CSS-ORPHAN, deferred to CF-4a").
Generality sound; orphan-block is CH1/CH4. ACCEPT.

**C6 (i8mm) — ACCEPT (generality; V1 REVISE-3 FOLDED).** `p2c:257-278` + §3 verdict line.
Explicit §3 verdict ("grammar-neutral IN SHAPE, CSS-ORPHAN + kernel-absent, deferred to
CF-4b"). Generality sound; no-antecedent REJECT is CH1's. ACCEPT.

P2-C §3 now carries one verdict line per candidate (C1, C2, C3, C4, C5, C6) — V1
REVISE-3 fully discharged; the §2.1 per-candidate schema is satisfied.

### P2-D (substrate + tape) — `p2d-substrate-tape.md`

**D1 (`push_plain_offset` emit op) — ACCEPT.** `p2d:206-239` + §3. GENERALISABLE,
Lock-14 clean; per-grammar datum is which positions push (from `BackendRule`). ACCEPT.

**D2 (lazy `ValueRef` projection) — ACCEPT.** `p2d:241-278` + §3. GENERALISABLE; byte→kind
decode is the grammar-neutral mechanism; Sheets/BBNF-self correctly scoped to SK-V18.
ACCEPT.

**D3 (O(1) checkpoint/truncate) — ACCEPT.** `p2d:281-323` + §3. GENERALISABLE; a generic
`Vec<u32>` `len`/`truncate`. Correctly separates the substrate mechanism from the
commit-placement codegen decision (CF-3). The CONDITIONAL lever-status (post-CF-1
typed-tape re-profile) is a CH1 concern; the generality verdict is clean. ACCEPT.

**D4 (one-shot SIMD reservation) — ACCEPT.** `p2d:331-367` + §3. GENERALISABLE;
`CapacityPlan` is grammar-free, the SIMD count reuses the shared `select_classifier`
kernel with the CSS alphabet as the only per-grammar datum; fallback-on-collision note
consistent with the verified lo6 finding. ACCEPT.

**D5 (sparse-flag side-table) — ACCEPT.** `p2d:375-404` + §3. GENERALISABLE-WITH-GUARD —
the sharpest CH2-aware row: it embeds the CH2 re-express requirement as an admission guard
(each flag bit MUST be a `BackendRule` branch-tag, not a per-rule constant; else the
semantics become a relocated `W5C_REQUEST_FACT_PROFILES` per-rule catalogue, the Lock-14
trap). The guard IS CH2's re-express requirement, stated by the author. The §3 row carries
"CH2 REVISE if semantics become a per-rule catalogue" — the self-conditioning is correct.
No CH2 action needed. ACCEPT.

**D6 (no second substrate — REJECT-on-sight) — ACCEPT.** `p2d:414-421` + §3. Negative-space
row; "Grammar-neutral verdict: N/A — REJECTED by Lock 1" is correct. The Lock-1 assertions
are CH5's domain. ACCEPT on the CH2 axis.

### P2-E (parse-that gaps) — `p2e-parse-that-gaps.md`

**G1 (`comment_body_mask_64`) — ACCEPT (V1 REVISE-2 FOLDED).** `p2e:102-135` (§2) +
`p2e:243-250` (§3). The V1 CSS-pinning is RESOLVED: the §2 signature now reads
`fn comment_body_mask_64(src: &[u8;64], open: [u8;2], close: [u8;2], in_comment_carry: bool)`
(`p2e:104`), the scalar sketch tests `src[i]==open[0] && src[i+1]==open[1]` /
`src[i]==close[0] && src[i+1]==close[1]` (`p2e:124-125`) and "never a literal `/` or `*`"
(`p2e:118`), and the NEON body's "compare operands are the digraph parameters" (`p2e:131-134`).
The §2 candidate SHAPE — the load-bearing artefact per §2.1 — now MATCHES the §3 neutrality
verdict (GRAMMAR-NEUTRAL by digraph parameterisation, `p2e:243-250`). The per-grammar
parameterisation P2-E asked P2-F to verify is now discharged at the signature level. ACCEPT.

**G2 (`bracket_depth_mask_64`) — ACCEPT.** `p2e:130-164` (§2) + `p2e:251-258` (§3). The
signature takes open/close MASKS (already abstracted from the bracket bytes, produced
upstream by alphabet-driven `byte_class_from_eq_set_64`), so the primitive sees only masks,
never literal CSS bracket bytes. §3 correctly calls it "the canonical Lock-14 primitive"
(nested-bracket balance generalises to JSON arrays/objects, BBNF `()`/`[]`, Sheets parens).
Exemplary shared-leaf hygiene. ACCEPT.

**G3 (`scan_components_to_index`) — ACCEPT.** `p2e:166-184` (§2) + `p2e:259-266` (§3). The
explicit Lock-14 neutrality vehicle via `select_classifier(alphabet)`; the delimiter
alphabet is the only per-grammar datum; lives in the per-grammar consumer (`runtime/.../scan.rs`),
NOT bbnf-simd, so JSON policy never enters the generic crate (PASS-2 §8.5). Isomorphic to
`json/scan.rs:22`. The shared assembler consuming G1/G2 into the Lock-1 `Vec<u32>` index.
ACCEPT.

**G4 (`parse_4_digits` checkasm gate) — ACCEPT (generality).** `p2e:186-200` + `p2e:267-270`.
GRAMMAR-NEUTRAL but ORPHAN — the 4-digit decode is neutral, but it has no benched CSS
antecedent and is gated behind a typed-path re-profile. "Its neutrality is moot until an
antecedent exists" is the honest framing. Orphan-block is CH1/CH4. ACCEPT on the CH2 axis.

**G5 (FNV/hex non-candidate) — ACCEPT.** `p2e:201-211` + §3 N/A. Correctly a non-candidate
that retires with the fact-stream; no NEON hex/FNV primitive proposed. ACCEPT.

P2-E §3 closes (`p2e:271-273`) "No candidate is JSON-overfit; none is CSS-overfit (G1/G2
are alphabet/digraph-parameterised, G3 is the neutrality vehicle itself)" — accurate after
the REVISE-2 fold.

### P2-F (the Lock-14 owner) — `p2f-grammar-neutral.md`

**CF-1 (tape-append + `ValueRef`) — ACCEPT.** `p2f:138-164`. GRAMMAR-NEUTRAL
(JSON+CSS-witnessed), conditional on §1.4 routing-derived-from-grammar (retire
`W5C_REQUEST_FACT_PROFILES`, no relocated per-rule branching into projection DATA). The
conditional-REVISE escape hatch is the correct CH2-compliant framing. `ValueRef<G>` verified
generic (`mod.rs:175`). ACCEPT.

**CF-2 (structural-membership classifier) — ACCEPT.** `p2f:166-201`. GRAMMAR-NEUTRAL at
the interface; the BACKEND-choice neutrality crux (§1.2) is correctly identified and the
candidate held to the admissible eq-set/256-table primitive, NOT lo6-reuse. The strongest
CH2 row: it pre-empts the exact JSON-overfit trap (silently scalar-falling-back lo6 on CSS).
Gate B met here. ACCEPT.

**CF-3 (commit-by-construction Alt-mode) — ACCEPT (generality).** `p2f:203-235`.
GRAMMAR-NEUTRAL codegen property derived from `BackendRule` Alt shape, JSON-witnessed. The
no-measured-speculative-rollback antecedent (the hard S-P1-re-confirm obligation) is a
CH1 concern; the §2 verdict is scrupulous in NOT claiming the LOCKED 28.87%+2.45%
recognition-control figures as a measured rollback antecedent. Generality sound. ACCEPT
on the CH2 axis.

**CF-4a (udot 4-digit decode) — ACCEPT (generality).** `p2f:238-265`. GRAMMAR-NEUTRAL
shape (`byte_class_from_range_64` digit-run family, `LOCKS.md:426-431`) — NOT CSS-overfit.
"GRAMMAR-NEUTRAL … BUT CURRENTLY ORPHAN" is the honest disposition. Orphan to CH1/CH4.
ACCEPT on the CH2 axis.

**CF-4b (i8mm dimension decode) — ACCEPT (generality).** `p2f:267-287`. GRAMMAR-NEUTRAL in
shape (digit-run family); the no-P1-antecedent REJECT is CH1's. Generality verdict honest.
ACCEPT on the CH2 axis.

**CF-0 (negative space) — ACCEPT.** `p2f:289-301`. The no-unicode / no-dispatch-self /
no-FNV findings are correctly framed; the UTF-8-continuation classifier is "grammar-neutral
in the abstract, but JSON/other-grammar-only here" — exactly the CH2 distinction between
abstract neutrality and witnessed antecedent. ACCEPT.

## §3 — Cross-artefact consistency (CH2-relevant)

- **The shared-NEON-leaf identity is now consistent and correct across ALL SIX artefacts.**
  P2-A CP-A1, P2-B C-B1, P2-C C1/C2, P2-D D4, P2-E G3, and P2-F CF-2 / §1.2 all name
  `byte_class_from_eq_set_64` (via `select_classifier(alphabet)`) as the admissible CSS
  scan kernel and lo6/`classify_tbl4` as the JSON-admissible-only backend. The one V1
  inconsistency (C-B1's conflation) is now resolved. CH2 confirms: NO artefact now implies
  the eq-set kernel is live-JSON-wired; all correctly scope its non-JSON exercise to its
  checkasm corpus-parity test.
- **The lo6 collision is the single most-cited neutrality fact and CH2 independently
  re-derived it this cycle:** `;`(0x3b) & 0x3f = 59; `{`(0x7b) & 0x3f = 59 (bitmask collision);
  `0x7b % 0x3f = 60` (no modulo collision). It holds. The guard `(byte & 0x3f)` is verified
  at `dispatch.rs:106`.
- **Sheets/BBNF-self scoping is uniformly honest.** No candidate in any of the six artefacts
  uses fleet-wide / four-grammar wording; all scope to JSON+CSS-witnessed with SK-V18
  deferral. Gate B's "must be shared (JSON+CSS+Sheets+BBNF-self)" is satisfied in the
  asserted-by-construction sense the locks permit (`LOCKS.md:386-387`); CH2 does NOT require
  a live Sheets/BBNF rider in SK-V17 (P2-F §1.5 correctly states `sheets_witness` is a
  24-LOC byte-classification trait with no `.bbnf`/`BackendRule`).
- **The two overfit re-entry seams are both fenced.** CF-1's routing (the
  `W5C_REQUEST_FACT_PROFILES` const must retire, branching must derive from grammar shape,
  not relocate into projection DATA — P2-F §1.4) and D5's flag semantics (each bit a
  `BackendRule` branch-tag — P2-D §2 D5) are the only places a CSS-overfit could re-enter,
  and both are named with the precise Lock-14 trap and a conditional-REVISE guard.

## §4 — V1-REVISE fold verification (for §3Z convergence)

All three V1 CH2 REVISEs verified FOLDED at source. None carries forward as an orphan.

1. **REVISE-1 (P2-B C-B1)** — DISCHARGED at `p2b:136-153`. The "wired for JSON" claim is
   split into shared-interface (JSON-wired) / lo6 backend (JSON-only, the live
   `json/scan.rs:207` path) / eq-set backend (CSS-bound, non-test exerciser = corpus-parity
   smoke + checkasm only). The slot-59 collision is cited (`p2b:147-153`). The false
   "already JSON-wired" implication for the eq-set kernel is removed.
2. **REVISE-2 (P2-E G1)** — DISCHARGED at `p2e:104-135`. The signature now carries
   `open:[u8;2], close:[u8;2]` parameters; the scalar sketch tests `open[0]/open[1]/close[0]/close[1]`
   and "never a literal `/` or `*`". The §2 candidate shape now matches the §3 neutrality
   verdict.
3. **REVISE-3 (P2-C §3)** — DISCHARGED in P2-C §3. C3, C4, C5, C6 each carry an explicit
   one-line grammar-neutral verdict, satisfying the §2.1 per-candidate schema.

This cycle introduces ZERO new REVISE and ZERO REJECT. The pool is grammar-neutral by
construction; every candidate carries a verdict; the shared-NEON-leaf is shared and
honestly scoped. The CH2 axis is converged: 27/27 ACCEPT, no orphan REVISE, V ≤ 5
satisfied (this is V2).

## §5 — Sources (verified this cycle)

- **bbnf source (master HEAD `0ae1caa52`):** `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:33-58`
  (real NEON eq-set body, `set: &[u8]` runtime param); `aarch64/byte_class_from_table_64.rs:2`
  (scalar passthrough — verified tail-calls `..._scalar`); `dispatch.rs:42` (select_classifier),
  `:101-113` (lo6_table_admissible, `(byte & 0x3f)` MASK at `:106`); `skinny/crates/runtime/src/tape/mod.rs:175`
  (`ValueRef<'doc,'input,K,G: EventGrammar>` generic); `tape/assembler.rs:71` (push_plain_offset
  branchless u32 write); `skinny/crates/runtime/src/grammars/json/scan.rs:6` (STRUCTURAL_BYTES
  `{}[],:"`), `:10` (STRUCTURAL_CLASS_TABLE_LO6), `:214-228` (live classify_tbl4 lo6 path).
- **Computed (lo6 admissibility):** `;`(0x3b)&0x3f=59, `{`(0x7b)&0x3f=59 (collision under
  bitmask); `0x7b % 0x3f = 60` (no collision under modulo). JSON alphabet `{}[],:"` admissible;
  every CSS alphabet with the `;{` pair inadmissible.
- **S-P2 artefacts (V2 cycle):** `p2a-sota-teardown.md` §2 (CP-A1..A4) / §3; `p2b-dav1d-process.md`
  §2 (C-B0..B3) / §3 / §4; `p2c-arch-esoterica.md` §2 (C1..C6) / §3; `p2d-substrate-tape.md`
  §2 (D1..D6) / §3; `p2e-parse-that-gaps.md` §2 (G1..G5) / §3; `p2f-grammar-neutral.md` §1-§5.
- **V1 disposition source:** `p2/hardening/V1/CH2.md` §2 (REVISE-1/2/3) + §4 orphan-REVISE
  tracking.
- **Locks:** `LOCKS.md:75` (Lock 1 substrate-union / transient-producer), `:386-387` (Lock 14
  phrase #2 witnessed-grammar scoping), `:393-397` (Lock 14 grammar-neutral primitive: delimiter
  policy = caller data), `:426-431` (byte_class_from_eq_set_64 / byte_class_from_range_64 abstract
  split, digit-run family), `:603` (Lock 14 phrase #1: no hand-coded profile arrays).
- **Pass contract:** `restart/prompts/skinny/PASS-2-RESEARCH.md` §2.1 (per-candidate §3 schema),
  §3 CH2 (the two gates), §8 axes.
- **Host:** Apple M5 Max, aarch64-apple-darwin. S-P1 commit `0ae1caa52`; master HEAD `0ae1caa52`.
