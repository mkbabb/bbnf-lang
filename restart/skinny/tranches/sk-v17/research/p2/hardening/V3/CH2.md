# SK-V17 S-P2 CHALLENGE — CH2 GENERALITY (V3)

Lens: CH2 GENERALITY. Cycle: V3. Date: 2026-05-29.
Reviewer scope: PASS-2-RESEARCH §3 CH2 + ORCHESTRATOR §3W. Per-candidate Lock-14
grammar-neutral verdict; a JSON/CSS-only primitive with no grammar-neutral
byte-set / classifier / tape expression is REVISE (re-express) or REJECT (drop);
the NEON class leaf must be a SHARED kernel (JSON + CSS witnessed; Sheets/BBNF-self
asserted-by-construction, proof deferred to SK-V18).
Inputs reviewed end-to-end this cycle: `p2a-sota-teardown.md` §2/§3/§4,
`p2b-dav1d-process.md` §2/§3, `p2c-arch-esoterica.md` §2/§3, `p2d-substrate-tape.md`
§2/§3, `p2e-parse-that-gaps.md` §2/§3, `p2f-grammar-neutral.md` (the load-bearing
Lock-14 verdict source) end-to-end.
Source re-verified at master HEAD `0ae1caa52` (confirmed `git rev-parse HEAD` =
`0ae1caa5235ee867c5c081f186b6452c51e44a14`); S-P1 profile locked `0ae1caa52`.

## §0 — Verdict summary

CH2 reviews **27 candidate/section rows** across six artefacts (the same pool the V2
review carried forward: P2-A CP-A1..A4 + non-candidate row; P2-B C-B0..B3; P2-C C1..C6;
P2-D D1..D6; P2-E G1..G5; P2-F CF-0..CF-4b). Every candidate carries a grammar-neutral
verdict somewhere in the S-P2 set, and P2-F (the designated Lock-14 owner) carries a
verdict for every CF-row. The pool is **grammar-neutral by construction** — there is
NO JSON/CSS-overfit primitive that lacks a byte-set / classifier / tape expression.

The three V1 CH2 REVISEs (folded in V2) REMAIN folded at V3 source (§4 re-verifies each
at the live line). The V2 cycle issued 0 REVISE / 0 REJECT; this V3 cycle confirms that
disposition holds against the now-current V3 artefacts (P2-B/C/D/E/F all advanced to
"Cycle: V3"; P2-A still carries "Cycle: V2" in frontmatter — a CH6/CH1 hygiene note, NOT
a CH2 generality defect; its §2/§3/§4 content is unchanged and grammar-neutral-clean).

The shared-NEON-leaf requirement is SATISFIED by `byte_class_from_eq_set_64_neon` — a
real NEON body (four `vld1q_u8` stripes at `aarch64/byte_class_from_eq_set_64.rs:40-43`,
a `for &member in set` `vdupq_n_u8`/`vceqq_u8`/`vorrq_u8` fan at `:55-60`, `set.len()<=8`
debug_assert at `:34`) reached through the `select_classifier(alphabet)` Lock-14 vehicle
(`dispatch.rs:42`). CH2 re-verified the body, the passthrough gap, and the lo6 collision
at source this cycle (§1 Gate B).

| Disposition | Count |
|---|---|
| ACCEPT | 27 |
| REVISE | 0 |
| REJECT | 0 |
| **Total candidate/section rows** | **27** |

ACCEPT-rate, all 27 rows: 27/27 = **100%**. On candidate-bearing rows only (excluding
6 negative-space / non-candidate rows recorded for completeness — CF-0, CP-NONE,
CP-BLOCKED×2, D6, G5): 21/21 = **100%**.

## §1 — The two CH2 gates, applied

### Gate A — does every candidate carry a P2-F grammar-neutral verdict?

YES. Every CF-row in P2-F §2/§3 (CF-1..CF-4b + CF-0) carries an explicit verdict, and
each candidate-bearing artefact (P2-A/B/C/D/E) ALSO carries its own §3 grammar-neutral
block with a per-candidate line. The cross-references resolve cleanly:

- P2-F CF-1 (tape-append + `ValueRef`, `p2f:138-174`) covers C-B2 / D1+D2 / CP-A2+CP-A3.
- P2-F CF-2 (structural-membership classifier, `p2f:176-218`) covers C-B1 / C1+C2 / G3 /
  CP-A1 / D4.
- P2-F CF-3 (commit-by-construction Alt-mode, `p2f:220-261`) covers D3's placement note.
- P2-F CF-4a (`p2f:263-297`) covers C-B3 / C5 / G4 / CP-BLOCKED-digit.
- P2-F CF-4b (`p2f:299-326`) covers C6.
- P2-E supplies the G1/G2 mask-primitive verdicts itself (`p2e:243-257`, both
  GRAMMAR-NEUTRAL); P2-F §1.2 folds them as "the parse-that gap P2-E/P2-C own". This
  folding was V1's REVISE-2, discharged in V2 and re-verified intact at V3 (§4).
- CP-A4 (tokenize-once) carries its own §3 verdict (`p2a:404`); it is a consumption
  pattern over the neutral CP-A1 index, not a free-standing kernel.

No candidate is orphaned without a verdict. Gate A passes for all 27 rows.

### Gate B — is the NEON class leaf shared (JSON + CSS + Sheets + BBNF-self)?

The shared leaf is `byte_class_from_eq_set_64` via `select_classifier(alphabet)`. CH2
re-verified the whole chain at source this cycle (HEAD `0ae1caa52`):

- **The eq-set NEON body is real and runtime-parameterised.**
  `byte_class_from_eq_set_64_neon(src: &[u8;64], set: &[u8]) -> u64`
  (`aarch64/byte_class_from_eq_set_64.rs:33`): four `vld1q_u8` source stripes
  (`:40-43`), a `for &member in set { ... vceqq_u8 ... vorrq_u8 ... }` fan over the
  runtime `set` parameter (`:55-60`), `set.len() <= 8` debug_assert at `:34`. The byte
  set is the ONLY grammar-specific datum; the kernel carries no grammar role. This is the
  body P2-A/C/F all name as the shared CSS scan leaf.
- **The interface is the Lock-14 vehicle.** `select_classifier(alphabet: &'static [u8;
  64])` (`dispatch.rs:42`) takes the alphabet as caller data, matching `LOCKS.md:393-395`
  (delimiter policy = caller data, not hardcoded JSON/CSS constants).
- **JSON+CSS witnessed.** JSON's structural set `b"{}[],:\""` (`json/scan.rs:6`,
  re-verified) and the CSS delimiter sets (`b";{}"` / `b":{};"`, ≤8 bytes each) are
  different `set` arguments to the SAME kernel.
- **Sheets/BBNF-self:** asserted-by-construction (the kernel is grammar-parametric by
  type), proof deferred to SK-V18 per Lock 14 phrase #2 (`LOCKS.md:386-387`). NO candidate
  in ANY of the six artefacts uses fleet-wide / four-grammar wording; this is correct CH2
  hygiene, the shared-leaf requirement met on the witnessed pair and honestly scoped.

**CH2 re-derived the load-bearing neutrality split (P2-F §1.2) at source this cycle:**
- `lo6_table_admissible` (`dispatch.rs:101-113`) computes `let slot = (byte & 0x3f)` at
  `:106` — a low-6-bit MASK, not a modulo — and rejects on `seen[slot]` collision.
- Independently computed this cycle: `;`(0x3b) & 0x3f = **59**; `{`(0x7b) & 0x3f = **59**
  — collision CONFIRMED, so the lo6 backend is inadmissible for every CSS alphabet
  containing the `;{` pair. True modulo `0x7b % 0x3f = 60` would NOT collide — confirming
  the mechanism is the bitmask fold, exactly as every artefact states. JSON's `{}[],:"`
  alphabet is distinct under `& 0x3f` (verified: 7 distinct slots) — admissible.
- JSON's live NEON scan rides the lo6/table backend, NOT the eq-set kernel: `json/scan.rs`
  uses `STRUCTURAL_CLASS_TABLE_LO6` (`:10`) → `classify_tbl4::load_lo6_table` →
  `classify_structural_terminator_block_from_table` (the `:210-219` region). The eq-set
  kernel is a SEPARATE code path, exercised only by its `checkasm_byte_class_from_eq_set_64`
  corpus-parity test today, NOT live in JSON prod. (V1 REVISE-1's exact distinction; held.)
- The wired table-64 NEON impl `byte_class_from_table_64_neon` IS a scalar passthrough
  today (`aarch64/byte_class_from_table_64.rs:2-3` tail-calls `..._scalar`) — re-verified.

Gate B passes: the shared leaf is real, runtime-parameterised by the alphabet, and the
JSON-lo6 / CSS-eq-set backend split is honestly framed across all six artefacts.

## §2 — Per-candidate dispositions (path:line + verdict)

### P2-A (SOTA teardown) — `p2a-sota-teardown.md`

**CP-A1 (block-wide byte-class classifier) — ACCEPT.** `p2a:401` (§3) + `p2a:429-434`
(§4 demotion). GRAMMAR-NEUTRAL by shared-interface / per-grammar-backend: "CSS = eq-set
fan, JSON = lo6 table". The §4 "unearned-SIMD scan on the lo6/table route" demotion
explicitly bars routing CSS through `byte_class_from_table_64_neon` because that route is
(a) lo6-INADMISSIBLE for the CSS alphabet (`;`/`{`→slot-59, `dispatch.rs:101`, CH2
re-derived) and (b) a scalar passthrough today — preventing the silent-scalar-SIMD-win
trap. Alphabet is the only grammar datum; output is a policy-free `Vec<u32>`. This is the
Gate-B shared leaf, correctly framed. ACCEPT.

**CP-A2 (tape-append `push_plain_offset`) — ACCEPT.** `p2a:402`. GRAMMAR-NEUTRAL; the
`TapeBuilder` is a single offset/payload sink with no grammar-keyed fields (re-verified
`assembler.rs:42-48` — `source/offsets/flag_cursors/flag_values/payloads`, no per-grammar
field), `push_plain_offset` a branchless u32 append (`:71`). Matches CF-1 / C-B2 / D1.
ACCEPT.

**CP-A3 (lazy `ValueRef` rider) — ACCEPT.** `p2a:403`. GRAMMAR-NEUTRAL by construction
(witnessed JSON+CSS only); the cursor walk is `BackendRule`-shaped, not hand-coded
per-rule. `ValueRef` verified generic over `G: EventGrammar` (`mod.rs:175`,
`G: EventGrammar = AnyGrammar`). Sheets/BBNF-self correctly scoped to SK-V18. Matches
CF-1 / D2. ACCEPT.

**CP-A4 (tokenize-once shared-scan reuse) — ACCEPT.** `p2a:404`. GRAMMAR-NEUTRAL: the
reuse pattern is generic (a generic primitive consumed by a per-grammar template, Lock 14
phrase #1); which bytes index is grammar-specific data. The REDRESS-53 bound is a CH3
concern; the generality verdict is sound. ACCEPT.

**P2-A non-candidate row (CP-NONE / CP-BLOCKED×2) — ACCEPT.** `p2a:408-409`. FNV/hex,
udot digit kernel, asmjson FSM correctly recorded as dropped (not re-framed); no
grammar-neutral verdict required. ACCEPT on the CH2 axis.

### P2-B (dav1d process) — `p2b-dav1d-process.md`

**C-B1 (`byte_class_from_eq_set_64`) — ACCEPT (V1 REVISE-1 fold HELD).** `p2b:225-232`.
GRAMMAR-NEUTRAL: a byte-set membership classifier over `set: &[u8]`; JSON `b"{}[],:"`,
CSS delimiter sets, Sheets cell-delimiters, BBNF metacharacters all different `set`
arguments. The §3 explicitly defers the full cross-grammar set-mapping to P2-F. The V1
"already JSON-wired" conflation remains resolved (the eq-set kernel's only exerciser is
its checkasm/corpus-parity test, not live JSON). ACCEPT.

**C-B2 (`push_plain_offset`) — ACCEPT.** `p2b:233-237`. GRAMMAR-NEUTRAL; a u32 append into
the shared offset tape; the CSS-specific part (which offsets, what they mean) lives in the
per-grammar projection, not the tape op. Matches CF-1. ACCEPT.

**C-B3 (orphan udot) — ACCEPT (generality).** `p2b:238-240`. "Would be grammar-neutral IF
admitted (4-digit→u32 is generic) … process-rejected … orphan-blocked, not JSON-overfit".
Correct generality framing; orphan disposition is CH1/CH4. ACCEPT on the CH2 axis.

**C-B0 (admission process / gate table) — ACCEPT, commended.** The gate row binding the
grammar-neutral verdict to the P2-F output institutionalises the CH2 requirement as a
per-candidate S-P3 admission gate. ACCEPT.

### P2-C (arch esoterica) — `p2c-arch-esoterica.md`

**C1 (lo6 TBL) — ACCEPT.** `p2c:303-308`. "GRAMMAR-NEUTRAL but ALPHABET-INADMISSIBLE for
CSS" — correct framing; `lo6_table_admissible` is the neutral gate, CSS answer is C2. CH2
re-verified the slot-59 collision. ACCEPT.

**C2 (eq-set fan) — ACCEPT.** `p2c:295-302`. The primary CSS shared-NEON-leaf route;
"GRAMMAR-NEUTRAL, JSON+CSS witnessed", `set: &[u8]` runtime param, alphabet the only
grammar datum. The Gate-B shared leaf; CH2 verified the NEON body is real, not a
passthrough. ACCEPT.

**C3 (shrn movemask) — ACCEPT (V1 REVISE-3 fold HELD).** `p2c:309-313`. Explicit "VERDICT:
PASS (grammar-neutral)" — bit-packing carries no grammar datum, folds under C1/C2. ACCEPT.

**C4 (host CTZ extract) — ACCEPT (V1 REVISE-3 fold HELD).** `p2c:314-317`. Explicit
"VERDICT: PASS (grammar-neutral)" — mask→first-set-index, grammar-free; REDRESS-89
bulk-form flagged for CH3. ACCEPT.

**C5 (UDOT 4-digit) — ACCEPT (generality; V1 REVISE-3 fold HELD).** `p2c:318-323`. Explicit
verdict "grammar-neutral IN SHAPE, but CSS-ORPHAN (deferred to P2-F CF-4a)" — the precise
missing-antecedent-vs-overfit distinction CH2 wants. Orphan-block is CH1/CH4. ACCEPT.

**C6 (i8mm) — ACCEPT (generality; V1 REVISE-3 fold HELD).** `p2c:324-328`. Explicit verdict
"grammar-neutral IN SHAPE, CSS-ORPHAN + kernel-absent (deferred to CF-4b)". No-antecedent
REJECT is CH1's. ACCEPT.

P2-C §3 carries one verdict line per candidate (C1..C6) and names all 7 REDRESS-blocked
instruction routes (PMULL-88, CSSC-CTZ-89, tiny-string-28/33, unicode-82/object-pair-84,
x86/AVX/GFNI/VBMI2/VPCLMUL, SVE/SME, runtime-feature-detect-in-hot-loop). V1 REVISE-3
fully held; §2.1 per-candidate schema satisfied.

### P2-D (substrate + tape) — `p2d-substrate-tape.md`

**D1 (`push_plain_offset` emit op) — ACCEPT.** `p2d:429`. GENERALISABLE, Lock-14 clean;
per-grammar datum is which positions push (from `BackendRule`). ACCEPT.

**D2 (lazy `ValueRef` projection) — ACCEPT.** `p2d:430`. GENERALISABLE; byte→kind decode is
the grammar-neutral mechanism; Sheets/BBNF-self correctly scoped to SK-V18 (`sheets_witness`
has no `BackendRule`, `SYNTHESIS.md:249-255`). ACCEPT.

**D3 (O(1) checkpoint/truncate) — ACCEPT.** `p2d:431`. GENERALISABLE; a generic `Vec::len`/
`truncate`. Separates the substrate mechanism from the commit-placement codegen decision
(CF-3). The CONDITIONAL post-CF-1-re-profile lever-status is a CH1 concern; generality
clean. ACCEPT.

**D4 (one-shot SIMD reservation) — ACCEPT.** `p2d:432`. GENERALISABLE; `CapacityPlan` is
grammar-free, the SIMD count reuses the shared `select_classifier` kernel with the CSS
alphabet as the only per-grammar datum. ACCEPT.

**D5 (sparse-flag side-table) — ACCEPT.** `p2d:433`. GENERALISABLE-WITH-GUARD — the
sharpest CH2-aware row: it embeds the re-express requirement as an admission guard ("flag
bit MUST be a `BackendRule` branch-tag, not a per-rule constant; CH2 REVISE if semantics
become a per-rule catalogue"). The guard IS CH2's re-express requirement, stated by the
author. No CH2 action needed. ACCEPT.

**D6 (no second substrate — REJECT-on-sight) — ACCEPT.** `p2d:434`. Negative-space row;
"REJECT (Lock 1)" is correct. The Lock-1 assertions are CH5's domain. ACCEPT on the CH2
axis.

### P2-E (parse-that gaps) — `p2e-parse-that-gaps.md`

**G1 (`comment_body_mask_64`) — ACCEPT (V1 REVISE-2 fold HELD).** `p2e:243-250`. The §2
signature carries `(open:[u8;2], close:[u8;2])`, the scalar sketch tests
`open[0]/open[1]/close[0]/close[1]` and "never a literal `/`/`*`". GRAMMAR-NEUTRAL by
digraph parameterisation; the §2 candidate SHAPE matches the §3 neutrality verdict. The
REVISE-2 fold (CSS-pinning removed) is intact at V3. ACCEPT.

**G2 (`bracket_depth_mask_64`) — ACCEPT.** `p2e:251-257`. Takes open/close MASKS (already
abstracted from bracket bytes by upstream `byte_class_from_eq_set_64`), so the primitive
sees only masks, never literal CSS bracket bytes. "The canonical Lock-14 primitive"
(nested-bracket balance generalises to JSON arrays/objects, BBNF `()`/`[]`, Sheets parens).
Exemplary shared-leaf hygiene. ACCEPT.

**G3 (`scan_components_to_index`) — ACCEPT.** `p2e:258-264`. The explicit Lock-14 neutrality
vehicle via `select_classifier(alphabet)`; the delimiter alphabet the only per-grammar
datum; lives in the per-grammar consumer, not bbnf-simd (PASS-2 §8.5). Isomorphic to
`json/scan.rs:22`. ACCEPT.

**G4 (`parse_4_digits` checkasm gate) — ACCEPT (generality).** `p2e:265-268`. GRAMMAR-NEUTRAL
but ORPHAN — "its neutrality is moot until an antecedent exists" — the honest framing.
Orphan-block is CH1/CH4. ACCEPT on the CH2 axis.

**G5 (FNV/hex non-candidate) — ACCEPT.** `p2e:269`. Correctly a non-candidate; no NEON
hex/FNV primitive proposed. ACCEPT.

P2-E §3 closes (`p2e:271-273`) "No candidate is JSON-overfit; none is CSS-overfit
(G1/G2 are alphabet/digraph-parameterised, G3 is the neutrality vehicle itself)" — accurate.

### P2-F (the Lock-14 owner) — `p2f-grammar-neutral.md`

**CF-1 (tape-append + `ValueRef`) — ACCEPT.** `p2f:138-174`. GRAMMAR-NEUTRAL
(JSON+CSS-witnessed), conditional on §1.4 routing-derived-from-grammar (retire
`W5C_REQUEST_FACT_PROFILES` — re-verified present at `codegen/src/lib.rs:336`, consumed at
`:567` — no relocated per-rule branching into projection DATA). The conditional-REVISE
escape hatch is the correct CH2-compliant framing. `ValueRef<G>` verified generic
(`mod.rs:175`). ACCEPT.

**CF-2 (structural-membership classifier) — ACCEPT.** `p2f:176-218`. GRAMMAR-NEUTRAL at the
interface; the BACKEND-choice neutrality crux (§1.2) is correctly identified and the
candidate held to the admissible eq-set/256-table primitive, NOT lo6-reuse. The strongest
CH2 row: it pre-empts the silently-scalar-falling-back lo6-on-CSS trap. Gate B met here.
ACCEPT.

**CF-3 (commit-by-construction Alt-mode) — ACCEPT (generality).** `p2f:220-261`.
GRAMMAR-NEUTRAL codegen property derived from `BackendRule` Alt shape, JSON-witnessed. The
no-measured-speculative-rollback antecedent (the hard S-P1-re-confirm obligation) is a CH1
concern; the §2 verdict is scrupulous in NOT claiming the LOCKED 28.87%+2.45%
recognition-control figures as a measured rollback antecedent. Generality sound. ACCEPT on
the CH2 axis.

**CF-4a (udot 4-digit decode) — ACCEPT (generality).** `p2f:263-297`. GRAMMAR-NEUTRAL shape
(`byte_class_from_range_64` digit-run family, `LOCKS.md:426-431`) — NOT CSS-overfit.
"GRAMMAR-NEUTRAL … BUT CURRENTLY ORPHAN" is the honest disposition. CH2 verified the
orphan kernel exists with scalar+dotprod paths (`digit_mac.rs:5,27`) and NO checkasm gate
(`digit` absent from `tests/`, confirming CF-4a's "REQUIRED-NEW" claim). Orphan to CH1/CH4.
ACCEPT on the CH2 axis.

**CF-4b (i8mm dimension decode) — ACCEPT (generality).** `p2f:299-326`. GRAMMAR-NEUTRAL in
shape (digit-run family); the no-P1-antecedent REJECT is CH1's. Generality verdict honest.
ACCEPT on the CH2 axis.

**CF-0 (negative space) — ACCEPT.** `p2f:328-340`. The no-unicode / no-dispatch-self /
no-FNV findings are correctly framed; the UTF-8-continuation classifier is "grammar-neutral
in the abstract, but JSON/other-grammar-only here" — exactly the CH2 distinction between
abstract neutrality and witnessed antecedent. ACCEPT.

## §3 — Cross-artefact consistency (CH2-relevant)

- **The shared-NEON-leaf identity is consistent and correct across ALL SIX artefacts.**
  P2-A CP-A1 (`p2a:401`), P2-B C-B1 (`p2b:225`), P2-C C1/C2 (`p2c:295-308`), P2-D D4
  (`p2d:432`), P2-E G3 (`p2e:258`), and P2-F CF-2 / §1.2 (`p2f:176-218,45-87`) all name
  `byte_class_from_eq_set_64` (via `select_classifier(alphabet)`) as the admissible CSS
  scan kernel and lo6/`classify_tbl4` as the JSON-admissible-only backend. NO artefact
  implies the eq-set kernel is live-JSON-wired; all correctly scope its non-JSON exercise
  to its checkasm corpus-parity test.
- **The lo6 collision is the single most-cited neutrality fact and CH2 independently
  re-derived it this cycle:** `;`(0x3b) & 0x3f = 59; `{`(0x7b) & 0x3f = 59 (bitmask
  collision); `0x7b % 0x3f = 60` (no modulo collision); JSON `{}[],:"` distinct under
  `& 0x3f`. It holds. The guard `(byte & 0x3f)` is verified at `dispatch.rs:106`.
- **Sheets/BBNF-self scoping is uniformly honest.** No candidate in any of the six artefacts
  uses fleet-wide / four-grammar wording; all scope to JSON+CSS-witnessed with SK-V18
  deferral. Gate B's "must be shared (JSON+CSS+Sheets+BBNF-self)" is satisfied in the
  asserted-by-construction sense the locks permit (`LOCKS.md:386-387`); CH2 does NOT require
  a live Sheets/BBNF rider in SK-V17 (`sheets_witness` is a 24-LOC byte-classification trait
  with no `.bbnf`/`BackendRule`, P2-F §1.5).
- **The two overfit re-entry seams are both fenced.** CF-1's routing (the
  `W5C_REQUEST_FACT_PROFILES` const must retire, branching must derive from grammar shape,
  not relocate into projection DATA — P2-F §1.4, P2-A §4, P2-D §4) and D5's flag semantics
  (each bit a `BackendRule` branch-tag — P2-D D5) are the only places a CSS-overfit could
  re-enter, and both are named with the precise Lock-14 trap and a conditional-REVISE guard.

## §4 — V1-REVISE fold verification (re-confirmed at V3 source)

All three V1 CH2 REVISEs (folded in V2) verified STILL FOLDED at the current V3 source.
None carries forward as an orphan.

1. **REVISE-1 (P2-B C-B1)** — HELD at `p2b:225-232`. The eq-set kernel is framed as a
   byte-set membership classifier over `set: &[u8]`, its only exerciser the checkasm /
   corpus-parity test, NOT live JSON. No "already JSON-wired" conflation for the eq-set
   kernel survives. (The deeper layered-backend split P2-F §1.2 owns is intact.)
2. **REVISE-2 (P2-E G1)** — HELD at `p2e:243-250`. The signature carries
   `open:[u8;2], close:[u8;2]`; the scalar sketch tests `open[0]/open[1]/close[0]/close[1]`
   and "never a literal `/`/`*`". The §2 candidate shape matches the §3 neutrality verdict.
3. **REVISE-3 (P2-C §3)** — HELD in P2-C §3 (`p2c:309-328`). C3, C4, C5, C6 each carry an
   explicit one-line grammar-neutral verdict, satisfying the §2.1 per-candidate schema.

This cycle introduces ZERO new REVISE and ZERO REJECT. The pool is grammar-neutral by
construction; every candidate carries a verdict; the shared-NEON-leaf is shared and
honestly scoped. The CH2 axis is converged: 27/27 ACCEPT, no orphan REVISE, V ≤ 5
satisfied (this is V3 — the second consecutive 100%-ACCEPT CH2 cycle).

**Non-CH2 note handed to CH1/CH6 (not a CH2 disposition):** P2-A frontmatter still reads
"Cycle: V2" while P2-B/C/D/E/F read "Cycle: V3" (`p2a:3`). This is a cycle-stamp hygiene
defect for CH1/CH6 to disposition; it does not affect any grammar-neutral verdict (P2-A's
§2/§3/§4 content is unchanged and grammar-neutral-clean). CH2 records it for the
aggregator and takes no generality action on it.

## §5 — Sources (verified this cycle)

- **bbnf source (master HEAD `0ae1caa52`, `git rev-parse HEAD` confirmed):**
  `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:33-60` (real NEON eq-set
  body — `vld1q_u8` stripes `:40-43`, `for &member in set` `vceqq_u8`/`vorrq_u8` fan
  `:55-60`, `set.len()<=8` debug_assert `:34`); `aarch64/byte_class_from_table_64.rs:2-3`
  (scalar passthrough — verified tail-calls `..._scalar`); `dispatch.rs:42`
  (select_classifier), `:101-113` (lo6_table_admissible, `(byte & 0x3f)` MASK at `:106`);
  `aarch64/digit_mac.rs:5,27` (parse_4_digits scalar+dotprod, orphan kernel);
  `skinny/crates/runtime/src/tape/mod.rs:175` (`ValueRef<'doc,'input,K,G: EventGrammar =
  AnyGrammar>` generic); `tape/assembler.rs:42-48` (TapeBuilder fields — no grammar-keyed
  field), `:71` (push_plain_offset branchless u32 append); `json/scan.rs:6` (STRUCTURAL_BYTES
  `{}[],:"`), `:10` (STRUCTURAL_CLASS_TABLE_LO6), `:210-219` (live classify_tbl4 lo6 path);
  `skinny/crates/codegen/src/lib.rs:336,567` (W5C_REQUEST_FACT_PROFILES decl + consumer);
  `skinny/crates/bbnf-simd/tests/` (checkasm_byte_class_from_eq_set_64,
  checkasm_byte_class_from_table_64, classifier_parity, corpus_parity present; NO `digit`
  test — confirms CF-4a's REQUIRED-NEW checkasm claim).
- **Computed (lo6 admissibility, re-derived this cycle):** `;`(0x3b)&0x3f=59,
  `{`(0x7b)&0x3f=59 (collision under bitmask); `0x7b % 0x3f = 60` (no collision under
  modulo); JSON `{}[],:"` = 7 distinct slots under `& 0x3f` (admissible). Every CSS
  alphabet with the `;{` pair inadmissible.
- **S-P2 artefacts (V3 cycle):** `p2a-sota-teardown.md` §2/§3 (CP-A1..A4)/§4;
  `p2b-dav1d-process.md` §2/§3 (C-B0..B3); `p2c-arch-esoterica.md` §2/§3 (C1..C6 + blocked
  routes); `p2d-substrate-tape.md` §2/§3 (D1..D6); `p2e-parse-that-gaps.md` §2/§3 (G1..G5);
  `p2f-grammar-neutral.md` §1-§5 (CF-0..CF-4b).
- **V2 disposition source:** `p2/hardening/V2/CH2.md` §2 (27/27 ACCEPT) + §4 V1-fold
  verification (REVISE-1/2/3 discharged).
- **Locks:** `LOCKS.md:75` (Lock 1 substrate-union / transient-producer), `:386-387`
  (Lock 14 phrase #2 witnessed-grammar scoping), `:393-397` (Lock 14 grammar-neutral
  primitive: delimiter policy = caller data), `:426-431` (byte_class_from_eq_set_64 /
  byte_class_from_range_64 abstract split, digit-run family), `:603` (Lock 14 phrase #1:
  no hand-coded profile arrays).
- **Pass contract:** `restart/prompts/skinny/PASS-2-RESEARCH.md` §2.1 (per-candidate §3
  schema), §3 CH2 (the two gates), §8 axes.
- **Host:** Apple M5 Max, aarch64-apple-darwin. S-P1 commit `0ae1caa52`; master HEAD
  `0ae1caa52`.
