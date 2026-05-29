# CH7 OVERFIT-PRUNE (V1) — S-P2 Research hardening

Lens: CH7 OVERFIT-PRUNE. Pass: S-P2 Research. Cycle: V1. Date: 2026-05-29.
Master HEAD `0ae1caa52`. S-P1 LOCKED `0ae1caa52`. bbnf-simd/runtime verified-at-source `6496fecae`.
Scope (per PASS-2 §3 CH7+ extension + ORCHESTRATOR §3W): no contrivance. The four CH7 probes:
(P1) lightningcss is the fair *materializing* bar (cssparser is a flaw-probe, never a SOTA-beat anchor);
(P2) the candidate set is genuinely grammar-general, not CSS-special-cased;
(P3) no fixture / FNV / broadcast / per-corpus-literal re-entry;
(P4) the CSS typed variants are derived from grammar projections (`BackendRule`/`.bbnf`), not hand-coded.

Disposition vocabulary: ACCEPT / REVISE / REJECT. Counts at foot.

## §0 — Verification performed (orchestrator-citable, this cycle)

Every CH7 disposition below is grounded on a source check, not a re-read of the artefact's own
prose. The probes I ran against the benched tree:

- **lo6 collision (the load-bearing neutrality split, P2-C C1 / P2-F §1.2):** computed mod-0x3f
  slots. JSON `{}[],:"` admissible; CSS `;{}():,`, `;{}`, `:{};` all **COLLIDE** at slot 59
  (`;`=0x3b, `{`=0x7b). Confirmed exactly as P2-C/P2-F/P2-D state. The lo6-TBL-on-CSS route is
  genuinely inadmissible — the artefacts that name the eq-set route as the *admissible* CSS path
  are correct; any artefact still naming lo6/`classify_tbl4` as the CSS scan kernel is contrivance
  (it would silently scalar-fall-back and claim an unearned SIMD win). See CH7-3 below.
- **`byte_class_from_table_64_neon` IS a scalar passthrough** (`bbnf-simd/src/aarch64/byte_class_from_table_64.rs:1-4`,
  body = `crate::scalar::…_scalar(src, table)`). P2-F §1.2 and P2-C §1.3 table-row are accurate;
  the "real 256-table NEON does not exist yet" framing is true, not paper.
- **No `checkasm_digit_mac`** (`ls tests/ | grep digit` = empty). G4/C5/CF-4a orphan-gate is real.
- **7 scalar references present** (`src/scalar/{byte_class_from_eq_set_64, byte_class_from_table_64,
  bitmap_prefix_xor_64, bulk_emit_positions_64, bitmap_next_set_bit, eob_pad_clamp, swar_8byte}.rs`)
  — P2-B §1.2 ledger accurate.
- **`W5C_REQUEST_FACT_PROFILES` is a hand-coded const array** (`codegen/src/lib.rs:336`, iterated
  `:567,:611`) — the Lock-14 overfit re-entry seam P2-A/D/F all name is real and load-bearing.
- **`emit_fact_stream`/`fnv64`/`push_ascii_lower_hex` are String-diagnostic serialization**
  (`generated.rs:5,25-26,425,535,619,628`) — FNV is welded into the fact-stream String, so the
  "FNV retires wholesale with the String, never a primitive" framing is structurally true.
- **`ValueRef<'doc,'input,K,G: EventGrammar>` is genuinely grammar-parametric** (`tape/mod.rs:175`,
  `Copy`/`Clone`/impls all `<…G: EventGrammar>`). The "grammar-neutral by construction" projection
  claim is type-witnessed, not asserted.

## §1 — CH7-1: lightningcss as the fair materializing bar (P1)

**ACCEPT (all six artefacts).** The comparator discipline is honoured precisely:

- P2-A §1.5 sets lightningcss = full owned L2 CSSOM (`src/stylesheet.rs:74-91`, `properties/mod.rs`),
  proven materializing by profiling the comparator's OWN flame (~30% typed-node build+drop;
  `parcel_selectors::parse_selector` 5.04%, `parse_declaration` 4.16%, `drop_in_place::<Token>` 3.95%).
  This is the SAME plane SK-V17 Track 1 must reach via lazy `ValueRef` — the fair bar. **ACCEPT.**
- P2-A §1.6 sets cssparser = token-scan flaw-probe (`CssparserFullParseProbe` iterates and RETURNS
  `()`, materializes nothing; `css_canon_bench.rs:282-403`). Named the SK-V6 sonic-rs `utf8_lossy`
  analogue: "a faster comparator that retains less is NOT a fair >SOTA bar." Kept as the
  *parity oracle* (8-field structural equality), explicitly NOT a speed anchor. **ACCEPT** — this
  is the exact anti-contrivance posture CH7 demands; no artefact anchors a SOTA-beat on cssparser.
- P2-A §1.4 host-blocks asmjson (x86 AVX-512) as a non-candidate, torn down only to mark the
  strict-comparator boundary — no candidate derives from it. **ACCEPT.**
- P2-B §1.4 / G5-gate (`p2b §2 G5`) binds the strict-comparator coupling: `BBNF_SIMD_STRICT=1`,
  "a primitive that clears the checkasm oracle but only beats a permissive comparator is not
  admitted." The strict bar is fixed at fact_stream 0.60–0.79× lightningcss. **ACCEPT.**

No artefact treats the recognizer's 2–3.6× lightningcss headroom as a SOTA-beat — it is correctly
framed as a *masking probe* (P2-A §1.0: "materializes nothing … a masking probe"), not a claim. This
is the single most common contrivance temptation (cite the recognizer number as the win) and every
artefact refuses it. **ACCEPT.**

## §2 — CH7-2: candidate set genuinely grammar-general, not CSS-special-cased (P2)

ACCEPT for the substrate/scan/digit families; one cross-artefact REVISE (§3) and one latent-pin
REVISE (§4). Per-candidate:

- **CF-1 / D1 / C-B2 (tape-append `push_plain_offset`):** ACCEPT. The op takes a `usize` offset,
  carries zero grammar knowledge, and is the SAME op JSON rides today (`assembler.rs:71`). The only
  per-grammar datum is *which positions push*, derived from `BackendRule`/`lower/offset_tape.rs`.
  Not CSS-special-cased.
- **CF-2 / C2 / C-B1 / G3 (structural-membership classifier over per-grammar alphabet):** ACCEPT
  with the §3 correction. The `select_classifier(alphabet)` interface is the genuine Lock-14
  vehicle — alphabet = caller data (`dispatch.rs:42`; `lib.rs:25` `StructuralAlphabet::from_bytes`).
  Witnessed JSON (live) + CSS (new). The honest-fallback guard `lo6_table_admissible` (`:101`) is
  itself the anti-special-case mechanism: P2-F §1.2 explicitly flags that *hand-picking a
  non-colliding CSS subset to force the lo6 path would be the CSS special-case* — the correct
  answer is the alphabet-general eq-set / 256-table primitive. This is exactly CH7's posture
  stated by the agents themselves. ACCEPT the family; the lo6-vs-eq-set inconsistency between P2-A
  and P2-C/E/F is the §3 REVISE.
- **D2 / CF-1 lazy `ValueRef` projection:** ACCEPT-conditional. Genuinely grammar-parametric by
  type (`ValueRef<G: EventGrammar>`, verified §0). The view emitter walks ONE `BackendRule` shape
  for JSON and CSS. The condition (routing derived-from-grammar, not relocated into projection
  DATA) is the §4 / CH7-4 disposition.
- **G1 `comment_body_mask_64`:** REVISE (latent CSS-pin) — see §4.
- **G2 `bracket_depth_mask_64`:** ACCEPT. Open/close masks are the only per-grammar datum (fed by
  alphabet-driven `byte_class_from_eq_set_64`); nested-bracket balance is the most grammar-general
  shape there is. P2-E §3 correctly names JSON (emits brackets) vs CSS (suppresses them) as the
  witness that depth-tracking is a reusable mask op. Not CSS-special-cased.
- **D3/D4/D5 substrate ops:** ACCEPT. `Vec::len`/`truncate` (D3), `CapacityPlan` env-selected (D4),
  sparse-flag side-table (D5) are all grammar-free mechanisms. D5 carries the right GUARD already
  (its flag *semantics* must be a `BackendRule` branch-tag, not a per-rule catalogue — else the
  W5C overfit re-enters in flag form). ACCEPT the mechanism with the guard P2-D itself states.
- **C5 / C6 / CF-4a / CF-4b / G4 (digit/i8mm kernels):** ACCEPT-as-disposed. These are
  grammar-neutral *in shape* but have NO benched CSS antecedent (CH1's province). Every artefact
  disposes them identically: orphan-blocked / gated behind a post-tape typed-`ValueRef` re-profile,
  NOT shortlisted as active. From the CH7 angle this is the *opposite* of contrivance — the agents
  refuse to manufacture a CSS digit hot-leaf to justify a seductive idle `udot` kernel. ACCEPT the
  gating discipline. (CH1 owns whether the antecedent is sufficient; CH7 confirms no contrivance.)

The genuinely grammar-general construction holds: the per-grammar datum across the entire pool is
exactly {alphabet, open/close set, digraph, node-kind enum, `BackendRule` shape} — all DERIVED from
the `.bbnf`, never a hand-keyed CSS branch in a generic crate. The one place the artefacts *could*
have CSS-special-cased (force lo6 by subsetting) is the place they explicitly forbid. Strong PASS on
P2, modulo §3 and §4.

## §3 — CH7-3 (P2/P3 cross-artefact): P2-A CP-A1 names the lo6 route as the CSS scan kernel — contrivance risk

**REVISE — `p2a-sota-teardown.md:217-242` (CP-A1) + `:333` (§3 row) + `:419` (sources).**

CP-A1 is titled "Block-wide byte-class structural classifier (`byte_class_index_64`)" and its SHAPE
specifies "`vqtbl4q_u8` 4-table lookup … Routes through the EXISTING `select_classifier(alphabet)`
→ `byte_class_from_table_64_neon`" and its arch line cites "`vqtbl4q_u8` 4-table classify … Gated by
`lo6_table_admissible`." Its §3 grammar-neutral row asserts CP-A1 is "Already shared JSON↔CSS by
construction … JSON-wired (`json/scan.rs:219`), CSS-to-wire."

This is the precise contrivance P2-C §2-C1 and P2-F §1.2 prove false: **the CSS alphabet collides
mod 0x3f (verified §0), so the lo6-TBL backend is structurally inadmissible for CSS; `vqtbl4q_u8`
never runs and `select_classifier` falls back to scalar.** Worse, `byte_class_from_table_64_neon` —
the kernel CP-A1's shape names — is itself a scalar passthrough today (verified §0). So CP-A1 as
written describes a CSS NEON scan that (a) cannot use the lo6 table it names, and (b) names a "NEON"
kernel that is scalar. CP-A1 would let S-P3 shortlist a "SIMD CSS scan" that silently produces a
scalar result and claims an unearned win — the exact unearned-SIMD contrivance CH7 exists to prune.

CP-A1 is NOT wrong that ONE byte-class scan target exists (~69%, the antecedent is sound) — it is
wrong about the *instruction route* for CSS. The four sibling artefacts (P2-C C2, P2-E G3, P2-F CF-2,
P2-D §1.4) all correctly name the eq-set (`vceqq`×`vorrq`) or 256-table route as the admissible CSS
path and the lo6 path as JSON-only. P2-A is the outlier.

**Concrete fix:** rewrite CP-A1's SHAPE and ARCH to name the **eq-set fan classify** as the CSS
route (the `byte_class_from_eq_set_64_neon` genuine NEON body, `aarch64/byte_class_from_eq_set_64.rs:33`,
≤8-byte CSS delimiter slices), with the lo6/`classify_tbl4` route demoted to "JSON-admissible only;
INADMISSIBLE for the CSS alphabet (`;`/`{` collide mod 0x3f) — falls to eq-set/scalar." Cross-reference
P2-C C2 and P2-F §1.2. Strike the §3 claim "Already shared JSON↔CSS by construction" for the lo6
backend (it is shared at the *interface*, not the backend) and replace with "interface-shared;
CSS routes through the eq-set backend, not the JSON lo6 backend." Otherwise S-P3 inherits a
contrivance: a CSS scan candidate whose named kernel is scalar-in-disguise.

## §4 — CH7-2 (P2): G1 `comment_body_mask_64` scalar sketch hard-pins `/` `*` — latent CSS overfit

**REVISE — `p2e-parse-that-gaps.md:112-122` (G1 scalar sketch) + `:216` (summary row).**

G1's §3 verdict (`:224-230`) correctly asserts the primitive "must be parameterised by the
open/close digraph (`(open:[u8;2], close:[u8;2])`), NOT hard-coded to `/*`/`*/`." But the §2 scalar
reference *sketch* — the load-bearing executable spec per PASS-2 §3 CH6 — hard-codes the bytes:
`src[i]==b'/' && … src[i+1]==b'*'` and `src[i]==b'*' && … src[i+1]==b'/'`. The shape signature in
the summary table (`:216`) is `(&[u8;64], bool) -> (u64,bool)` with NO digraph parameter.

The §3 prose and the §2 sketch contradict: §3 says digraph-parameterized, §2 ships a `/*`-pinned
spec. This is a latent CSS-overfit — if S-P3 lifts the §2 sketch as the spec (the §3 prose is not
the executable artefact, the sketch is), the primitive is silently CSS-pinned and fails Lock 14's
"grammar lives in the grammar." P2-E itself flags the risk for P2-F (`:252-253`: "verify G1/G2's
parameterisation is genuinely per-grammar … and not silently CSS-pinned") — CH7 confirms the §2
sketch IS silently pinned.

**Concrete fix:** rewrite the G1 §2 scalar sketch to take `(src:&[u8;64], open:[u8;2], close:[u8;2],
carry_in:bool)` and test `src[i]==open[0] && src[i+1]==open[1]` / `…==close[0] && …==close[1]`;
update the summary-row shape (`:216`) to `(&[u8;64],[u8;2],[u8;2],bool)->(u64,bool)`. This makes the
*spec* match the §3 neutrality verdict (C/Rust/JS/SQL block comments are different digraphs). Without
this the digraph-neutrality is asserted in prose but contradicted by the only executable artefact.

(G2's scalar sketch `:140-150` takes `open_mask`/`close_mask` as inputs — already digraph/byte-free,
genuinely neutral. No fix needed for G2.)

## §5 — CH7-3: no fixture / FNV / broadcast / per-corpus-literal re-entry (P3)

**ACCEPT (all six artefacts).** Every contrivance vector is fenced:

- **FNV/hex:** recorded as an explicit NON-candidate in P2-A (CP-NONE `:312-317`), P2-B (`§4`,
  `:240-242`), P2-C (`§1, :16`), P2-D (`§4, :370-374`), P2-E (G5 `:201-210`), P2-F (CF-0 `:285-288`).
  Verified §0 that `fnv64`/`push_ascii_lower_hex` are welded into the `emit_fact_stream` String, so
  the "retires wholesale with the String" claim is structurally true, not aspirational. No artefact
  proposes a NEON hex/FNV kernel. ACCEPT.
- **Broadcast (the 24-row one-timing-tuple dishonesty):** P2-A §4 (`:371-372`) binds "every CSS row
  is per-corpus N≥50 median"; P2-D §4 (`:381-385`) binds cold per-parse N≥50 + counters compiled
  only under `bench-counters` (no hot-path instrumentation). The benched `assert!(n>=50)` is cited
  (`css_canon_bench.rs:250`). ACCEPT.
- **Per-corpus capacity literal (the fixture contrivance):** P2-E §4 (`:284-286`) explicitly forbids
  it — "No per-corpus capacity literal (the delimiter alphabet is grammar-derived, the index capacity
  is `input.len()/8+8` as JSON, not a tailwind literal)." P2-D D4 sizes capacity from the *scan
  output* (`scan_structurals(src).positions().len()+8`), not a hardcoded constant. ACCEPT — no
  candidate smuggles a tailwind-tuned constant.
- **Re-opened REDRESS (28+33, 50-55, 60-72, 80, 82-84, 88, 89):** each artefact's §4 ledger names the
  pre-block and the re-open test. CH3 owns the full regression sweep; from the CH7 contrivance angle,
  no candidate dresses a blocked route (PMULL hot body, CTZ bulk consumer, tiny-string wiring) as a
  fast path — P2-C §3 lists each with its measured refutation. ACCEPT.

## §6 — CH7-4: CSS typed variants derived from grammar projections, not hand-coded (P4)

**ACCEPT-conditional (CF-1 / D2), with the W5C retirement as the binding condition — already named.**

The CH7 requirement is that the CSS typed CSSOM (`CssColor`/`CssDimension`/`Selector`/`CssRule`/…)
is *projected from the grammar*, not assembled by a hand-written per-CSS-rule routine. The artefacts
hold this honestly:

- The projection generator walks `BackendRule` (verified: `ValueRef<G: EventGrammar>` is the generic
  vehicle; JSON's `value_from_ref` is one instantiation, the CSS rider the isomorphic one). D2 §2
  and CF-1 both require "the view emitter walks ONE `BackendRule` shape for both JSON and CSS (no
  CSS-keyed branch JSON lacks)." This is genuine derivation, not hand-coding.
- The single contrivance re-entry seam is correctly identified and fenced: `W5C_REQUEST_FACT_PROFILES`
  (verified hand-coded const at `codegen/src/lib.rs:336`). P2-A §4 (`:357-359`), P2-D §4 (`:361-364`),
  P2-F §1.4 (`:99-112`) + §4-1 (`:322-326`) all make CF-1/D1's grammar-neutral verdict *conditional*
  on this array being RETIRED — and explicitly forbid the trap of "relocating its per-rule branching
  into projection DATA" (the disguised-overfit re-entry). Every residual CSS routing entry must name
  its `.bbnf` rule. This is the exact CH7-4 discipline, stated by the agents.
- The witnessed-grammar bound is honest: Sheets/BBNF-self generality is "asserted-by-construction,
  proof deferred to SK-V18" (P2-F §1.5, D2 §2, CF-2 §3), with `sheets_witness` (24-LOC, no
  `BackendRule`) correctly disqualified as a projection rider. No artefact over-claims four-grammar
  generality — the anti-fleet-wide-wording discipline is enforced. ACCEPT.

**Condition (carried, not a new defect):** the grammar-neutral verdict for CF-1/D1/D2 is valid ONLY
if the W5C retirement lands deriving routing from the grammar shape. This is already stated as the
REVISE-trigger in P2-F CF-1 (`:155-160`) and D5's guard. CH7 confirms the condition is correctly
attached and the trap correctly named — no orphan REVISE here, the artefacts self-fence it. ACCEPT.

## §7 — Dispositions (counts + list)

Sections/candidates dispositioned by CH7: **24** (4 cross-cutting probes + 20 candidate rows across
the six artefacts; orphan rows C5/C6/CF-4a/CF-4b/G4 counted once each as a disposed family member).

| # | Target (path) | Disposition | Note |
|---|---|---|---|
| 1 | P2-A §1.5/§1.6 lightningcss/cssparser split | ACCEPT | fair materializing bar; flaw-probe correctly demoted |
| 2 | P2-A CP-A1 (`p2a:217-242,333,419`) | **REVISE** | names lo6/`classify_tbl4` as CSS route — inadmissible (collision); fix to eq-set (§3) |
| 3 | P2-A CP-A2 tape-append | ACCEPT | grammar-free offset sink |
| 4 | P2-A CP-A3 lazy `ValueRef` | ACCEPT | grammar-parametric; W5C condition carried |
| 5 | P2-A CP-A4 tokenize-once | ACCEPT | consumption over neutral index |
| 6 | P2-A CP-NONE/CP-BLOCKED (FNV/digit/asmjson) | ACCEPT | correctly retired/host-blocked |
| 7 | P2-B C-B1 eq-set scan | ACCEPT | strongest-grounded; strict-bar coupled |
| 8 | P2-B C-B2 tape-append | ACCEPT | fact-parity differential framing honest |
| 9 | P2-B C-B3 udot orphan | ACCEPT | process-rejected, not contrived in |
| 10 | P2-B C-B0 admission process G1–G6 | ACCEPT | strict comparator (G5) = lightningcss bar |
| 11 | P2-C C1 lo6 TBL | ACCEPT | correctly marked INADMISSIBLE for CSS (the honest finding) |
| 12 | P2-C C2 eq-set fan | ACCEPT | admissible CSS route; not special-cased |
| 13 | P2-C C3/C4 movemask/CTZ fold | ACCEPT | sub-tasks, no orphan |
| 14 | P2-C C5/C6 udot/i8mm | ACCEPT | orphan-gated, no contrivance |
| 15 | P2-C §3 REDRESS-block flags | ACCEPT | each refutation measured |
| 16 | P2-D D1 `push_plain_offset` | ACCEPT | grammar-free |
| 17 | P2-D D2 lazy `ValueRef` | ACCEPT | W5C condition carried |
| 18 | P2-D D3/D4 checkpoint/one-shot reserve | ACCEPT | capacity from scan output, not literal |
| 19 | P2-D D5 sparse-flag | ACCEPT | guard (BackendRule branch-tag, not per-rule catalogue) correct |
| 20 | P2-D D6 second-substrate | ACCEPT | REJECT-on-sight anchor, correct |
| 21 | P2-E G1 `comment_body_mask_64` (`p2e:112-122,216`) | **REVISE** | §2 scalar sketch hard-pins `/*`/`*/`; contradicts §3 digraph claim (§4) |
| 22 | P2-E G2 `bracket_depth_mask_64` | ACCEPT | mask-input, genuinely neutral |
| 23 | P2-E G3/G4/G5 | ACCEPT | index assembler neutral; G4 orphan-gated; G5 non-candidate |
| 24 | P2-F CF-1..CF-4b + §1.2 lo6 split + §1.4 W5C seam | ACCEPT | the neutrality split + overfit-seam fencing is the CH7 spine, correctly drawn |

**Counts:** ACCEPT 22, REVISE 2, REJECT 0.
ACCEPT rate = 22/24 = **91.7%** (below the 95% convergence floor; V2 fold required).

Both REVISEs are non-orphan (each carries a concrete path:line + fix). Neither is a
grammar-neutrality FAILURE in the candidate's *intent* — both are *spec-vs-prose contrivance
inconsistencies* where a CSS-pinned instruction route (lo6 in P2-A) or a CSS-pinned scalar sketch
(`/*` in P2-E G1) contradicts the artefact's own correctly-stated neutrality. The fix in both cases
is to make the executable/instruction artefact match the neutrality verdict the agent already wrote.

## §8 — V2 fold directives (for the consolidator)

1. **P2-A CP-A1:** rewrite SHAPE/ARCH/§3-row to name the eq-set fan (`byte_class_from_eq_set_64_neon`)
   as the CSS scan route; demote lo6/`classify_tbl4` to JSON-admissible-only with the mod-0x3f
   collision cited; strike the "shared JSON↔CSS by construction" claim for the lo6 *backend*. Align
   with P2-C C2 / P2-F §1.2 / P2-D §1.4. (CH7-3.)
2. **P2-E G1:** parameterise the §2 scalar reference sketch by `(open:[u8;2], close:[u8;2])` and
   update the summary-row signature; make the spec match the §3 digraph-neutrality verdict. (CH7-4.)
3. No FNV / broadcast / capacity-literal / lightningcss-bar fold needed — all clean this cycle.
4. The W5C-retirement condition on CF-1/D1/D2 is correctly attached; carry it forward as the
   shortlist gate, no re-statement needed.
