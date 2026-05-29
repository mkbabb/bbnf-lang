# CH7 OVERFIT-PRUNE (V2) — S-P2 Research hardening

Lens: CH7 OVERFIT-PRUNE. Pass: S-P2 Research. Cycle: V2. Date: 2026-05-29.
Master HEAD `0ae1caa52`. S-P1 LOCKED `0ae1caa52`. bbnf-simd/runtime verified-at-source this cycle.
Scope (per PASS-2 §3 CH7+ extension + ORCHESTRATOR §3W): no contrivance. The four CH7 probes:
(P1) lightningcss is the fair *materializing* bar (cssparser is a flaw-probe, never a SOTA-beat anchor);
(P2) the candidate set is genuinely grammar-general, not CSS-special-cased;
(P3) no fixture / FNV / broadcast / per-corpus-literal re-entry;
(P4) the CSS typed variants are derived from grammar projections (`BackendRule`/`.bbnf`), not hand-coded.

Disposition vocabulary: ACCEPT / REVISE / REJECT. Counts at foot.

## §0 — V1→V2 fold audit (the two carried REVISEs) + fresh source verification

The V1 CH7 (`hardening/V1/CH7.md`) returned ACCEPT 22 / REVISE 2 / REJECT 0 = 91.7% (below the 95%
floor), with TWO non-orphan REVISEs and explicit V2 fold directives (§8). Both are now folded; I
verified each at the artefact AND re-ran every load-bearing source probe (orchestrator-citable, not
a re-read of the artefacts' own prose).

**Fold 1 — V1 CH7-3: P2-A CP-A1 named the lo6/`classify_tbl4` route as the CSS scan kernel
(contrivance: a "SIMD CSS scan" that silently runs scalar). FOLDED.** P2-A is now `Cycle: V2`. CP-A1
is retitled "Block-wide byte-class structural classifier (**eq-set fan for CSS**; lo6 table for JSON)"
(`p2a:228`). Its SHAPE names `byte_class_from_eq_set_64_neon` as the CSS route (`p2a:234-239`); a new
"Why NOT the lo6 table route on CSS" paragraph (`p2a:240-253`) demotes lo6 to JSON-only with the
`& 0x3f` collision cited and the scalar-passthrough fact named; the §3 row (`p2a:401`) reads
"per-grammar *backend* (CSS = eq-set fan, JSON = lo6 table)" and no longer asserts "shared by
construction" for the lo6 backend; a new §4 risk (`p2a:429-434`) makes lo6-on-CSS an explicit REJECT.
Aligns with P2-C C2 / P2-F §1.2 / P2-D §1.4. The contrivance is removed at the executable artefact.

**Fold 2 — V1 CH7-4: P2-E G1 `comment_body_mask_64` §2 scalar sketch hard-pinned `/*`/`*/` (latent
CSS overfit; the executable spec contradicted the §3 digraph-neutral verdict). FOLDED.** P2-E is now
`Cycle: V2`. G1's SHAPE signature is `(src:&[u8;64], open:[u8;2], close:[u8;2], in_comment_carry:bool)`
(`p2e:104`); the §2 scalar sketch tests `src[i]==open[0] && src[i+1]==open[1]` / `…==close[0] &&
…==close[1]` and never a literal `/`/`*` (`p2e:120-129`); the summary-row shape is now
`(&[u8;64], open:[u8;2], close:[u8;2], bool) -> (u64,bool)` (`p2e:235`); the §3 verdict
(`p2e:243-250`) cross-references the sketch as the witness. The spec now matches the neutrality verdict.

**Fresh source verification this cycle (the V2 ground truth):**

- **lo6 `& 0x3f` collision is real and is a MASK not a modulo.** `dispatch.rs:106` computes
  `let slot = (byte & 0x3f) as usize;` (verified at source), then `dispatch.rs:107-110` rejects on
  `seen[slot]`. Arithmetic re-computed: `0x3b & 0x3f = 0x3b`, `0x7b & 0x3f = 0x3b` → COLLIDE at slot
  59; true modulo `0x7b % 0x3f = 0x3c` → would NOT collide. Every artefact that states the
  mask-not-modulo distinction (P2-A:246, P2-B:150-152, P2-C:155-159, P2-D:150-156, P2-F:65-68) is
  exactly correct. The lo6-on-CSS route is genuinely inadmissible.
- **`byte_class_from_table_64_neon` IS a scalar passthrough.** `aarch64/byte_class_from_table_64.rs:3`
  body = `crate::scalar::byte_class_from_table_64::byte_class_from_table_64_scalar(src, table)`
  (verified). The "real 256-table NEON does not exist yet" framing (P2-F §1.2, P2-C §1.3 table) is true.
- **`byte_class_from_eq_set_64_neon` IS a genuine NEON body.** `aarch64/byte_class_from_eq_set_64.rs:33`
  loads four `vld1q_u8` stripes and OR-reduces per-member equality (verified `:38-45`). The CSS route
  the artefacts name is a real vector body, not a stub — so routing CSS through it is an HONEST SIMD
  claim, the exact opposite of the V1 lo6 contrivance.
- **`W5C_REQUEST_FACT_PROFILES` is a hand-coded const array** (`codegen/src/lib.rs:336`, iterated
  `:567`/`:611`) — the Lock-14 overfit re-entry seam P2-A/D/F all name as the CF-1/D1 retirement
  condition is real and load-bearing.
- **No `checkasm_digit_mac`** (`ls tests/ | grep digit` = empty) — the G4/C5/CF-4a orphan-gate is real.
- **`checkasm_byte_class_from_eq_set_64.rs` exists** — the eq-set CSS route's parity gate (the
  scalar-oracle-first / dav1d discipline) is real, not aspirational.
- **`ValueRef<'doc,'input,K,G: EventGrammar>` is genuinely grammar-parametric** (`tape/mod.rs:175`;
  `Copy`/`Clone`/impls all `<…G: EventGrammar>` `:183,:185,:191`). The "grammar-neutral by
  construction" projection claim (P2-A CP-A3, P2-D D2, P2-F CF-1) is type-witnessed, not asserted.
- **FNV welded into the fact-stream String** (`generated.rs:5` `emit_fact_stream(...) -> String`;
  `:619` `fnv64`; `:628` `push_ascii_lower_hex(out:&mut String, …)`) — the "FNV retires wholesale
  with the String, never a primitive" framing is structurally true.

## §1 — CH7-1: lightningcss as the fair materializing bar (P1)

**ACCEPT (all six artefacts).** The comparator discipline is honoured precisely and the V1 ACCEPT
holds unchanged on V2 — no fold touched this surface:

- P2-A §1.5 sets lightningcss = full owned L2 CSSOM (`src/stylesheet.rs:74-91`, `properties/mod.rs`),
  proven materializing by profiling the comparator's OWN flame (~30% typed-node build+drop;
  `parcel_selectors::parse_selector` 5.04%, `parse_declaration` 4.16%, `drop_in_place::<Token>` 3.95%).
  The SAME plane SK-V17 Track 1 must reach via lazy `ValueRef` — the fair bar. LOCKED V4 band
  fact_stream 0.60–0.77× lcss is the materialization-plane gap, NOT a recognizer headroom claim. **ACCEPT.**
- P2-A §1.6 sets cssparser = token-scan flaw-probe (`CssparserFullParseProbe` iterates and RETURNS
  `()`, materializes nothing; `css_canon_bench.rs:282-403`), named the SK-V6 sonic-rs `utf8_lossy`
  analogue, kept as the 8-field structural-equality parity ORACLE, explicitly NOT a speed anchor.
  The single grep hit `p2a:205` is the anti-contrivance statement itself: "Beating cssparser is NOT a
  SOTA claim; beating lightningcss is." **ACCEPT.**
- P2-A §1.0/§1.4 frame the recognizer's 2.01–3.09× lcss headroom as a *masking probe* ("materializes
  nothing"), and host-block asmjson (x86 AVX-512) torn down only to mark the strict-comparator boundary
  with no candidate derived from it. The single most common contrivance temptation — citing the
  recognizer number as the win — is refused by every artefact. **ACCEPT.**
- P2-B §1.4 + G5 gate (`p2b:216`) bind the strict comparator: lightningcss full-CSSOM = speed bar,
  `BBNF_SIMD_STRICT=1`, "a primitive that clears the checkasm oracle but only beats a permissive
  comparator is not admitted." The strict bar is fixed at fact_stream 0.60–0.79× lightningcss. **ACCEPT.**

No grep hit anchors a SOTA-beat on cssparser or the recognizer numbers (verified §0 cross-check).

## §2 — CH7-2: candidate set genuinely grammar-general, not CSS-special-cased (P2)

**ACCEPT (all candidate families).** Both V1 latent-pin REVISEs (the P2-A lo6 instruction route and the
P2-E G1 scalar sketch) are folded; with those folds the genuinely grammar-general construction holds
with no remaining special-case. Per-candidate:

- **CP-A1 / C1+C2 / CF-2 / G3 / D4 (structural-membership classifier over a per-grammar alphabet):**
  ACCEPT. The V1 contrivance (lo6-as-CSS-route) is removed. Every artefact now routes CSS through the
  eq-set fan `byte_class_from_eq_set_64_neon` (a verified genuine NEON body, §0) and demotes lo6 to
  JSON-only with the `& 0x3f` collision cited. The `select_classifier(alphabet)` interface is the
  Lock-14 vehicle — alphabet = caller data (`dispatch.rs:42`, `StructuralAlphabet::from_bytes`
  `lib.rs:25`). The honest-fallback guard `lo6_table_admissible` (`:101`) is itself the anti-special-case
  mechanism: P2-C §3, P2-F §1.2 and P2-D D6 all explicitly flag that hand-picking a non-colliding CSS
  subset to FORCE the lo6 path would be the CSS special-case — and forbid it. The agents state CH7's
  posture themselves. ACCEPT.
- **CP-A2 / C-B2 / D1 (tape-append `push_plain_offset`):** ACCEPT. Takes a `usize`/`u32` offset, zero
  grammar knowledge, the SAME op JSON rides today (`assembler.rs:71`). The only per-grammar datum is
  WHICH positions push, derived from `BackendRule`/`lower/offset_tape.rs`. Not CSS-special-cased.
- **CP-A3 / C-B (consumer) / D2 / CF-1 (lazy `ValueRef` projection):** ACCEPT. Genuinely
  grammar-parametric by type (`ValueRef<G: EventGrammar>`, verified §0). The view emitter walks ONE
  `BackendRule` shape for JSON and CSS. The W5C routing-retirement condition (§6) is correctly attached.
- **G1 `comment_body_mask_64`:** ACCEPT (the V1 latent-pin is folded). The §2 scalar sketch is now
  digraph-parameterised `(open:[u8;2], close:[u8;2])` and tests `open[0]/open[1]/close[0]/close[1]`,
  never `/`/`*` (`p2e:120-129`). The executable spec now matches the §3 neutrality verdict — block
  comments are a 2-byte-open/2-byte-close opaque region across C/Rust/JS/SQL, the digraph the only datum.
- **G2 `bracket_depth_mask_64`:** ACCEPT. Open/close masks (fed by alphabet-driven
  `byte_class_from_eq_set_64`) are the only per-grammar datum; nested-bracket balance is the most
  grammar-general shape there is (JSON arrays, CSS component blocks, BBNF groups, Sheets parens). P2-E §3
  names JSON (emits brackets) vs CSS (suppresses them) as the witness that depth-tracking is reusable.
- **D3/D4/D5 substrate ops:** ACCEPT. `Vec::len`/`truncate` (D3), `CapacityPlan` env-selected (D4),
  sparse-flag side-table (D5) are grammar-free mechanisms. D5 carries the right GUARD (its flag
  *semantics* must be a `BackendRule` branch-tag, not a per-rule catalogue — else W5C re-enters in flag
  form; `p2d:404-412`). ACCEPT the mechanism with the guard P2-D states.
- **C5/C6 / CF-4a/CF-4b / G4 (digit/i8mm kernels):** ACCEPT-as-disposed. Grammar-neutral in shape but
  NO benched CSS antecedent (CH1's province). Every artefact disposes them identically: orphan-blocked /
  gated behind a post-tape typed-`ValueRef` re-profile, NOT shortlisted as active. From the CH7 angle
  this is the OPPOSITE of contrivance — the agents refuse to manufacture a CSS digit hot-leaf to justify
  a seductive idle `udot`/`usmmla` kernel. ACCEPT the gating discipline.

The genuinely grammar-general construction holds across the entire pool: the per-grammar datum is
exactly {alphabet, open/close digraph, open/close set, node-kind enum, `BackendRule` shape} — all
DERIVED from the `.bbnf`, never a hand-keyed CSS branch in a generic crate. The two places the V1
artefacts CSS-pinned (lo6 instruction route, `/*`-literal sketch) are both folded. Strong PASS on P2.

## §3 — CH7-3: no fixture / FNV / broadcast / per-corpus-literal re-entry (P3)

**ACCEPT (all six artefacts).** Every contrivance vector remains fenced (unchanged from V1, re-verified):

- **FNV/hex:** recorded as an explicit NON-candidate in P2-A (CP-NONE `:380-385`), P2-B (`:262-264`),
  P2-C (`§3 item 4` + `:16`), P2-D (`:469-473`), P2-E (G5 `:220-229`), P2-F (CF-0 `:298-301`). Verified
  §0 that `fnv64`/`push_ascii_lower_hex` write into the `emit_fact_stream` String, so "retires wholesale
  with the String" is structurally true. No artefact proposes a NEON hex/FNV kernel. ACCEPT.
- **Broadcast (the 24-row one-timing-tuple dishonesty):** P2-A §4 (`:446-449`) binds every CSS row to a
  per-corpus N≥50 median and explicitly distinguishes the SIMD `vdupq_n_u8` lane-splat from the
  evidence-measurement broadcast pre-block; P2-C §2-C2 (`:191-194`) makes the SAME distinction
  explicitly (the splat "is wholly unrelated to the §0.4 broadcast pre-block, which forbids the
  evidence-measurement practice of projecting one CSS timing tuple across 24 rows"); P2-D §4 (`:480-484`)
  binds cold per-parse N≥50 with counters compiled only under `bench-counters`. ACCEPT — the splat-vs-
  broadcast disambiguation is itself an anti-contrivance refinement.
- **Per-corpus capacity literal (the fixture contrivance):** P2-E §4 (`:307-309`) forbids it — "No
  per-corpus capacity literal (the delimiter alphabet is grammar-derived, the index capacity is
  `input.len()/8+8` as JSON, not a tailwind literal)." P2-D D4 sizes capacity from the SCAN OUTPUT
  (`scan_structurals(src).positions().len()+8`), not a hardcoded constant. ACCEPT — no candidate
  smuggles a tailwind-tuned constant.
- **Re-opened REDRESS (28+33, 50-55, 60-72, 80, 82-84, 88, 89):** each artefact's §4 ledger names the
  pre-block and the re-open test; P2-C §3 lists each blocked instruction route with its measured
  refutation (PMULL -10/-12/-15%, CTZ bulk consumer, tiny-string). CH3 owns the full regression sweep;
  from the CH7 contrivance angle, no candidate dresses a blocked route as a fast path. ACCEPT.

## §4 — CH7-4: CSS typed variants derived from grammar projections, not hand-coded (P4)

**ACCEPT (CF-1 / D1 / D2 / CP-A3), with the W5C retirement as the binding shortlist condition — already
named and correctly attached.** No fold touched this surface; the V1 ACCEPT-conditional holds and the
condition remains a self-fenced, non-orphan REVISE-trigger inside the artefacts (not an open CH7 defect):

- The projection generator walks `BackendRule` (verified: `ValueRef<G: EventGrammar>` is the generic
  vehicle; JSON's `value_from_ref` is one instantiation, the CSS rider the isomorphic one). D2 §2,
  CF-1 and CP-A3 all require "the view emitter walks ONE `BackendRule` shape for both JSON and CSS (no
  CSS-keyed branch JSON lacks)." Genuine derivation, not hand-coding.
- The single contrivance re-entry seam is correctly identified and fenced: `W5C_REQUEST_FACT_PROFILES`
  (verified hand-coded const at `codegen/src/lib.rs:336`). P2-A §4 (`:426-428`), P2-D §4 (`:460-463`),
  P2-F §1.4 (`:104-116`) + §4-1 (`:340-344`) all make CF-1/D1's grammar-neutral verdict CONDITIONAL on
  this array being RETIRED — and explicitly forbid the trap of "relocating its per-rule branching into
  projection DATA" (the disguised-overfit re-entry, also caught by D5's guard `p2d:404-412`). Every
  residual CSS routing entry must name its `.bbnf` rule. This is the exact CH7-4 discipline, stated by
  the agents.
- The witnessed-grammar bound is honest: Sheets/BBNF-self generality is "asserted-by-construction, proof
  deferred to SK-V18" (P2-F §1.5, D2 §3 `:430`, CF-2 §3), with `sheets_witness` (24-LOC, no
  `BackendRule`) correctly disqualified as a projection rider. No artefact over-claims four-grammar
  generality — the anti-fleet-wide-wording discipline is enforced. ACCEPT.

**Condition (carried, not a new defect):** the grammar-neutral verdict for CF-1/D1/D2 is valid ONLY if
the W5C retirement lands deriving routing from the grammar shape. This is correctly attached as the
REVISE-trigger in P2-F CF-1 (`:160-164`) and D5's guard, and as the SYNTHESIS Layout close gate. CH7
confirms the condition is correctly attached and the trap correctly named — no orphan REVISE. ACCEPT.

## §5 — New-this-cycle scan for fresh contrivance (V2 introduced no regression)

The V2 fold rewrote CP-A1 (P2-A) and G1 (P2-E). I checked the rewrites for a NEW contrivance the fold
might have introduced:

- **CP-A1 rewrite does not over-claim the eq-set NEON.** P2-A SR (`p2a:254-258`) correctly states the
  eq-set scalar reference is the existing `delimiters.contains(&byte)` membership and the NEON
  `vceqq_u8`/`vorrq_u8` fan is "Already a real NEON impl (not a passthrough)" — verified true at source
  (§0). The CK (`p2a:259-267`) correctly notes the eq-set kernel is NOT live in JSON prod (JSON uses the
  lo6 table) so its parity gate is its sole exercise until CSS wires it — an honest disclosure, not a
  "JSON-witnessed" over-claim. No fresh contrivance.
- **P2-B §C-B1 (`p2b:131-160`) makes the SAME honest disclosure independently:** "JSON-wired" is true of
  the shared `select_classifier` INTERFACE, FALSE of the eq-set NEON LEAF, which is exercised today only
  by the differential harness (verified §0: `checkasm_byte_class_from_eq_set_64.rs` + corpus-parity
  smoke, no live JSON prod consumer). This is a strengthening over V1 — the "JSON-wired" claim is now
  layer-split so it cannot be read as an unearned witness. No contrivance.
- **G1 rewrite does not pin the NEON body to `/*`.** The NEON construction (`p2e:131-134`) compares
  `chunk` against `open[0]`/`open[1]`/`close[0]`/`close[1]` (the digraph parameters), reusing the
  `escape_mask_64` `overflowing_add` carry idiom (`lib.rs:188`) — so the kernel is alphabet-driven by
  construction and introduces no PMULL (REDRESS-88 clean) and no `/`/`*` literal. No fresh contrivance.

The V2 fold is clean: it removed two latent CSS-pins and introduced no new over-claim.

## §6 — Dispositions (counts + list)

Sections/candidates dispositioned by CH7 this cycle: **24** (4 cross-cutting probes + 20 candidate rows
across the six artefacts; orphan rows C5/C6/CF-4a/CF-4b/G4 counted once each as a disposed family member),
identical census to V1 for comparability.

| # | Target (path) | V1 | V2 | Note |
|---|---|---|---|---|
| 1 | P2-A §1.5/§1.6 lightningcss/cssparser split | ACCEPT | ACCEPT | fair materializing bar; flaw-probe correctly demoted |
| 2 | P2-A CP-A1 (`p2a:228-284,401,429-434`) | REVISE | **ACCEPT** | FOLDED — CSS now routes through eq-set fan; lo6 demoted JSON-only with `& 0x3f` collision cited |
| 3 | P2-A CP-A2 tape-append | ACCEPT | ACCEPT | grammar-free offset sink |
| 4 | P2-A CP-A3 lazy `ValueRef` | ACCEPT | ACCEPT | grammar-parametric (type-witnessed §0); W5C condition carried |
| 5 | P2-A CP-A4 tokenize-once | ACCEPT | ACCEPT | consumption over neutral index |
| 6 | P2-A CP-NONE/CP-BLOCKED (FNV/digit/asmjson) | ACCEPT | ACCEPT | correctly retired/host-blocked |
| 7 | P2-B C-B1 eq-set scan | ACCEPT | ACCEPT | strongest-grounded; JSON-wired layer-split honest (§5) |
| 8 | P2-B C-B2 tape-append | ACCEPT | ACCEPT | fact-parity differential framing honest |
| 9 | P2-B C-B3 udot orphan | ACCEPT | ACCEPT | process-rejected at G1/G2/G4, not contrived in |
| 10 | P2-B C-B0 admission process G1–G6 | ACCEPT | ACCEPT | strict comparator (G5) = lightningcss bar |
| 11 | P2-C C1 lo6 TBL | ACCEPT | ACCEPT | correctly marked INADMISSIBLE for CSS (the honest finding) |
| 12 | P2-C C2 eq-set fan | ACCEPT | ACCEPT | admissible CSS route; not special-cased |
| 13 | P2-C C3/C4 movemask/CTZ fold | ACCEPT | ACCEPT | sub-tasks, no orphan |
| 14 | P2-C C5/C6 udot/i8mm | ACCEPT | ACCEPT | orphan-gated, no contrivance |
| 15 | P2-C §3 REDRESS-block flags | ACCEPT | ACCEPT | each refutation measured |
| 16 | P2-D D1 `push_plain_offset` | ACCEPT | ACCEPT | grammar-free |
| 17 | P2-D D2 lazy `ValueRef` | ACCEPT | ACCEPT | W5C condition carried |
| 18 | P2-D D3/D4 checkpoint/one-shot reserve | ACCEPT | ACCEPT | capacity from scan output, not literal; D3 re-profile obligation honest |
| 19 | P2-D D5 sparse-flag | ACCEPT | ACCEPT | guard (BackendRule branch-tag, not per-rule catalogue) correct |
| 20 | P2-D D6 second-substrate | ACCEPT | ACCEPT | REJECT-on-sight anchor, correct |
| 21 | P2-E G1 `comment_body_mask_64` (`p2e:104,120-129,235`) | REVISE | **ACCEPT** | FOLDED — §2 sketch now digraph-parameterised `(open:[u8;2],close:[u8;2])`, no `/*` literal |
| 22 | P2-E G2 `bracket_depth_mask_64` | ACCEPT | ACCEPT | mask-input, genuinely neutral |
| 23 | P2-E G3/G4/G5 | ACCEPT | ACCEPT | index assembler neutral; G4 orphan-gated; G5 non-candidate |
| 24 | P2-F CF-1..CF-4b + §1.2 lo6 split + §1.4 W5C seam | ACCEPT | ACCEPT | the neutrality split + overfit-seam fencing is the CH7 spine, correctly drawn |

**Counts:** ACCEPT 24, REVISE 0, REJECT 0.
ACCEPT rate = 24/24 = **100%** (above the 95% convergence floor).

Both V1 REVISEs (P2-A CP-A1 lo6-as-CSS-route; P2-E G1 `/*`-pinned scalar sketch) are FOLDED at the
executable artefact and re-verified against the benched source this cycle. No new contrivance was
introduced by the fold (§5). The W5C-retirement condition on CF-1/D1/D2 is correctly attached as a
self-fenced shortlist gate, not an open CH7 defect.

## §7 — V3 fold directives (for the consolidator)

**None from CH7.** This lens returns zero REVISE / zero REJECT this cycle. The two V1 dispositions are
discharged. If the wave reaches a second consecutive ≥95% cycle (this is the first 100% CH7 cycle on
S-P2), CH7 raises no obstacle to advancing S-P3. The only carried item is the W5C-retirement
shortlist gate, which is already named in P2-A/D/F and is a self-fenced condition, not an orphan REVISE.
