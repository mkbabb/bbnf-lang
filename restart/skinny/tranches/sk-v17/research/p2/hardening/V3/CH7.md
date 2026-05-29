# CH7 OVERFIT-PRUNE (V3) — S-P2 Research hardening

Lens: CH7 OVERFIT-PRUNE. Pass: S-P2 Research. Cycle: V3. Date: 2026-05-29.
Master HEAD `0ae1caa52`. S-P1 LOCKED `0ae1caa52`. bbnf-simd/runtime/codegen verified-at-source this cycle.
Scope (PASS-2 §3 CH7+ extension + ORCHESTRATOR §3W): no contrivance. The four CH7 probes:
(P1) lightningcss is the fair *materializing* bar (cssparser = flaw-probe, never a SOTA-beat anchor);
(P2) the candidate set is genuinely grammar-general, not CSS-special-cased;
(P3) no fixture / FNV / broadcast / per-corpus-literal re-entry;
(P4) the CSS typed variants are derived from grammar projections (`BackendRule`/`.bbnf`), not hand-coded.

Disposition vocabulary: ACCEPT / REVISE / REJECT. Counts at foot.

## §0 — V2→V3 census + fresh source verification (NOT an artefact re-read)

V2 CH7 (`hardening/V2/CH7.md`) returned ACCEPT 24 / REVISE 0 / REJECT 0 = 100%, discharging both
V1 carried REVISEs (P2-A CP-A1 lo6-as-CSS-route fold; P2-E G1 `/*`-pinned scalar sketch fold). For
V3, five of the six P2 agents RE-RAN: `p2b`, `p2c`, `p2d`, `p2e`, `p2f` are now `Cycle: V3`
(verified `grep -m1 "Cycle:"`); `p2a` is unchanged `Cycle: V2` (the lo6/eq-set fold is intact). V3
is therefore a FRESH review of the re-written artefacts, not a carry-forward — every CH7 probe is
re-run against the V3 prose AND re-grounded at the benched source this cycle.

**Fresh source verification this cycle (the V3 ground truth, orchestrator-citable):**

- **lo6 `& 0x3f` collision is real and is a MASK not a modulo.** `dispatch.rs:106`
  `let slot = (byte & 0x3f) as usize;` (verified at source), `:107` `if seen[slot]` rejects.
  Arithmetic re-computed this cycle: `0x3b & 0x3f = 0x3b = 59`, `0x7b & 0x3f = 0x3b = 59` → COLLIDE
  at slot 59; true modulo `0x7b % 0x3f = 0x3c = 60` → would NOT collide. Every artefact that states
  the mask-not-modulo distinction (P2-A:246, P2-B:147-152, P2-C:155-159, P2-D:150-156, P2-E:331,
  P2-F:65-68) is exactly correct. The lo6-on-CSS route is genuinely inadmissible.
- **`SelectedBackend` is `Scalar`/`NeonTbl4` ONLY** (`dispatch.rs:12-15`, verified) — there is NO
  eq-set arm in the classifier dispatch. This confirms P2-B C-B1's load-bearing layer-split: the
  eq-set NEON leaf is NOT a live JSON production consumer (it is not a `SelectedBackend` arm).
- **`byte_class_from_eq_set_64_neon` IS a genuine NEON body** (`aarch64/byte_class_from_eq_set_64.rs:33`,
  verified `:40-46`: `vld1q_u8` × 4 stripes + `vdupq_n_u8` + per-member OR-reduce). The CSS route
  every V3 artefact names is a real vector body, not a stub — routing CSS through it is an HONEST
  SIMD claim, the exact inverse of the V1 lo6 contrivance.
- **`byte_class_from_table_64_neon` IS a scalar passthrough** (`aarch64/byte_class_from_table_64.rs:3`,
  verified: body = `crate::scalar::byte_class_from_table_64::byte_class_from_table_64_scalar(src, table)`).
  The "real 256-table NEON does not exist yet" framing (P2-C §1.3 table row, P2-F §1.2) is true.
- **The eq-set leaf is NOT a live JSON prod consumer** — `grep -rn byte_class_from_eq_set_64
  runtime/src/grammars/json/` = EMPTY (verified); JSON scan routes through the lo6 table
  (`json/scan.rs:219` `classify_structural_terminator_block_from_table`). The eq-set checkasm uses the
  JSON structural set `b"{}[],:\"\\"` (`checkasm_byte_class_from_eq_set_64.rs:197`) + a twitter.json
  corpus-parity slide (`:294-301`) as a PARITY arg, NOT a live consumer. P2-B C-B1's §1.2 honest
  disclosure ("'JSON-wired' is true of the interface, FALSE of the eq-set leaf") is verified true.
- **`W5C_REQUEST_FACT_PROFILES` is a hand-coded const array** (`codegen/src/lib.rs:336`, iterated
  `:567`/`:611`, verified) — the Lock-14 overfit re-entry seam P2-A §4 / P2-D §4 / P2-F §1.4 all name
  as the CF-1/D1/D2 retirement condition is real and load-bearing.
- **No `checkasm_digit`** (`ls tests/ | grep -i digit` = empty, verified) — the G4/C5/CF-4a orphan
  REQUIRED-NEW checkasm gate is real.
- **`ValueRef<'doc,'input,K,G: EventGrammar>` is genuinely grammar-parametric** (`tape/mod.rs:175`;
  Copy/Clone/impls all `<…G: EventGrammar>` `:183,:185,:191`, verified). The "grammar-neutral by
  construction" projection claim (CP-A3 / D2 / CF-1) is type-witnessed, not asserted.
- **FNV welded into the fact-stream String** (`generated.rs:5` `emit_fact_stream(...) -> String`;
  `:619` `fnv64`; `:628` `push_ascii_lower_hex(out:&mut String, …)`, verified) — "FNV retires
  wholesale with the String, never a primitive" is structurally true.

## §1 — CH7-1: lightningcss as the fair materializing bar (P1)

**ACCEPT (all six artefacts).** The comparator discipline holds on V3; the five re-written artefacts
did not regress the fair-bar split, and one (P2-B) strengthened it:

- P2-A §1.5 sets lightningcss = full owned L2 CSSOM (`src/stylesheet.rs:74-91`), proven materializing
  by profiling the comparator's OWN flame (~30% typed-node build+drop). §1.6 sets cssparser =
  token-scan flaw-probe (`CssparserFullParseProbe` returns `()`, materializes nothing) and the
  anti-contrivance statement is verbatim at `p2a:205`: "Beating cssparser is NOT a SOTA claim;
  beating lightningcss is." The recognizer's 2.01–3.09× lcss headroom is framed as a *masking probe*
  (§1.0/§1.4), never the win. **ACCEPT.**
- P2-B §1.4 (V3-rewritten) binds the strict comparator precisely: "a primitive's *parity* anchor is
  the scalar oracle … its *speed-admission* anchor is the strict comparator plane (lightningcss =
  materializing full-CSSOM; cssparser = token-scan flaw-probe)", `BBNF_SIMD_STRICT=1`, strict bar
  fixed at fact_stream 0.60–0.79× lightningcss. The C-B0 admission process makes this a formal GATE
  (G5 row: "the moved row beats the STRICT (materializing) bar, not a permissive one"). This is a
  V3 strengthening — the fair bar is now a gate condition, not just prose. **ACCEPT.**
- P2-C §1.1/§1.2 ground the host envelope (NEON only) and the scan-does-nothing-today finding with no
  comparator over-claim; P2-D §1.0 antecedent line / §4 N≥50 cold; P2-E §4 / P2-F §5 both name the
  lightningcss=materializing / cssparser=flaw-probe split correctly. No artefact anchors a SOTA-beat
  on cssparser or on the recognizer numbers (verified §0). **ACCEPT.**

## §2 — CH7-2: candidate set genuinely grammar-general, not CSS-special-cased (P2)

**ACCEPT (all candidate families).** The V3 re-write preserved every grammar-general framing the V2
fold established, and the new V3 material (P2-E's §1.3 comment/bracket suppressor analysis) is itself
grammar-neutral mask algebra, not a CSS special-case. Per-candidate:

- **CP-A1 / C1+C2 / CF-2 / G3 / D4 (structural-membership classifier over a per-grammar alphabet):**
  ACCEPT. Every artefact routes CSS through the eq-set fan `byte_class_from_eq_set_64_neon` (verified
  genuine NEON body, §0) and demotes lo6 to JSON-only with the `& 0x3f` collision cited. The
  `select_classifier(alphabet)` interface is the Lock-14 vehicle (alphabet = caller data,
  `dispatch.rs:42`). P2-C §3 / P2-F §1.2 / P2-D D6 all explicitly forbid the would-be CSS special-case
  (hand-picking a non-colliding CSS subset to FORCE the lo6 path) and name the honest 256-table/eq-set
  answer — the agents state CH7's posture themselves. ACCEPT.
- **CP-A2 / C-B2 / D1 (tape-append `push_plain_offset`):** ACCEPT. Takes a `u32` offset, zero grammar
  knowledge, the SAME op JSON rides (`assembler.rs:71`). Per-grammar datum is only WHICH positions
  push, derived from `BackendRule`/`lower/offset_tape.rs`. Not CSS-special-cased.
- **CP-A3 / C-B (consumer) / D2 / CF-1 (lazy `ValueRef` projection):** ACCEPT. Grammar-parametric by
  type (`ValueRef<G: EventGrammar>`, verified §0). One view emitter walks ONE `BackendRule` shape for
  JSON and CSS. W5C routing-retirement condition correctly attached (§4).
- **G1 `comment_body_mask_64`:** ACCEPT (the V2 fold is preserved in V3). The §2 scalar sketch
  (`p2e:120-129`) tests `src[i]==open[0] && src[i+1]==open[1]` / `…==close[0] && …==close[1]` and
  never a literal `/`/`*`; the §3 verdict (`p2e:243-250`) cross-references the sketch as the digraph
  witness. A 2-byte-open/2-byte-close opaque region is the grammar-general shape (C/Rust/JS/SQL).
- **G2 `bracket_depth_mask_64`:** ACCEPT. Open/close masks (fed by alphabet-driven
  `byte_class_from_eq_set_64`) are the only per-grammar datum; nested-bracket balance is the most
  grammar-general shape (JSON arrays, CSS component blocks, BBNF groups, Sheets parens). P2-E §3 names
  JSON-emits-brackets vs CSS-suppresses-brackets as the witness that depth-tracking is reusable.
- **P2-E §1.3 "stage-1 then suppress" (the NEW V3 material):** ACCEPT. The comment + bracket-depth
  suppressors are framed as the two AND-NOT masks CSS needs beyond JSON's two (whitespace, string),
  with the digraph (G1) and open/close-set (G2) as the ONLY per-grammar data. This is the SOTA
  simdjson `find_structural_bits` → `string_scanner` body-suppression shape generalised, not a CSS
  branch. The fact that CSS needs two MORE suppressors than JSON is an additive-mask observation, not
  a special-case — JSON is the witness that the suppressor set is grammar-extensible. ACCEPT.
- **D3/D4/D5 substrate ops:** ACCEPT. `Vec::len`/`truncate` (D3), `CapacityPlan` env-selected (D4),
  sparse-flag side-table (D5) are grammar-free mechanisms. D5 carries the right GUARD (flag
  *semantics* must be a `BackendRule` branch-tag, not a per-rule catalogue — else W5C re-enters in
  flag form; `p2d:404-412`). ACCEPT with the guard P2-D states.
- **C5/C6 / CF-4a/CF-4b / G4 (digit/i8mm kernels):** ACCEPT-as-disposed. Grammar-neutral in shape but
  NO benched CSS antecedent (CH1's province). Every artefact disposes them identically: orphan-blocked
  / gated behind a post-tape typed-`ValueRef` re-profile, NOT shortlisted as active. From the CH7
  angle this is the OPPOSITE of contrivance — the agents refuse to manufacture a CSS digit hot-leaf to
  justify a seductive idle `udot`/`usmmla` kernel. ACCEPT the gating discipline.

The per-grammar datum across the entire pool is exactly {alphabet, open/close digraph, open/close set,
node-kind enum, `BackendRule` shape} — all DERIVED from the `.bbnf`, never a hand-keyed CSS branch in
a generic crate. Strong PASS on P2.

## §3 — CH7-3: no fixture / FNV / broadcast / per-corpus-literal re-entry (P3)

**ACCEPT (all six artefacts).** Every contrivance vector remains fenced; the V3 re-write did not
re-open any:

- **FNV/hex:** explicit NON-candidate in P2-A (CP-NONE `:380-385`), P2-B (G5/§4 `:262-264`), P2-C
  (`§3 item 4` + `:16`), P2-D (§4 `:469-473`), P2-E (G5 `:220-229`), P2-F (CF-0 `:337-340`). Verified
  §0 that `fnv64`/`push_ascii_lower_hex` write into the `emit_fact_stream` String, so "retires
  wholesale with the String" is structurally true. No artefact proposes a NEON hex/FNV kernel. ACCEPT.
- **Broadcast (the 24-row one-timing-tuple dishonesty):** P2-A §4 (`:446-449`) binds every CSS row to
  a per-corpus N≥50 median and distinguishes the SIMD `vdupq_n_u8` lane-splat from the
  evidence-measurement broadcast pre-block; P2-C §2-C2 (`:191-194`) makes the SAME splat-vs-broadcast
  disambiguation explicitly ("wholly unrelated to the §0.4 broadcast pre-block, which forbids …
  projecting one CSS timing tuple across 24 rows"); P2-D §4 (`:480-484`) binds cold per-parse N≥50 with
  counters compiled only under `bench-counters`. ACCEPT — the splat-vs-broadcast distinction is an
  anti-contrivance refinement.
- **Per-corpus capacity literal (the fixture contrivance):** P2-E §4 (`:307-309`) forbids it — "No
  per-corpus capacity literal (the delimiter alphabet is grammar-derived, the index capacity is
  `input.len()/8+8` as JSON, not a tailwind literal)." P2-D D4 sizes capacity from the SCAN OUTPUT
  (`scan_structurals(src).positions().len()+8`, `json/scan.rs:53`), not a hardcoded constant. ACCEPT.
- **Re-opened REDRESS (28+33, 50-55, 60-72, 80, 82-84, 88, 89):** each artefact's §4 names the
  pre-block + re-open test; P2-C §3 lists each blocked instruction route with its measured refutation
  (PMULL -10/-12/-15% `REDRESS.md:2510`, CTZ bulk consumer `:2544`, tiny-string). CH3 owns the full
  regression sweep; from the CH7 contrivance angle no candidate dresses a blocked route as a fast path.
  ACCEPT.

## §4 — CH7-4: CSS typed variants derived from grammar projections, not hand-coded (P4)

**ACCEPT (CF-1 / D1 / D2 / CP-A3), with the W5C retirement as the binding shortlist condition —
correctly attached as a self-fenced REVISE-trigger inside the artefacts, NOT an open CH7 defect:**

- The projection generator walks `BackendRule` (verified: `ValueRef<G: EventGrammar>` is the generic
  vehicle; JSON's `value_from_ref` `json/value.rs:143` is one instantiation, the CSS rider the
  isomorphic one). D2 §2, CF-1, CP-A3 all require "the view emitter walks ONE `BackendRule` shape for
  both JSON and CSS (no CSS-keyed branch JSON lacks)." Genuine derivation, not hand-coding.
- The single contrivance re-entry seam is correctly identified and fenced: `W5C_REQUEST_FACT_PROFILES`
  (verified hand-coded const, `codegen/src/lib.rs:336`). P2-A §4 (`:426-428`), P2-D §4 (`:460-463`),
  P2-F §1.4 (`:104-116`) + §4-1 (`:379-383`) all make CF-1/D1/D2's grammar-neutral verdict CONDITIONAL
  on this array being RETIRED — and explicitly forbid the trap of "relocating its per-rule branching
  into projection DATA" (the disguised-overfit re-entry, also caught by D5's guard `p2d:404-412`).
  Every residual CSS routing entry must name its `.bbnf` rule. This is the exact CH7-4 discipline,
  stated by the agents.
- The witnessed-grammar bound is honest: Sheets/BBNF-self generality is "asserted-by-construction,
  proof deferred to SK-V18" (P2-F §1.5/§3, D2 §3 `:430`, CF-2/G3 §3), with `sheets_witness` (24-LOC,
  no `BackendRule`) correctly disqualified as a projection rider (`codegen/src/lib.rs:1075-1090`
  negative controls). No artefact over-claims four-grammar generality. ACCEPT.

**Condition (carried, not a new defect):** the grammar-neutral verdict for CF-1/D1/D2 is valid ONLY if
the W5C retirement lands deriving routing from the grammar shape. This is correctly attached as the
REVISE-trigger in P2-F CF-1 (`:170-174`) and D5's guard, and as the SYNTHESIS Layout close gate. No
orphan REVISE. ACCEPT.

## §5 — New-this-cycle scan for fresh contrivance (V3 re-write introduced no regression)

V3 re-wrote five artefacts. I checked the re-writes for a NEW contrivance the re-run might have
introduced:

- **P2-B C-B1 layer-split (V3 §1.2 `:131-157`) is verified-honest, not over-claimed.** The disclosure
  "'JSON-wired' is true of the SHARED classifier interface (`select_classifier`); it is FALSE of the
  eq-set NEON leaf, which is exercised today only by the differential harness" is confirmed at source
  (§0: `SelectedBackend` has no eq-set arm; zero eq-set hits in `json/grammars/`). This is the
  strongest possible anti-contrivance posture — the artefact pre-empts the unearned-witness reading.
  No fresh contrivance.
- **P2-C/D/F refusal to manufacture a speculative-rollback antecedent (CF-3 / D3) is anti-contrivance,
  not contrivance.** The V3 re-write of P2-F CF-3 (`:236-255`) and P2-D D3 (`:305-322`) explicitly
  states the LOCKED profile measured ZERO speculative checkpoint/rollback self-time — only a 28.87%
  recognition control loop + 2.45% block dispatch, classed `structural` NOT rollback — and that the
  ~31% rollback figure alphaE C3 carried is a core-tree number NOT re-confirmed on the benched path.
  Both refuse to treat the recognition-control figure as a measured rollback antecedent and gate the
  candidate behind a hard post-CF-1 re-profile. From the CH7 angle this is the agents declining to
  contrive an antecedent for a seductive codegen lever. (CH1 owns whether the gating is sufficient;
  CH7 confirms there is no manufactured antecedent.) No fresh contrivance.
- **P2-E §1.3 comment/bracket suppressor (the new V3 analysis) is grammar-neutral mask algebra, not a
  CSS pin.** The §2-G1 sketch tests digraph parameters; §2-G2 takes open/close masks; both are
  alphabet-driven. The "two more suppressors than JSON" framing is additive and witnessed (JSON has
  none of comment, has string; CSS adds comment + bracket-depth). No `/`/`*` literal, no PMULL
  (REDRESS-88 clean), CTZ only as a parity-gated consumer (REDRESS-89 clean, reconciled with P2-C §3.2
  `p2e:174,283`). No fresh contrivance.

The V3 re-write is clean: it strengthened the C-B1 layer-split, made the speculative-rollback
antecedent honest, and added a grammar-neutral suppressor analysis — no new over-claim or special-case.

## §6 — Dispositions (counts + list)

Sections/candidates dispositioned by CH7 this cycle: **24** (4 cross-cutting probes + 20 candidate rows
across the six artefacts; orphan rows C5/C6/CF-4a/CF-4b/G4 counted once each as a disposed family
member), identical census to V1/V2 for comparability.

| # | Target (path) | V2 | V3 | Note |
|---|---|---|---|---|
| 1 | P2-A §1.5/§1.6 lightningcss/cssparser split | ACCEPT | ACCEPT | fair materializing bar; flaw-probe demoted |
| 2 | P2-A CP-A1 (`p2a:228-284,401,429-434`) | ACCEPT | ACCEPT | V2 fold intact (p2a unchanged); CSS = eq-set fan, lo6 JSON-only |
| 3 | P2-A CP-A2 tape-append | ACCEPT | ACCEPT | grammar-free offset sink |
| 4 | P2-A CP-A3 lazy `ValueRef` | ACCEPT | ACCEPT | grammar-parametric (type-witnessed §0); W5C condition carried |
| 5 | P2-A CP-A4 tokenize-once | ACCEPT | ACCEPT | consumption over neutral index |
| 6 | P2-A CP-NONE/CP-BLOCKED (FNV/digit/asmjson) | ACCEPT | ACCEPT | correctly retired/host-blocked |
| 7 | P2-B C-B1 eq-set scan (`p2b:131-157`) | ACCEPT | ACCEPT | V3 layer-split verified-honest at source (§0/§5); not over-claimed |
| 8 | P2-B C-B2 tape-append | ACCEPT | ACCEPT | fact-parity differential framing honest |
| 9 | P2-B C-B3 udot orphan | ACCEPT | ACCEPT | PROCESS-REJECTED at G1/G2/G4, not contrived in |
| 10 | P2-B C-B0 admission process G1–G6 | ACCEPT | ACCEPT | G5 strict comparator = lightningcss bar (V3 formal gate) |
| 11 | P2-C C1 lo6 TBL | ACCEPT | ACCEPT | INADMISSIBLE for CSS (mask-not-modulo cited :155-159) |
| 12 | P2-C C2 eq-set fan | ACCEPT | ACCEPT | admissible primary CSS route; genuine NEON (§0); splat≠broadcast :191-194 |
| 13 | P2-C C3/C4 movemask/CTZ fold | ACCEPT | ACCEPT | sub-tasks, no orphan |
| 14 | P2-C C5/C6 udot/i8mm | ACCEPT | ACCEPT | orphan-gated, no contrivance |
| 15 | P2-C §3 REDRESS-block flags | ACCEPT | ACCEPT | each refutation measured (PMULL/CTZ/x86/SVE) |
| 16 | P2-D D1 `push_plain_offset` | ACCEPT | ACCEPT | grammar-free |
| 17 | P2-D D2 lazy `ValueRef` | ACCEPT | ACCEPT | W5C condition carried |
| 18 | P2-D D3/D4 checkpoint/one-shot reserve | ACCEPT | ACCEPT | V3 D3 honest (no manufactured rollback antecedent); D4 capacity from scan output, not literal |
| 19 | P2-D D5 sparse-flag | ACCEPT | ACCEPT | guard (BackendRule branch-tag, not per-rule catalogue) correct |
| 20 | P2-D D6 second-substrate | ACCEPT | ACCEPT | REJECT-on-sight anchor, correct |
| 21 | P2-E G1 `comment_body_mask_64` (`p2e:104,120-129,235`) | ACCEPT | ACCEPT | V2 digraph-fold preserved; §2 sketch tests open/close params, no `/*` literal |
| 22 | P2-E G2 `bracket_depth_mask_64` | ACCEPT | ACCEPT | mask-input, genuinely neutral; CTZ consumer-only (REDRESS-89 clean) |
| 23 | P2-E G3/G4/G5 + §1.3 suppressor analysis | ACCEPT | ACCEPT | index assembler neutral; G4 orphan-gated; G5 non-candidate; §1.3 mask algebra not CSS-pin |
| 24 | P2-F CF-1..CF-4b + §1.2 lo6 split + §1.4 W5C seam | ACCEPT | ACCEPT | neutrality split + overfit-seam fencing is the CH7 spine, correctly drawn |

**Counts:** ACCEPT 24, REVISE 0, REJECT 0.
ACCEPT rate = 24/24 = **100%** (above the 95% convergence floor).

This is the SECOND consecutive 100% CH7 cycle on S-P2 (V2 = 100%, V3 = 100%). All four CH7 probes pass:
(P1) lightningcss = fair materializing bar, cssparser = demoted flaw-probe — and the V3 P2-B re-write
made it a formal admission gate (G5); (P2) the candidate set is genuinely grammar-general — the
per-grammar datum is always {alphabet, digraph, open/close set, node-kind enum, `BackendRule`}, derived
from the `.bbnf`; (P3) no fixture/FNV/broadcast/per-corpus-literal re-entry, every vector fenced and
re-verified; (P4) CSS typed variants derive from `BackendRule` projection with the W5C retirement as
the self-fenced shortlist condition. The V3 re-write introduced no fresh contrivance (§5).

## §7 — V4 fold directives (for the consolidator)

**None from CH7.** This lens returns zero REVISE / zero REJECT this cycle — the second consecutive
100% CH7 cycle on S-P2. The only carried item is the W5C-retirement shortlist gate on CF-1/D1/D2,
which is already named in P2-A/D/F and is a self-fenced condition (a downstream redress obligation),
not an orphan REVISE. From the CH7 OVERFIT-PRUNE angle there is no obstacle to advancing S-P3.
