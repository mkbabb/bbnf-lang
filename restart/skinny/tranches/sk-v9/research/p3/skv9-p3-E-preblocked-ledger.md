# SK-V9 P3-E: Pre-Blocked-Route Ledger

Pass: S-P3 Synthesis-Plan. Cycle: V1.
Date: 2026-05-18.
Scope: the binding pre-block ledger for the SK-V9 wave plan — per-wave
REDRESS entries each wave must NOT reopen, the material differentials that
make REDRESS-adjacent waves admissible, the hard pre-blocks no wave may
reopen, the W10b six-row maintain block, and the SUPERSEDED entries.
Output: this file.
Pass Alpha goalset: SK-V9 §0 — lift the typed-GO count (4 → 6), close the
four uncloseable string-bound rows where measurement admits, repair the
substrate-bound parse-plane loss; every wave falsifiability-gated, no
pre-blocked route reopened.
Candidate pool: research/p2/ post-CHALLENGE survivors (P2-A union
event-model, P2-B retained grammar proof, P2-C Apache/CITM admission,
P2-D aarch64 ASM, P2-E unicode codec, P2-F SOTA teardown).

---

## §1 — Method

`skinny/REDRESS.md` carries 93 numbered entries plus a three-fact preamble.
P1-V3-F reconciled all 93 against the `SK-V9-open` baseline: ~60
STILL-LOAD-BEARING, 7 SUPERSEDED (35, 36, 37, 38, 46, 49, 70), ~14
HISTORICAL. This ledger is the wave-facing projection of that
reconciliation: it answers, per SK-V9 wave, *which rejected routes the
wave must not reopen* and *what differential makes a REDRESS-adjacent
intervention admissible anyway*.

A REDRESS entry is a **pre-block** for a wave when the wave touches the
same owner surface, the same kernel, or the same architectural axis that
the entry rejected. A pre-block is **honoured** when the wave's owner
paths, consumer, and gate are disjoint from the rejected shape. A
pre-block is **reopened-with-differential** when the wave touches a
REDRESS-adjacent route but states a *material differential* — a concrete,
falsifiable structural distinction from the rejected shape — and binds it
to a same-row falsification gate per SK-V9 SPEC §1.

Three disposition classes, mirroring P1-V3-F:

- **HARD PRE-BLOCK** — no SK-V9 wave reopens it under any framing. Reopening
  requires a future S-P{N} plan with fresh measured evidence, exact owner
  paths, a same-wave consumer, a no-regression gate, the REDRESS citation,
  and CHALLENGE acceptance (HANDOFF §5 closing clause).
- **ADMISSIBLE-WITH-DIFFERENTIAL** — the wave touches a REDRESS-adjacent
  route; §3 records the differential and the binding gate. The differential
  is itself falsifiable: if the gate fires, the route reverts and a new
  REDRESS entry records the falsification (SK-V9 SPEC §1, no paper-close).
- **SUPERSEDED** — the entry no longer binds; a later monotonic admit or
  reject owns the live constraint (§6).

The SK-V9 wave set this ledger covers, drawn from the S-P2 candidate pool
and the HANDOFF §3 candidate boundaries:

| Wave | Topic | REDRESS-adjacency |
|---|---|---|
| W0 | baseline-profile / telemetry-lock | telemetry-hygiene only; no behavior route |
| W-AC | Apache/CITM typed row-table admission (P2-C) | REDRESS-adjacent (91) |
| W-RG | retained class/event grammar + `ValueRef` proof (P2-B) | REDRESS-adjacent (92, 50–72) |
| W-UE | union event-model (P2-A) | REDRESS-adjacent (92, 50–53, 60–72) |
| W-UC | unicode codec + string-block widening (P2-D / P2-E) | REDRESS-adjacent (82, 83, 64) |
| W-AS | aarch64 ASM (SHA3 EOR3, CSSC CTZ) (P2-D) | REDRESS-adjacent (88, 89, 33) |

Wave letters are P3-E shorthand; P3-B owns the canonical W0…W{n}
numbering. Topological order per S-P2 CONVERGED: W-RG proof → W-UE union →
W-UC / W-AS consumers; W-AC is fully independent.

---

## §2 — Per-Wave Pre-Block List

For each wave: the REDRESS entries it must NOT reopen (honour as written),
and — separately — the REDRESS-adjacent entries it *touches* and must clear
via the §3 differential.

### §2.1 — W0 baseline-profile / telemetry-lock

W0 is telemetry-hygiene only. It produces no behavior route.

**Must NOT reopen (honour as written):** REDRESS 1, 8, 9, 10, 26, 32, 47,
75, 77, 78 (telemetry-lock invariants — Mbps units, no payload-counter
hardcode, diagnostic non-producers, comparator hygiene, schema-v3
provenance). W0 must also honour the SK-V9 SPEC §1 non-negotiable that
PMU, cycles-per-byte, masking probes, structural-scan-only paths, and
Criterion-slope artifacts are **diagnostic non-producers** — V3's real-PMU
c/B characterises hot leaves but admits no behavior route (P1-V3-F §3.2,
§3.4).

**REDRESS-adjacent:** none. W0 touches no behavior route.

### §2.2 — W-AC Apache/CITM typed row-table admission (P2-C)

**Must NOT reopen (honour as written):**

- REDRESS 92 — W-AC adds **no structural surface**; it is row-table
  admission only (P2-C §6). No retained class/event grammar, no `ValueRef`
  cursor, no scanner/tape change.
- REDRESS 93 — W-AC touches no direct guard plane; the two existing direct
  rows for Apache (`N-direct/NO-GO`) and CITM (`A/GO`) hold their
  `SK-V9-open` verdicts unchanged (P2-C §4.3 gate, P2-C §6).
- REDRESS 60–72 — W-AC extends only the existing typed schema/generator
  path (`real_typed_schema.rs`, `real_typed_struct.rs`,
  `generated_real_typed.rs`); it adds no retained-parse sidecar, no
  source-hook field-layout materializer, no parser-owned scratch, no
  byte-output `unescape_*`, no DirectBuild semantic string fact, and does
  not extend cap-16 beyond REDRESS 72's admitted scope (P2-C §6).
- REDRESS 80 — `canada/real_typed_struct` mantissa-widen stays rejected;
  W-AC does not touch the f64 path.
- REDRESS 85, 86, 87 + Lock 14 — no JSON policy enters a generic crate;
  W-AC touches only bench/xtask typed surfaces, not `bbnf-simd`,
  `parse-that-regex`, `ir`, or `codegen` generic shells.

**REDRESS-adjacent (clear via §3.1):** REDRESS 91.

### §2.3 — W-RG retained class/event grammar + `ValueRef` proof (P2-B)

**Must NOT reopen (honour as written):**

- REDRESS 50, 51, 53 — no parser-written aux side table, no parser-local
  byte-class cursor, no parser-local structural-mask cursor. W-RG is a
  `cfg(feature = "proof")` `EventGrammar` trait + `ValueRef<G>` witness;
  it edits **no production parser path** (P2-B §4.2: the unifying defect
  of REDRESS 60–69 was that every candidate edited the generated/runtime
  parser body — W-RG does not).
- REDRESS 60–72 — W-RG does not reopen the retained-parse candidate ledger.
  It has no production consumer and no `RESULTS.md` row movement; it is
  proof-only depth (P2-B §1, S-P2 CONVERGED item 2). REDRESS 71 is the
  *admitted* host/API typed-DirectBuild route and is orthogonal — the proof
  sits on the `OffsetTape` retained lowerer, REDRESS 71 on the `SinkOnly`
  direct lowerer (P2-B R5).
- REDRESS 73 — no helper-shape transfer from generated retained parsing to
  hand Track 2 or control-path work.
- REDRESS 91, 93 — W-RG admits no Apache/CITM measured row and no direct
  contract; those are REDRESS 91/93 residuals owned by other waves (P2-B
  §"removes exactly one pre-block").

**REDRESS-adjacent (clear via §3.2):** REDRESS 92.

### §2.4 — W-UE union event-model (P2-A)

**Must NOT reopen (honour as written):**

- REDRESS 50 — the class column is **co-emitted at the existing
  `emit_plain_offset` call site** (`push_plain_offset` → `push_offset_with
  _class`); it is not a parser-written aux side table. Falsifier: if any
  pass other than the parser writes `classes`, REDRESS 50 is reopened
  (P2-A §6).
- REDRESS 51 — no parser-local byte-class cursor. The parser walks the
  SIMD-produced structural index; `ParserState` gains no cursor field
  beyond `state.cursor: usize` + the structural-index walker idx.
  Falsifier: a new `ParserState` cursor field (P2-A §6).
- REDRESS 53 — no parser-local structural-mask cursor / second scanner.
  The model consumes *the* existing `scan_structurals` product by move;
  no second mask is built inside the parser. Falsifier: a second
  `compact_mask`-class call site inside the parser (P2-A §6).
- REDRESS 60–72 — no retained-parse sidecar producer. The class column is
  the tape's own column written by the *primary and only* producer (the
  parser); no new producer pass. Falsifier: any wave-level pass other than
  the parser writing into `tape.classes` (P2-A §6).
- REDRESS 83 — orthogonal: W-UE changes no string-scanner pair; it removes
  the per-cursor source-byte rediscovery in `JsonNodeKind::at_cursor`, a
  removal of work, not a new route into string-boundary closure (P2-A §6).
- REDRESS 84 — orthogonal: no object-pair value-byte compaction; the class
  column is one byte per cursor without packing (P2-A §6).
- REDRESS 88, 89 — orthogonal: the SIMD producer is unchanged at the
  Layer-1 vocabulary level (P2-A §6).
- The REDRESS 92 blanket pre-block (`REDRESS.md:2673-2676`): W-UE
  introduces **no new `BackendShape` variant** (the class column is a
  representation refinement of the `OffsetTape` runtime struct only;
  `derive_backend_shape` gains no branch), **no new BIR variant**, **no new
  directive**, **no public substrate API** (`runtime/src/tape/` exposes
  only `class_at`/`push_offset_with_class` over an opaque `Vec<u8>`), **no
  parser-owned structural cursor/facts** (the `StructuralIndex` is consumed
  by move, lifetime ≤ one `parse(input)`, never named on a retained
  struct), **no `tape_vs_tape` as production consumer**, **no `UnionTape`
  public type**, **no Tier B string-boundary/quote-backslash/parity work**
  (P2-A §6).

**REDRESS-adjacent (clear via §3.2):** REDRESS 92. W-UE *implements the
routed precursor* REDRESS 92 named; it does not reopen the rejection. W-UE
is gated behind the W-RG proof per S-P2 CONVERGED dependency order.

### §2.5 — W-UC unicode codec + string-block widening (P2-D / P2-E)

**Must NOT reopen (honour as written):**

- REDRESS 64 — no retained Unicode-escape run validator. W-UC's
  `escape_codec_hex_unit` primitive operates on the unescape *materialiser*
  hot path, not a retained run validator (P2-E §5).
- REDRESS 66, 67, 68, 69 — no direct source-hook field-layout materializer,
  no parser-owned decoded scratch, no byte-output `unescape_*` rewrite, no
  DirectBuild semantic string fact. W-UC's codec gate is `parse_only` only;
  the direct plane lives behind REDRESS 66–69 + 93 and W-UC does not enter
  it (P2-E §5 falsifiability row, P2-E §"V3-D §6.4").
- REDRESS 65, 84 — no object-pair value-byte / next-key carry.
- REDRESS 60, 61, 62 — no retained trusted-string boundary collapse, no
  always-wide / delayed-wide retained trusted scan. W-UC's 32-byte block
  is a `match_string_at_quote_trusted_utf8` producer-site widening, not a
  retained-scan boundary collapse.

**REDRESS-adjacent (clear via §3.3, §3.4):** REDRESS 82 (`escape_codec_hex
_unit` broadening / `unescape_uxxxx_x4_neon` per-quartet route), REDRESS 83
(32-byte string-block widening).

### §2.6 — W-AS aarch64 ASM (SHA3 EOR3, CSSC CTZ) (P2-D)

**Must NOT reopen (honour as written):**

- REDRESS 88 — PMULL prefix-XOR as the default hot body of
  `bitmap_prefix_xor_64` stays rejected. W-AS keeps the scalar shift-XOR
  ladder as the unconditional production default; the SHA3 EOR3 path is a
  capability-conditional specialisation, not a new default (P2-D §5.3).
- REDRESS 89 — the CSSC CTZ body for `bitmap_next_set_bit` /
  `bulk_emit_positions_64_neon` (the structural-scan bulk consumer) stays
  rejected. W-AS does not rewire that call site (P2-D §4.4).
- REDRESS 33 (+ 28) — NEON `match_tiny_plain_string` as a retained parse-G
  fix stays permanently rejected; W-AS does not wire Class-A NEON
  `match_tiny_plain_string` into a field-name match-arm chain (P2-F §7.3,
  the REDRESS 33 rejected shape).
- REDRESS 90 — the B6 stack-canary Stage 1 is checkasm hardening, not a
  behavior producer; W-AS adds no checkasm-canary behavior claim.

**REDRESS-adjacent (clear via §3.3, §3.5):** REDRESS 88 (SHA3 EOR3
prefix-XOR), REDRESS 89 (CSSC CTZ string-mask consumer).

---

## §3 — Material Differentials (per REDRESS-adjacent wave)

Every REDRESS-adjacent route below is admissible *only* with the stated
material differential bound to a same-row falsification gate. The
differential is itself falsifiable: if the gate fires, the route reverts
and a new REDRESS entry records the falsification (SK-V9 SPEC §1).

### §3.1 — W-AC vs REDRESS 91 (Apache/CITM measured-row admission)

**REDRESS 91 rejected:** measured `real_typed_struct A / GO` row-table
admission for Apache/CITM. The W2 admit (commit `12aff1e4`) admitted
*source/product parity only* — verbatim: *"W2 therefore admits
source/product parity only and does not claim six measured `real_typed
_struct A / GO` rows."* The cause was not the typed shape; it was that the
W0 run-id validator tripped on metadata drift unrelated to W2 source, and
W2 routed the row-table admission out rather than weakening the gate.

**Material differential (P2-C §1, §3, §4):** REDRESS 91's gap is a
*whitelist*, not an architecture. The `SK_V8_OPEN_BASELINE` whitelist was
never expanded with W2's admission, so the gate's typed-metadata
requirement still derives from the W0 measured baseline (which omits
Apache/CITM). The W-AC differential is: own a **fresh run-id / metadata
validation** and produce **measured rows** under that fresh run-id, then
expand the whitelist. This is the mechanical step REDRESS 91 explicitly
deferred — *"Apache/CITM remain source/product parity rows until a later
accepted benchmark row-table wave."* W-AC *is* that later wave. It admits
**Apache + CITM only**; `canada/real_typed_struct` stays rejected (REDRESS
91 long-decimal DirectBuild-vs-serde checksum mismatch is binding).

**Binding gate (P2-C §4.3):** the fresh run-id must validate; the four
existing typed GO rows (twitter, update_center, mesh, marine_ik) must hold
`A / GO` with no regression below `sonic-rs strict × 1.10⁻¹`; the two
direct rows must hold their `SK-V9-open` verdicts. Miss any gate → halt at
redress, record the falsified gate, replace the REDRESS-entry "promotion"
framing with a "falsification report" framing, revert.

### §3.2 — W-RG / W-UE vs REDRESS 92 (W3 union rejection) + 50–72 (sidecar routes)

**REDRESS 92 rejected:** the SK-V8 W3 Tier A tape + structural-projection
implementation *before source redress*. The fit gate failed because the
scanner/tape event model was not isomorphic — SC-3's structural alphabet
handled 3 of 7 JSON event classes, the 4 scalar anchors had no derivation,
and `JsonNodeKind::at_cursor` did a hidden per-cursor source-byte
rediscovery. REDRESS 92 routed the precursor: *"define the retained
class/event grammar including numbers/literals and string quote ownership,
prove the retained `ValueRef` cursor contract over that grammar, and only
then reopen a measured structural-heavy parse row wave."* REDRESS 50–72
are the recurrent sidecar / aux-side-table / retained-parse-candidate
class pre-blocks REDRESS 92 sits atop.

**Material differential — W-RG (P2-B §4):** REDRESS 60–69's unifying
defect was that *every candidate edited the production parser path* —
generated parser body, runtime mirror, or codegen template — and carried
no measurement surface. W-RG edits **no production parser path**: it is a
`cfg(feature = "proof")` `EventGrammar` trait + `ValueRef<G>` cursor +
JSON & Sheets witnesses. Five-axis differential: (1) no production
consumer, (2) no row-movement surface — it is the missing *proof* REDRESS
92 demanded, not a *candidate*, (3) it touches none of the parser-control
files REDRESS 60–72 touched, (4) it adds no payload field, (5) the cursor
is unchanged — it *proves* the existing `ValueRef` cursor contract rather
than modifying control flow. W-RG is **proof-only depth**; admitting it
removes exactly one pre-block (the W3 structural reopen) and binds nothing
else.

**Material differential — W-UE (P2-A §1, §6):** the SK-V8 W3 fit gate
conflated *cursor* and *class*. The union event-model splits them: it
keeps the parser-event cursor stream (the existing retained tape) and adds
a **co-indexed class column written at the same `emit_plain_offset` call
site** — the SIMD structural index becomes a *transient producer consumed
by move* (Lock 1 cardinality stays at one: one substrate, one producer).
This is materially distinct from REDRESS 50–53's parser-written aux tables
and parser-local cursors (the class column is not a side table — it is the
tape's own column, written by the primary producer) and from REDRESS
60–72's sidecar producers (no second producer pass). W-UE does not reopen
REDRESS 92 — it **implements the routed precursor** REDRESS 92 named, and
is gated behind W-RG's accepted proof per the S-P2 dependency order.

**Binding gate:** W-UE's structural falsifiers (P2-A §6) — class column
written only by the parser; no new `ParserState` cursor; no second
`compact_mask` call site; no new `BackendShape`/BIR/directive/public
substrate API/`UnionTape`. Any falsifier tripping reopens the cited
REDRESS class. P3-C owns the named-corpus + Mbps thresholds.

### §3.3 — W-AS vs REDRESS 88 (SHA3 EOR3 prefix-XOR vs PMULL rejection)

**REDRESS 88 rejected:** the aarch64 PMULL (`vmull_p64`/`vmull_high_p64`)
implementation of `bitmap_prefix_xor_64` as the default hot body — a
64-bit carryless multiply computing the prefix-XOR as a polynomial
product. Escape-heavy and narrow parse-only JSON rows regressed 12–15% on
the production benchmark even though the SIMD-scan microbench was stable;
the failure mode was PMULL.1Q retire latency (4-cycle) on the M5 Max
P-core. REDRESS 88's verdict: *the scalar prefix-XOR is the production
default.*

**Material differential (P2-D §5.3):** SHA3 `veor3q_u8` is **not a PMULL
wrapper, re-admit, or substitution** — it accelerates the scalar shift-XOR
ladder REDRESS 88 *kept*. Three orthogonal axes: (a) **different
intrinsic** — EOR3 is a 3-input bitwise XOR on a 128-bit vector, no
multiply, no polynomial-field arithmetic; (b) **different latency
profile** — PMULL.1Q is 4-cycle latency, EOR3 is 1-cycle latency, so the
REDRESS-88 retire-latency failure mode is structurally inapplicable (no
PMULL op exists in the EOR3 chain); (c) **different primitive shape** —
the EOR3 proposal is a *vector shift-XOR ladder* over `uint8x16_t`, an
algebraic fold of the existing scalar ladder (6-stage shift-XOR ladder →
3 EOR3 ops, ~12 µops → ~6 µops), whereas PMULL *replaced* the ladder
entirely. The absolute cycle counts are a host-capability-gated estimate
(M5 Max P-core specifics unpublished by Apple); the **monotonic ordering
EOR3 < PMULL** is the load-bearing claim.

**Lock 16 admissibility caveat:** `FEAT_SHA3` is a host capability. The
EOR3 body is admitted *only* when the host-cap survey reports
`FEAT_SHA3=1`; the scalar shift-XOR ladder remains the **unconditional
fallback**. The EOR3 variant is a capability-conditional specialisation
under the Lock 16 grammar-neutral admissibility predicate — the same shape
as `digit_mac` (DotProd-gated) and the AES gadget — *not* a new default
body. This honours REDRESS 88: PMULL stays rejected, the scalar ladder
stays the production default.

**Binding gate (P2-D §5.3):** a vector-vs-scalar-vs-PMULL three-way
checkasm differential; an explicit no-regression maintain gate on the
W10b six-row WIN block (§5). MEDIUM risk: the vector-ladder representation
differs from the u64-word scalar representation, so the parity oracle
must cover all three. Miss → revert to scalar-only.

### §3.4 — W-UC vs REDRESS 82 (`unescape_uxxxx` broadening / single-quartet classifier)

**REDRESS 82 rejected:** the SK-V7 W4 single-quartet Unicode-escape
classifier. The W4 candidate moved the scalar `\uXXXX` decoder into
`parse-that-regex/src/unicode/escape_decode.rs` and reused
`unescape_uxxxx_neon` for **one quartet at a time** as a *parser-owned
per-quartet helper*, consumed in `decode_json_unicode_escape` and the
`unescape_json_string` materialiser. Correctness was green; the gate
failed — `unicode_escapes/parse_only` at 82.1% of sonic,
`y_string_unicode/parse_only` at 49.9%, direct rows far below threshold,
`y_string_unicode` Track 2 regressed 6.6%. Failure mode: per-`\u`
dispatch overhead with no batching.

**Material differential (P2-D §3.5, P2-E §5):** `unescape_uxxxx_x4_neon`
is **already wired** at `parse-that-regex/src/lib.rs:402` as the 4-quartet
batcher — the SK-V9 route is *not* "wire it." The differential is on five
orthogonal axes: (1) **not a parser-owned per-quartet classifier** — the
4-quartet batched primary write path is the union-substrate path; the
single-quartet binding fires *only* when the 4-quartet pre-filter rejects
(P2-E §5 "hot path entered"); (2) **same-wave consumer is real** — the
already-wired x4 JSON path at `lib.rs:402`, plus CSS L4 + TOML scaffolds
for Lock-14 grammar-neutrality, vs REDRESS 82's single JSON-materialiser
consumer; (3) **`escape_codec_hex_unit` is a const-generic primitive** (5
bindings: JSON-4, CSS L4 variable, JS variable, TOML-4, TOML-8) — a
grammar-neutral primitive, not a single parser-owned classifier; (4)
**material new evidence** — P1-V3-B/C xctrace Time Profiler self-time
(38.2% / 43.9% on `y_string_unicode`) is post-V3, vs REDRESS 82 operating
on the SK-V6 profile; (5) **falsification gate is `parse_only` only**, set
against the four uncloseable rows P1-V3-D §5.3 named, with the direct
plane explicitly left behind REDRESS 66–69 + 93.

**Honest verdict — same-wave conditional admission (P2-E):** the codec
*alone* closes zero of the four uncloseable rows — `unicode_escapes`
NEAR-FAIL 94.5%, `y_string_unicode` 94.8%, `unicode_mixed` FAIL 63.7%,
`gsoc-2018` no-regression-basis. Admission is the **§6.4 same-wave
conditional rule**: the codec ships paired with the string-scanner
widening (§3.5 below) in W-UC, never alone. A codec-only orphan is a
REDRESS-82-style orphan and is itself pre-blocked.

### §3.5 — W-UC vs REDRESS 83 (32-byte string-block widening vs StringBlock16 tiny probe)

**REDRESS 83 rejected:** the SK-V7 W5 generated-retained StringBlock16
tiny probe. The candidate added a JSON-specific 16-byte wrapper over
`scan_string_special_block` and wired it **only** into the generated
retained `match_tiny_plain_string_with_cap::<16>` helper (the tiny path).
Zero of six named parse rows crossed threshold; six regressed >3%. Failure
mode: the AArch64 `string_block` movemask shape is too expensive for the
already-tiny generated retained quote-pair probe.

**Material differential (P2-D §4.3):** (1) **different target call site**
— REDRESS 83 wired the *tiny* 16-byte-cap path; the SK-V9 widening targets
`match_string_at_quote_trusted_utf8` (`parse-that-regex/src/lib.rs:162`),
the *full* path called on `unicode_mixed/escapes/gsoc-2018` once the
16-byte cap is exceeded — the TP table shows it at 15–20% on the four LOSS
rows; (2) **successor, not wrapper** — a 32-byte primitive replacing the
16-byte primitive at the producer site (or a `scan_string_special_block
_32` variant), not a JSON-specific wrapper layered on top; (3) **same-wave
consumer is the existing `match_string_at_quote_trusted_utf8`** — no
sidecar primitive, no parallel substrate (Lock 1 compliant).

**Binding gate (P2-D §4.3):** the widening's binding risk is the
µop-neutral-per-byte finding (§4.2) — the win is consumer-side
mask-handling halving, not producer-side throughput — so the falsification
gate must measure the *combined producer + consumer path*, not the
block-scan microbench. MEDIUM risk. Miss → revert.

### §3.6 — W-AS vs REDRESS 89 (CSSC CTZ string-mask consumer vs CTZ bulk consumer)

**REDRESS 89 rejected:** the SK-V7 W10b CSSC CTZ body for
`bitmap_next_set_bit`, consumed from `bulk_emit_positions_64_neon` (the
structural-scan bulk consumer). Six WIN-block rows regressed 3–8%.

**Material differential (P2-D §4.4):** (1) **different call site** —
REDRESS 89's target was the structural-scan bulk-emit pipeline; the SK-V9
target is the string-block scanner consumer's per-mask first-set extract
(`<u16>::trailing_zeros`, 10.5% on `gsoc-2018/t1`); (2) **different
failure profile** — the W10b failure was a 2–8% drop on *currently-winning*
numeric-token-heavy rows; the SK-V9 proposal targets *LOSS* rows with the
winning rows held under the falsification gate; (3) **same-wave consumer
is the union-substrate string-mask consumer (P2-A scope)**, a separate
call site from the structural-scan bulk-emit pipeline with its own µop
budget.

**Binding gate (P2-D §4.4):** `cargo asm` proof that `ctz` emits under
`-C target-cpu=native`; an explicit no-regression maintain gate on the
W10b six-row WIN block (§5). **HIGH risk** — REDRESS 89 already rejected
the structurally adjacent body; the differential is plausible but
unproven. This slice **blocks on P2-A landing in the same wave** — absent
the union-substrate string-mask consumer, the CTZ extract is an orphan and
does not ship. Miss the WIN-block gate → revert.

---

## §4 — Hard Pre-Blocks (No Reopening)

The following routes are HARD PRE-BLOCKS: **no SK-V9 wave may reopen them
under any framing.** They are sourced from HANDOFF §5, SK-V9 SPEC §1, and
P1-V3-F §3. A future S-P{N} candidate touching any of them must cite the
REDRESS item, state the material differential, pre-register a same-row
falsification gate, and pass CHALLENGE before implementation planning
(HANDOFF §5 closing clause). None of these is admitted by any §3
differential.

1. **Apache/CITM measured-row overclaim** (REDRESS 91). Claiming six
   measured `real_typed_struct A / GO` rows without a fresh run-id /
   metadata validation and measured rows under it. W-AC's §3.1
   differential is the *only* admissible path, and only for Apache + CITM.

2. **`canada/real_typed_struct`** without full-fixture DirectBuild-vs-serde
   checksum proof (REDRESS 91 long-decimal mismatch; REDRESS 80
   mantissa-widen). Permanently route-out until the checksum proof exists.

3. **W3 structural implementation without the retained class/event grammar
   + retained `ValueRef` cursor proof** (REDRESS 92). No structural-heavy
   parse implementation reopens until W-RG's proof is *accepted*. W-UE
   cannot precede W-RG.

4. **W4 scalar-parent fold or renamed parent-digest fold** (REDRESS 93)
   without a V9-aware checked gate, a full-table maintain proof, and an
   independent Track 2 digest-arithmetic backstop. No SK-V9 wave carries a
   direct guard scalar-parent fold under any name.

5. **REDRESS 73 helper-shape transfer** from generated retained parsing to
   hand Track 2 or control-path work without direct hand-parser code-layout
   profiling. No wave transfers an array-next-byte / object-pair helper
   shape across the generated/hand boundary.

6. **Sidecar / parallel-substrate class** (REDRESS 50, 51, 53, 60–72, 92 +
   SPEC §1): sidecar substrate, parser-owned cursor/fact slots,
   `UnionTape` public type, a new `BackendShape` variant, a new
   directive/BIR variant, a public substrate API, and `tape_vs_tape` as a
   production consumer. The substrate union (Lock 1 cardinality = one)
   holds across every SK-V9 wave. W-UE's class column is admissible *only*
   because it is the tape's own column written by the primary producer —
   any structural falsifier in §3.2 tripping returns this to a hard block.

7. **PMULL prefix-XOR and CTZ/bulk production rewires as default hot
   paths** (REDRESS 88, 89). PMULL stays rejected as the
   `bitmap_prefix_xor_64` default. The CSSC CTZ `bulk_emit_positions_64`
   structural-scan consumer stays rejected. W-AS's SHA3 EOR3 (§3.3) is a
   Lock-16-gated capability-conditional specialisation with a scalar
   unconditional fallback — *not* a default rewire — and W-AS's CTZ (§3.6)
   targets the string-mask consumer, *not* the bulk-emit pipeline.

8. **Generic JSON policy leaks / Lock 14 weakening** (REDRESS 85, 86, 87).
   No JSON name, no JSON shape policy, no JSON allowlist enters a generic
   crate (`bbnf-simd`, `parse-that-regex`, `ir`, `codegen` generic shell).
   Every generic-crate edit carries a non-JSON proof (CSS L4 / Sheets /
   BBNF-self) per SK-V9 SPEC §2.1.

**Additional class umbrellas binding by reference** (P1-V3-F §3.2 — the
HANDOFF §5 list is correct but incomplete relative to the underlying
ledger; these umbrellas close the recurrent re-proposal door):

9. **String-scanner widening / boundary-collapse class** (REDRESS 60, 61,
   62, 64, 65, 82, 83, 84). Retained or direct string-scan widening,
   trusted boundary collapse, value-byte/next-key carry, and
   per-quartet/per-segment unicode-escape classifier routes — pre-blocked
   *without a same-row falsification gate pre-registered in this S-P3
   plan*. W-UC's §3.4 + §3.5 differentials are the only pre-registered
   admissions; everything else in the class stays hard-blocked.

10. **Direct receiver / scratch / semantic-fact class** (REDRESS 66, 67,
    68, 69; 49 superseded). Direct source-hook field folding, parser-owned
    decoded scratch, byte-output `unescape_*` rewrites, and DirectBuild
    semantic-string-fact streaming for the digest workload. No SK-V9 wave
    enters the direct plane; W-UC's codec gate is `parse_only` only.

11. **Bench-private hand Track 1 / hand typed sink class** (REDRESS 34,
    70). No bench-private hand Track 1 parser or hand typed sink presented
    as generated direct/typed proof.

12. **PMU / cycles / Criterion-slope / masking / structural-scan as
    producer** (SPEC §1). PMU, cycles-per-byte, masking probes,
    structural-scan-only paths, and Criterion-slope artifacts remain
    diagnostic non-producers under V3 PMU evidence too — V3 real-PMU c/B
    *characterises* hot leaves; it admits no Track 1 / Track 2 / typed /
    direct / strict producer (P1-V3-F §3.2, §3.4).

13. **NEON `match_tiny_plain_string` as a retained parse-G fix** (REDRESS
    28 + 33). Permanently invalidated as a Class-A retained-G fix; no wave
    wires Class-A NEON `match_tiny_plain_string` into a field-name
    match-arm chain (P2-F §7.3). The asmjson Class-A NEON scanner remains
    pre-blocked by REDRESS 28+33; P2-F §7.3 routes it to a future named
    wave under an explicit REDRESS 28+33 material-differential gate or
    formal retirement — it is *not* an SK-V9 wave.

---

## §5 — W10b Six-Row Maintain Block

REDRESS 89 (SK-V7 W10b) rejected the CSSC CTZ bulk consumer because **six
Track 1/Track 2 rows dropped more than 2% versus the saved pre-W10b
report**, with no verdict downgrades. These six rows are the **W10b WIN
block** — the currently-winning, numeric-token-heavy / structural-scan
rows that any aarch64 SIMD-touching SK-V9 wave must NOT regress:

| Corpus | Workload | W10b observed regression (the rejection) |
|---|---|---|
| `canada` | `parse_only` | Track 1 −3.11%, Track 2 −4.14% |
| `citm_catalog` | `parse_only` | Track 1 −7.36% |
| `instruments` | `parse_only` | Track 1 −3.96% |
| `marine_ik` | `parse_only` | Track 1 −5.68% |
| `mesh` | `parse_only` | Track 1 −8.07%, Track 2 −7.46% |
| `numbers` | `parse_only` | Track 1 −6.44% |

**Binding maintain block.** Every SK-V9 wave that touches an aarch64 SIMD
kernel — W-AS (SHA3 EOR3 §3.3, CSSC CTZ §3.6) and W-UC (32-byte
string-block widening §3.5) — carries an explicit **no-regression maintain
gate on all six rows**: each row's Track 1 and Track 2 Mbps must hold
within the per-wave guard band against the `SK-V9-open` baseline (P3-C
sets the exact band; P2-D §4.4 and §5.3 both name this six-row block as
the hard blocking precondition). The W10b precedent fixes the guard at
**no row dropping below the maintain threshold**; a single row regressing
past the band falsifies the wave, the kernel reverts to its scalar /
16-byte body, and a new REDRESS entry records the falsification. The CSSC
CTZ slice (§3.6) is **HIGH risk** precisely because it is structurally
adjacent to the route that produced this exact six-row regression — its
differential (different call site, LOSS-rows-under-guard) is plausible but
unproven, so the six-row maintain gate is its hard blocking precondition.

The maintain block also constrains W-UE: although the union event-model
keeps the SIMD producer unchanged at the Layer-1 vocabulary level (P2-A
§6), its exit gate must demonstrate these six rows hold — the structural
index becoming a consumed-by-move producer must not perturb the
structural-scan-heavy WIN rows.

---

## §6 — SUPERSEDED Entries

Per P1-V3-F §2.13, seven REDRESS entries are SUPERSEDED — a later monotonic
admit or reject owns the live constraint, so the entry itself no longer
binds today. The provenance lines remain in `skinny/REDRESS.md` unchanged;
what changed is whether the entry is a *pre-block*. **No SK-V9 wave needs
to honour these as pre-blocks** — but the live superseder *is* binding and
is listed.

| # | SUPERSEDED entry | Superseder (the live constraint) | Why it no longer binds |
|---|---|---|---|
| 35 | Codegen lowerer scaffolding gap IDENTIFIED | 40, 48, 71, 81 | The admit chain delivered the concrete generator path the gap diagnosed; 35 is a *diagnosis*, the chain is a *delivered generator*. |
| 36 | JSON-hardcoded scalar references in `bbnf-simd` IDENTIFIED | 85, 86 (Lock 14 Phase A–D) | The admits neutralised the grammar-name leaks under the Lock 14 fence; 36 diagnosed JSON-name presence, the admits removed it. |
| 37 | `bbnf-simd/src/lib.rs` JSON god-module status IDENTIFIED | 85, 86 (Lock 14 Phase A–D) | The admits refactored the structure into the generic-crate codegen shell; 37 diagnosed god-module structure. |
| 38 | `crates/simd-scan/` fossil status IDENTIFIED | SK-V6/V7 crate restructure | The workspace member was removed pre-SK-V7; the directory does not exist on disk (P1-V3-E §2.7 spot-check). |
| 46 | Direct-number / context-sink redress | 71, 81 (typed DirectBuild admits) | 71/81 land the direct-number outcome at row level (`real_typed_struct A / GO` on numeric corpora); 46 was a context-sink proposal. |
| 49 | Generated source-hook string ADMIT (direct) | 66 (source-hook field-layout REJECTED) | 66 closed the field-layout route that consumed 49's surface; the surface exists but the route is closed, so 49 is no longer a forward producer. |
| 70 | First `real_typed_struct` attempt REJECTED | 71 (generated typed DirectBuild ADMIT) | 71 landed the second attempt under the host/API schema lesson; the architectural lesson is preserved in 70, but the live producer is 71. |

**Caveat — supersession is not re-admission.** SUPERSEDED means the entry
is not a *standalone* pre-block; it does **not** re-open the rejected
route. REDRESS 49's source-hook surface is still closed (by 66). REDRESS
70's first-attempt route is still rejected (71 is a *different*
host/API-schema route, not a revival of 70). REDRESS 34's *rejection* of
bench-private hand parsers as Track 1 remains permanent even though 34's
defect is owned by 40 — §4 item 11 keeps that umbrella binding. No SK-V9
wave may treat a SUPERSEDED entry as license to reopen its rejected
shape; it may only treat the live superseder as the binding constraint.

**SK-V9 wave relevance:** none of the seven SUPERSEDED entries is a
pre-block for any SK-V9 wave. W-AC's typed-path lineage runs through the
live superseders 71 and 81 (not 35/46/70); W-RG's grammar-neutrality runs
through the live superseders 85/86 (not 36/37). The W-RG proof must still
honour 66 (the live superseder of 49) as a hard pre-block per §2.3.

---

## §7 — Sources

Every upstream artefact cited:

- `skinny/REDRESS.md` — entries 1–93; SUPERSEDED detail at 2285–2729
  (W4/W5/W6 unicode/string-block/value-byte rejections), 2508–2618 (W10 /
  W10b / W10c PMULL+CTZ rejections, the W10b six-row block at 2573–2585),
  2620–2729 (SK-V8 W2/W3/W4 — entries 91/92/93).
- `restart/skinny/tranches/sk-v9/HANDOFF.md` §3 (candidate boundaries),
  §5 (pre-blocked routes — the eight-item list + the binding-by-reference
  closing clause).
- `restart/skinny/tranches/sk-v9/research/p1/skv9-p1-v3-F-redress-reconciliation.md`
  §2 (REDRESS ledger reconciliation table — ~60 STILL-LOAD-BEARING / 7
  SUPERSEDED / ~14 HISTORICAL), §2.13 (SUPERSEDED supersession chains),
  §3 (pre-blocked-routes delta vs HANDOFF §5 — the four additional class
  umbrellas), §3.4 (PMU unblock changes no §5 entry).
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-A-union-event-model.md`
  §1, §6 (REDRESS pre-block citations — 50/51/53/60–72/82/83/84/88/89/92;
  the REDRESS 92 blanket pre-block check).
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-B-retained-grammar-proof.md`
  §1, §4 (differential vs REDRESS 60–72; the five-axis differential; R5
  on REDRESS 71 orthogonality), §"removes exactly one pre-block".
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-C-apache-citm-admission.md`
  §1 (REDRESS 91 differential — the whitelist gap), §4.2/§4.3
  (falsifiability gate), §6 (pre-block risk + REDRESS citations).
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-D-aarch64-asm-opportunities.md`
  §3.5 (codec broadening vs REDRESS 82), §4.3 (32-byte string-block vs
  REDRESS 83), §4.4 (CSSC CTZ string-mask consumer vs REDRESS 89), §5.3
  (SHA3 EOR3 vs REDRESS 88 — three-axis structural differential, Lock 16
  caveat).
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-E-unicode-escape-codec.md`
  §5 (REDRESS 82 five-axis material differential), §6.4 (same-wave
  conditional admission rule), §8.3 (REDRESS antecedents).
- `restart/skinny/tranches/sk-v9/research/p2/skv9-p2-F-sota-teardown-m5max.md`
  §7.3 (REDRESS 28+33 — asmjson Class-A NEON scanner pre-block, the
  `match_tiny_plain_string` rejected shape).
- `restart/skinny/tranches/sk-v9/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`
  (the six S-P2 artefacts handed to S-P3; the firm dependency order
  W-RG proof → W-UE union → W-UC/W-AS consumers; W-AC fully independent).
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` §2 (P3-E scope row),
  §3 CH3 (the pre-blocked ledger CHALLENGE lens — REDRESS 28+33, 50–55,
  60–72, 80, 82–84, 88, 89 + historical routes), §8.6 (no hypothesis
  transfer).
