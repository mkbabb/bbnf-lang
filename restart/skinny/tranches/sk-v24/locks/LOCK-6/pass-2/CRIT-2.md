# LOCK-6 — THE ISOMORPHISM PLANE (rust/ts split): ADVERSARIAL CRITIC 2, pass 2

*Critic 2 of 2. Charge: harden every family against the failure-mode checklist AND weight the
CROSS-LOCK view — where LOCK-6 contradicts or double-homes against the GN/PT codex and the sibling
locks. Convergence % + enumerated open gaps per family. Every tree/probe claim re-verified this
session (paths and the two patches read, not trusted from prose). Pass-2 is a genuine advance — both
briefs the pass-1 critics demanded ran (PROTO-1 = Brief 2 floor decider; PROTO-2 = 6B number), SPEC_ONLY
labels are applied, and 4 of the 5 pass-1 cross-lock ties are now NAMED. This critique attacks the two
places where a probe RAN but tested a proxy for the real question, and the one live cross-lock
contradiction the synthesis names but does not adjudicate.*

---

## 0. Verification ledger (re-checked before critiquing)

- **PROTO-1 RAN — but it is a Rust↔Rust divergence, not Rust↔TS (the sharpest pass-2 finding).**
  Read `floor_probe.patch`: the STRUCTURAL test parses the TS union names from `include_str!("../ts/
  path.ts")` (green, real). The BEHAVIORAL test — the one that *proves 6D is a mandatory floor* —
  drives `check_path_against_registry::<bbnf::path::Json,()>` against `compile_path_native(...)`.
  **Both are Rust** (`crates/bbnf-path-ts/src/lib.rs:69` defines `compile_path_native`; the probe's own
  comment calls it "the same shape the wasm-bindgen export serializes" = the host-side mirror of the
  wasm export). The TS union is **never behaviorally exercised.** So the green/red pair is
  *name-set(rust,ts) green* ⊕ *behavior(rust-canonical, rust-cdylib) red*. See §4 gap (vi).
- **PROTO-2 RAN — but measured the EASY face, and its headline ratio is tautological.** The 1.00
  faithful ratio holds *by identity* for 2 of 3 grammars ("faithful consumer type ≡ parse() return
  type ≡ emitted union" — the report's own words, line 128); only JSON's 9-line baseline was
  independently authored + typechecked. The face measured is the **type/schema interface** — which
  pass-1 critic-1 (6B-ii) already found "partly present via `backend_ts.rs`," and PROTO-2 confirms it
  "already exists and is live" (`emit_type_definitions_impl`). The **load-bearing Face I (tape-layout
  gen + AoS↔SoA)** — the greenfield the family is crowned to own — was **not built and not measured.**
  See §2 gaps (i)(ii)(iii).
- **Cross-lock homing re-verified in-tree:** `crates/bbnf-wasm` **ABSENT**; `wasm/Cargo.toml` already
  declares `name = "bbnf-wasm"` (V5's collision is real, still unpriced). `value_parity.rs` **ABSENT**
  tree-wide (the four-way value oracle three families lean on is unbuilt); `crates/core/tests/
  tape_substrate.rs` and `parity.rs` exist (some substrate is live, the four-way one is not).
- **The two probes' PROVENANCE is clean** (both fast-forwarded to base-pin `7cdbff4ef` before
  measuring; no wrong-artifact risk; parse-that untouched; no x86; no perf claim). Credit given.

---

## 1. Family 6A — MIRROR-TREE — convergence **74%** (↑ from pass-1 70%)

**What pass-2 earned.** The Q2-identity trichotomy (Class-1 provenance / Class-2 name-map / Class-3
budget) genuinely dissolves the pass-1 "hand-authored-twice vs grammar-derived → tautology" dilemma
that critic-2 §1.4 and critic-1 §6A-ii both raised — a real design advance, correctly routing each
class to a *different* lawful check. The three-conjunct gate replaces pass-1's vacuous "warns as it
grows" budget (my gap §1.1) with two falsifiable RED lines (0%-tolerance mapped-set; allow-list
monotonicity). Both are earned.

**Where it does not hold (enumerated open gaps):**

1. **The impedance cap `MAX_EXEMPTIONS = 2·T + I` is fit-to-observation and internally inconsistent —
   the "un-gameable number" cannot fail today because it was set to today's count.** The spec says
   "2·2 + 2 = **6** = the observed count," but the enumerated idiom set it defines is *three* members
   ("V8-monomorph, RegExp, lifetime-shape") → `I` should be 3 → `2·2+3 = 7`, not 6; and Class-3 lists
   **six** concrete exempt items (`#[wasm_bindgen]`, `init()`, `makePathTag`, V8-monomorph, RegExp,
   lifetime-shape). The coefficients (`2·` multiplier, `+I`) are reverse-derived to land on the
   already-observed 6. A cap calibrated to the current count is a **gate that cannot fail on the
   surface it was measured on** — its falsifiability ("exceed → RED") has never been exercised against
   a real overrun. Gap: pin `T` and `I` to a grep-countable definition **independent of** the observed
   total, then show the cap RED on a synthetic 7th exemption.
2. **The linter itself is STILL UNBUILT — the green/red PAIR was produced by a *bespoke* test, not the
   `mirror-map.toml` manifest linter.** PROTO-1 hand-replicated the fixture and asserted `BTreeSet`
   equality inline; it did **not** build the manifest-driven T1/T2 linter, the `syn`-over-declarations
   extractor, the `dist/*.d.ts` TS-side extractor, or delete the `schema.rs` prose. So 6A's *core
   deliverable* remains SPEC_ONLY exactly as in pass-1 (my gap §1.2, critic-1 §6A-iii); only a bespoke
   demonstration of the *principle* ran. "The mechanism is UNBUILT" is the spec's own open-Q (i) — it
   survived the pass unchanged.
3. **The green/red pair proves the principle on a Rust↔Rust proxy, not the rust/ts surface 6A gates
   (shared with §4 gap vi).** The structural tier reads TS names; the behavioral divergence is
   canonical-Rust vs cdylib-Rust. This is a-fortiori-defensible (if same-language name-parity fails to
   guarantee behavior-parity, cross-language surely does) — but it means 6A's "necessary-not-sufficient"
   is demonstrated by proxy, and the actual TS-union behavioral fork the linter would miss is unmeasured.
4. **Cross-repo CI ownership under F6 (open-Q iii) remains prose** — per-repo manifest fragments +
   vendored corpus, unbuilt, the family's hardest logistics, unchanged from pass-1.

**Cross-lock (pass-2 now names these — credit):** the LOCK-3-owns-Class-1-names interlock and the
LOCK-5-ONE-HIR/regex-exemption compatibility are now explicitly stated (closing my pass-1 §1.5). The
freeze order (AFTER LOCK-1 + LOCK-7) is carried. Residual: whether LOCK-3 or LOCK-6 *owns* the linter
that gates Class-1 provenance is stated as a "proposal," not settled.

---

## 2. Family 6B — SPEC-GENERATED-SKELETON — convergence **60%** (↑ from pass-1 55%)

**What pass-2 earned.** PROTO-2 ran (closing pass-1's zero-prototype defect) and the schema-face
generation is **real and tsc-strict-clean** across json/sheets/css_l4 — that is concrete evidence, not
SPEC. The anchor is partially rescued: `classify_materialization` + `compute_payload_layouts` are
genuine grammar→layout-fact derivations (verified live), so Face I is not wholly phantom. Q3-bounding
(generator owns schema + N-grammar classify; linter guards the ~15 fixed constants) and the Q4
layout-gen re-opening are principled. Credit.

**Where it does not hold (enumerated open gaps):**

1. **The headline "generation earns its cost, ratio 1.00" is a TAUTOLOGY, and the honest signal points
   the other way (R3 is NOT excluded).** The faithful baseline is *defined* as the emitted union →
   ratio ≡ 1.0 by construction for sheets/css (only JSON's 9 lines were independently authored). A
   metric that is 1.0 by identity measures nothing about whether generation *saves labor*. The
   non-tautological signal is the **idiomatic** ratio: json 1.29, **sheets 3.08** — generation emits
   3× the lines a human would idiomatically write. The spec explains this away as "a faithfulness tax
   hand-authoring shares equally," but that is precisely R3's claim inverted: **if the idiomatic type
   is what a consumer actually wants, the human writes 12 idiomatic lines + a thin adapter, not 37
   generated faithful lines — and the adapter cost is UNMEASURED.** R3 ("linter/hand is cheaper")
   survives on the only baseline that isn't circular. Constitution vii: the "earns its cost" verdict is
   SPEC-grade dressed as measured.
2. **THE ELEGANT-REDUCTION TRAP PERSISTS — PROTO-2 measured the face that was already live and left the
   load-bearing hard face untouched.** The type/schema interface PROTO-2 measured is `emit_type_
   definitions_impl`, which the report itself finds "already exists and is live." The three faces the
   family IS (`emit_tape_layout`, `emit_tape_decoder_skeleton`, `emit_oracle_table`) remain **0% built**
   (pass-1 critic-1 §6B-ii, unchanged). The read is: *the easy half (type gen, pre-existing) ran green,
   and-then-the-hard-part (tape-layout gen) is unconverged.* The number that would decide 6B's crown —
   gen-vs-hand LOC **for the tape/decoder face** — is still 0% measured.
3. **The AoS↔SoA reconciliation — the genuinely hard unconverged core — is still parked "GN-1
   reconciles" (open-Q iii), dressed "definitional."** One `LayoutSpec` emitting BOTH the AoS
   stored-discriminator (`record.rs`) AND the SoA discriminator-free offset-tape (kind re-derived from
   `source[offset]`) is not obviously expressible; pass-1 flagged it (critic-1 §6B-iv, critic-2 §2.3),
   pass-2 restates it verbatim as a hard core "dressed definitional." Zero progress on the plane's
   load-bearing expressibility question.
4. **The %HaveSameMap V8 shape gate (b) remains UNRUN — a perf-load-bearing invariant on SPEC_ONLY
   footing.** Pass-1 critic-2 §2.2 flagged that the monomorphism claim protecting the 4.4× stop-early
   is asserted, not probed. Unchanged. R1 (generator emits V8-hostile TS) is still untested.
5. **The flag-bit anchor is CONFIRMED phantom by PROTO-2 itself.** `STRING_NEEDS_DECODE`/`GRAMMAR_BIT0`
   are absent from `crates/` (live only in READ-ONLY `skinny/`, hand-authored per-grammar). So Face I's
   *flag-bit layer* is LINTED, not generated — the spec concedes this. Fine as a concession, but it
   means "Face I 100% generated" is retired to "Face I partially generated," narrowing 6B's crown.
6. **Cross-lock: the LOCK-4C ⊗ 6B-as-layout-gen contradiction is NAMED but explicitly NOT
   adjudicated — the plane's one live unresolved cross-lock item.** §2 Claim B-full re-opens 6B for the
   LOCK-4C shared-VM instruction stream and calls it "the lock's sharpest unresolved cross-lock
   adjudication"; the §5 table marks that row "OPEN." Pass-2 correctly *repairs* pass-1's overclaim
   ("no 6B on combinators" was conflating type-gen with layout-gen), but repairing the overclaim opens
   an undecided fork: **does the combinator layout ride 6B (LOCK-4C VM stream) or hand-code?** LOCK-8C
   ties its mirror to LOCK-4A/4C "shared instruction shape." This is the interlock the gestalt collides
   on first, and it is deferred, not resolved.
7. **Cross-lock double-home (still only "name the ownership," not resolved):** `LayoutSpec` + `emit_*`
   faces home inside LOCK-3's driver and consume a LOCK-1/2 CSP variable (`MaterializationClass`). §2
   now says "name the ownership before building" — an improvement over pass-1's silence, but the
   ownership is *named as open*, not assigned. Who owns the derivation: LOCK-3 or LOCK-6?

---

## 3. Family 6C — WASM-CORE — convergence **76%** (↑ from pass-1 72%)

**What pass-2 earned (the lock's best-evidenced fate).** PROTO-1's R-A-negative (wasm SIMD128 0.34–0.47×
native NEON, parity-exact) firmly earns the BANKED disposition. The Q6 correction — the 4 KB
synchronous-instantiation limit was removed in Chrome 114, so R-E is a *one-time boot obligation* not a
per-parse async tax — is a real, well-sourced sharpening that softens (without dissolving) the
confinement. The LOCK-9 interlock is now NAMED (`6C.wasm-scan ⇐ 9A.kernel-home; 9B is the cost-bearing
fallback`), closing my pass-1 §3.4 double-home complaint.

**Where it does not hold (enumerated open gaps):**

1. **The decision-relevant comparator STILL was not run (unchanged from pass-1 critic-1 §6C-i,
   critic-2 §3-implicit).** PROTO-1 measured wasm-SIMD128 vs native-NEON (native not browser-
   deployable). The bbnf-side question — **wasm-SIMD128 vs a TS-native scalar scan** (both
   browser-deployable) — was NOT measured. The spec honestly carries this as gap (i) ("6C-on-bbnf-side
   is undecided, not settled-viable"), but it is the second pass in a row the actually-decisive
   comparison was skipped in favor of a NEON comparison that cannot settle the browser home.
2. **R-B memory-grow view-invalidation gate STILL unbuilt (gap iii, unchanged).** A `memory.grow()`
   detaches every outstanding typed-array view; the two-parses-through-one-instance corruption case —
   the one hazard that actually bites the *surviving* 6C crossing role — has never been exercised. The
   gate that "must not corrupt the first parse" has never been made to fail.
3. **R-E rests on an unverifiable cross-repo count (gap iv, F6-bound).** "value.js has 59 sync `all()`
   sites" confines 6C to the bbnf side and is un-checkable in-tree. Plausible, load-bearing, second-hand.
4. **Homing collision (V5) confirmed real and unpriced (gap v).** `crates/bbnf-wasm` absent vs `wasm/`
   already `name = "bbnf-wasm"` — verified this session. The one-vs-two-cdylib cold-instantiation
   decision (§2b) was not measured either pass.

**Cross-lock:** the x86-in-probed-crate constitution-iv violation (V4: `avx2.rs`/`avx512.rs` live in
`simd-scan`) is surfaced to GESTALT (correct — it is LOCK-9 surface). Freeze order (AFTER LOCK-1/5/7/9)
carried. This family's *fate* is well-converged; its *residual viability on the bbnf side* is the open
part, and the spec is honest about that.

---

## 4. Family 6D — ORACLE-ISOMORPHISM / DIGEST (the mandatory FLOOR) — convergence **66%** (↑ from 60%)

**What pass-2 earned.** The IEEE-754 correction is the pass's decisive advance and it *reverses* a
pass-1 hazard rather than merely restating it: the pass-1 "fold the 8-byte f64 bit pattern" insight was
a **false-divergence generator** (NaN payloads, ±0, endianness), and pass-2 correctly re-specs the fold
over source-byte number *spans* / RFC-8785 canonical decimal, never raw bits — with a sound correctly-
rounded-parse argument (Eisel-Lemire ↔ V8 StringToDouble). The u32→u64 widening (collision at ~77k by
the birthday bound → ~4B) is correct and load-bearing. The error-class-in-contract decision (gate the
enum, not wording/offset) is a clean named supersession. These are real.

**Where it does not hold (enumerated open gaps):**

1. **BOTH verdict numbers remain UNMEASURED (gap i, unchanged from pass-1).** Drift-escape rate and
   marginal maintenance-cost-per-grammar are 6D's *entire verdict* and no prototype produced either.
   6D is installed "everywhere as the floor" while its two self-nominated deciders are 0% measured —
   the same vacuous-convergence structure pass-1 flagged. SPEC_ONLY label correctly applied; the
   disposition ("ADVANCED · mandatory FLOOR") still reads with more confidence than the evidence.
2. **The teeth are STILL SPEC_ONLY → gate (a) exercises only the EASY direction (gap iv, acknowledged).**
   The coverage-guided fuzzer + delta-debug shrink — the mechanism that turns "wins unless we forget a
   test" into "wins because drift cannot hide" — is unbuilt. Gate (a) "100% digest agreement" today
   only checks agreement among correct impls (which PROVE-14 shows for JSON); it never catches planted
   divergence. A gate that only checks agreement is close to a gate that cannot fail.
3. **The cross-language fold has NEVER RUN — the whole differential is SPEC_ONLY.** The spec's own gap
   (iii): "the Rust-side fold does not exist." PROVE-14's two arms are BOTH JS. So 6D's central artifact
   — a Rust fold of the same spec compared against the TS fold — has never been executed in any form.
   The IEEE-754 *correction* is sound reasoning, but "V8 and Rust fold identically over the corpus"
   remains unexercised (the correction even notes "the prototype must still exercise it — PROVE-14 never
   did"). The floor's transport has zero running instance.
4. **The floor-is-mandatory proof is a Rust↔Rust proxy (shared with §1 gap 3, §0).** PROTO-1's
   behavioral red is canonical-Rust vs cdylib-Rust; the TS surface is never behaviorally driven. 6D is
   specifically "the floor for the rust/ts split," and the demonstration that structure is
   necessary-not-sufficient was produced on a *same-language* pair. Defensible a-fortiori, but the
   probe is labeled "the mandatory-floor decider" for a split whose boundary it does not cross. Gap:
   drive the TS `path.ts`/cdylib surface behaviorally against canonical Rust, not two Rust paths.
5. **Cross-lock ordering now carried (credit) but the reasonable-point in/out partition is named-not-
   drawn (gap v).** "Freeze strictly AFTER LOCK-1 (alphabet) + LOCK-7 (tie-breaker)" is stated — closing
   my pass-1 §4.4. But the in/out-of-contract partition (values+accept/reject in; wording+span out;
   error-class toggle DECIDED-ON) is *asserted as data* and only partially enumerated; the "which
   divergence classes are reachable by the JSON corpus vs closed-by-spec" line is drawn in prose, not
   in a checked-in partition file.

---

## 5. §5 Incompatibility map / stratification — convergence **66%** (↑ from pass-1 58%)

**What pass-2 earned.** The partition table is now backed by *two* data points instead of one (6A ratio
47/61/0 from pass-1; 6B ratio from PROTO-2). Four of five pass-1 cross-lock ties are NAMED. The
stratification is internally coherent and the graft logic (6B-without-crossing = spec with no consumer;
6C-without-contract = two hand-transcribed alphabets) is sound.

**Where it does not hold (enumerated open gaps):**

1. **The partition rests on a 6B number that measured the WRONG face.** §5 draws the line "6B governs
   schema+tape+classify" using PROTO-2's ratio — but PROTO-2 measured the **type/schema** face (ratio
   tautological, §2 gap 1) and NOT the **tape-layout** face (§2 gap 2). The half of the surface where
   6B's crown is contested (tape layout, AoS↔SoA) has zero measured gen-vs-hand number. The partition
   line's second half is drawn on an unmeasured segment.
2. **The 6B-layout-gen ⊗ LOCK-4C VM contradiction is the one live unadjudicated cross-lock item (§2 gap
   6).** §5's own table marks the combinator-layout row "OPEN." Until it is decided, the stratification
   has a hole exactly where LOCK-6 meets the parse-that combinator core — the seam the GESTALT-P weave
   must close before it can consume this lock.
3. **The T0 byte-identity tier three families lean on is STILL an absent substrate (clause viii).**
   `crates/bbnf-wasm` absent (verified); `value_parity.rs` absent (verified). 6B gate (c) and 6C both
   cite the GN-4.W4 born-RED wasm byte cross-check as their firmest gate, and it has no home. The
   plane's most confident-sounding gate is consumer-less until W4 lands.
4. **The stratification is still, by the synthesis's own framing, a HYPOTHESIS, not a verdict.** §5.6
   says the next pass "adjudicates the cross-lock contradictions before either gestalt consumes the
   lock" — i.e. the central adjudication (6B-layout-gen ⊗ LOCK-4C) is deferred *again*. The table is
   the plane's deliverable and it carries one measured-tautological input, one unmeasured face, and one
   OPEN cross-lock row.

---

## 6. Cross-lock summary (critic 2's specific charge)

| interaction | pass-1 status | pass-2 status |
|---|---|---|
| **LOCK-4C / LOCK-8C** — combinator layout: 6B-VM-stream vs hand-code | unnamed (sharpest miss) | **NAMED, explicitly UNADJUDICATED ("OPEN")** — the one live contradiction |
| **LOCK-3 / LOCK-2** — `LayoutSpec` + `emit_*` annex the driver + a CSP variable | unnamed | **NAMED as "name the ownership"** — ownership still unassigned |
| **LOCK-9** — wasm128 kernel is a second impl; two kernel homes | unnamed | **NAMED interlock** (6C ⇐ 9A; 9B cost-bearing) — closed |
| **LOCK-5 / constitution ii** — Rust-DFA ↔ TS-RegExp exemption vs ONE-HIR | unnamed | **NAMED compatible** (behavioral routing, no layout mirror) — closed |
| **LOCK-1 / LOCK-7** — 6D floor cannot freeze before ADT + oracle | partial | **CARRIED into every family's freeze order** — closed |

Pass-2 discharged 3 of the 5 pass-1 cross-lock complaints (LOCK-9, LOCK-5, LOCK-1/7) and *named* the
other 2 (LOCK-4C, LOCK-3). **The LOCK-4C combinator-layout fork is the single unresolved cross-lock item
in the plane** — named, correctly re-opened (pass-1's "no 6B on combinators" overclaim repaired), but
handed to the gestalt undecided. It plus the LOCK-3 ownership question are the two the GESTALT-P weave
collides on first.

---

## 7. Convergence roll-up

| family | pass-1 (crit-2) | pass-2 | one-line blocker |
|---|---|---|---|
| 6A MIRROR-TREE | 70% | **74%** | impedance cap fit-to-observation + arithmetic (I=2 vs 3 classes); linter still UNBUILT (bespoke test ≠ manifest linter) |
| 6B SPEC-GEN-SKELETON | 55% | **60%** | headline ratio tautological; measured the pre-existing EASY face; tape-layout gen + AoS↔SoA 0% built; LOCK-4C fork OPEN |
| 6C WASM-CORE | 72% | **76%** | fate BANKED (strong); wasm-vs-TS-native comparator still unrun; R-B gate unbuilt; homing collision unpriced |
| 6D ORACLE-ISO (floor) | 60% | **66%** | both verdict numbers unmeasured; cross-language fold never ran; floor-proof is a rust↔rust proxy; teeth unbuilt |
| §5 stratification | 58% | **66%** | 6B number measured wrong face; LOCK-4C row OPEN; T0 byte substrate absent; still a hypothesis by its own framing |

**The plane's honest pass-2 state:** two probes RAN (a real advance over pass-1's one), the SPEC_ONLY
labels are applied, the IEEE-754 correction and the 6A trichotomy are genuine design advances, and 3 of
5 cross-lock ties are closed. But **both probes tested a proxy for the decisive question**: PROTO-1
proved the floor on a Rust↔Rust pair (not the rust/ts boundary it gates), and PROTO-2 measured a
tautological ratio on the *pre-existing easy face* while the tape-layout face the family is crowned to
own stayed 0% built. The stratification is a better-supported hypothesis than in pass-1, but its second
half rests on an unmeasured face and its central cross-lock adjudication (6B-layout-gen ⊗ LOCK-4C) is
deferred a second time. **The non-negotiable pass-3 minimum: (1) build the actual `mirror-map.toml`
manifest linter and drive the TS surface behaviorally (not a rust↔rust proxy); (2) measure gen-vs-hand
LOC on the TAPE-LAYOUT face, not the type face, and exercise the cross-language fold at least once; (3)
adjudicate the LOCK-4C combinator-layout fork before GESTALT-P consumes the lock.**
