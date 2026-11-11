# LOCK-6 — THE ISOMORPHISM PLANE · CRIT-1 (pass 2, WITHIN-LOCK view)

*Adversarial critic 1 of 2. Weight: internal soundness of each family spec and whether the pass-2
prototypes tested the **sharpest** uncertainty. Every claim below was checked against the live tree this
session; the two load-bearing verifications are pinned inline. Convergence is my conservative floor, not a
negotiated midpoint. Pass-1 CRIT-1 floors carried for delta: 6A 62 / 6B 48 / 6C 60 / 6D 55.*

**Headline.** Pass 2 finally RAN the two experiments both prior passes skipped (PROTO-1 = floor decider;
PROTO-2 = 6B gen-vs-hand number + anchor). Both RAN_MEASURED — a real, creditable advance over pass-1's
"crown with the least evidence." But **each prototype measured a softer target than its family's sharpest
uncertainty**, and in two cases the softening is load-bearing: PROTO-1 proves the floor with a **Rust↔Rust**
divergence (never a Rust↔TS fold), and PROTO-2 measures the gen-vs-hand ratio on the **one face that was
already built**, leaving 6B's three actual new faces at 0%. The stratification (§5) is now better-evidenced
than pass-1 but still hangs on one tautological number and one unbuilt mechanism.

---

## 6A — MIRROR-TREE · ADVANCED · dependent-on-6D · **66%** (was 62)

**What genuinely advanced (credit).** (1) The **impedance-derived integer cap** `MAX_EXEMPTIONS = 2·T + I`
is a real answer to pass-1 open-gap 1 (the vacuous "warns as it grows" budget): it pins the ceiling to
grep-countable seams (`#[wasm_bindgen]` entry-points, a closed idiom-class enum) instead of a re-labellable
percentage — the pass-1 vacuous-gate finding is partly discharged. (2) The **Class-1/2/3 trichotomy**
dissolves pass-1 open-gap 2's identity fork by *routing* each class to a different lawful check
(provenance / name-map / exemption-budget) rather than forcing one answer — a genuine design advance. (3)
The green/red PAIR (pass-1 gap 4, "the cheapest unrun test") was produced by PROTO-1: the **structural**
green side is a real live TS-union parse (`include_str!("../ts/path.ts")`, 8 members, set-equal).

**Failure-mode findings (the cap did not fully escape its own trap).**

1. **[vacuous-convergence, RE-EMERGENT] The impedance cap equals today's observed count — it passes by
   construction and only bites future growth, which is the very "warns as it grows" pass-1 rejected.**
   `2·T + I = 2·2 + 2 = 6 = the observed count` (FAMILY-SPECS §1). The formula was reverse-fit to reproduce
   6: the multiplier "2" and the three-element idiom enum (`V8-monomorph, RegExp, lifetime-shape`) are
   chosen so the arithmetic lands on the current value. No second crate or grammar validates that
   `2·T + I` is a *principled* impedance law rather than a curve-fit to one data point (n=1). A cap set to
   exactly today's number cannot fail today; its only teeth are on growth — with no argument that growth
   *should* be forbidden at 7 rather than 9. **Justify the multiplier and the idiom enumeration against ≥2
   crates, or the RED line is a repainted version of the gate pass-1 called vacuous.**

2. **[elegant-reduction, UNCHANGED] The mechanism that is the family remains UNBUILT; only the feasibility
   ratio and the structural-green half ran.** "On GREEN it emits the parity table `:228`, shrinking that
   test to a generated fixture" describes a linter that does not exist. The manifest, the T1 provenance
   check, the `schema.rs` prose deletion, and the TS-side declaration extractor on the *real* surface all
   never ran (PROTO-2's rustdoc measurement was the 12-line toy twin, self-labelled). The hard half —
   author the linter and run it against the live `bbnf-path` ↔ `bbnf-path-ts` pair — is still read as
   "and-then." 6A's core deliverable is SPEC_ONLY.

3. **[proxy-substitution in the load-bearing evidence] The behavioral RED that anchors 6A's
   "necessary-not-sufficient" claim is a Rust↔Rust divergence, not Rust↔TS.** VERIFIED in
   `floor_probe.patch`: the red assertion compares `check_path_against_registry::<bbnf::path::Json>` (host)
   against `compile_path_native(…, "Json")` (the cdylib, also a host call) — **both Rust**. The TS union is
   parsed only for the *green* structural side. So "names green while behavior diverges" is demonstrated
   between two Rust implementations, with the cdylib standing in for what TS wraps. The floor-existence
   argument generalizes (if two same-taxonomy Rust impls diverge, a fortiori Rust/TS can), so this is not
   fatal — but it is **weaker than the spec's framing admits**, and the divergence is itself a latent *bug*
   (canonical checker and cdylib *should* agree on the same input/registry): a single anecdote, not a
   demonstrated divergence *class*. The claim "structural caps are necessary-not-sufficient for Rust↔TS
   parity" rests on an intra-Rust proxy plus one accidental bug.

4. **[identity fork, FORMALLY UNRESOLVED] The trichotomy routes the fork but does not close pass-1 gap 2.**
   The spec asserts Class-2 (the ~19-item hand-authored taxonomy) is "the ONLY class where a name-map is
   non-tautological." But it never *proves* the clean spine is hand-authored-twice rather than
   grammar-derived — it *assigns* it to Class 2 by fiat. If any Class-2 item is in fact projected from one
   source (as Class-1 is), 6A's genuine load shrinks again toward the idiom residue. The routing is a
   plausible partition, not a verified one.

5. **[cross-lock, carried] LOCK-3 emitter ownership (gap 5, pass-1) and LOCK-5 ONE-HIR regex exemption
   (gap 6) are re-stated as "proposals," not adjudicated.** Who owns the grammar-derived half of the name
   map remains unnamed; the permanent Rust-DFA↔TS-RegExp exemption is still unadjudicated against
   constitution ii (routed to 6D as "behavioral," which is defensible but not a LOCK-5 verdict).

6. **[open, carried] Cross-repo CI ownership under F6** (per-repo manifest fragments + vendored corpus):
   proposed, unbuilt — the family's hardest logistics, unmoved this pass.

**Exact open gaps:** (i) impedance cap curve-fit to n=1, unjustified as a law; (ii) linter + manifest +
prose-deletion + real-surface TS extractor UNBUILT (SPEC_ONLY core); (iii) the necessary-not-sufficient
anchor is a Rust↔Rust proxy + one latent bug, never a Rust↔TS fold nor a divergence class; (iv) the Class-2
"non-tautological" assignment is asserted, not proven; (v) LOCK-3 name-map ownership unnamed; (vi) LOCK-5
regex-exemption unadjudicated vs constitution ii; (vii) cross-repo CI unbuilt.

---

## 6B — SPEC-GENERATED-SKELETON · ADVANCED · SPEC_ONLY · **54%** (was 48)

**What genuinely advanced (credit).** (1) PROTO-2 RAN — the anchor question got a real, honest answer:
`classify_materialization` + `compute_payload_layouts` **do** derive a decode-*class*/tier and byte-offset
slots from grammar shape (a live seam, verified: `crates/ir/src/passes/materialization/`), while the
`STRING_NEEDS_DECODE`/`GRAMMAR_BIT0` flag-*bit* is hand-authored policy living only in read-only `skinny/`.
That converts pass-1 gap 3 (mis-cited phantom anchor) into a measured partial: **tier + payload layout
GENERATED, fixed flag-bits LINTED** — the §5 partition line at "~15 constants → linted" is now
measurement-confirmed, not asserted. (2) The schema face emits under `tsc --noEmit --strict` clean across
json/sheets/css_l4 — real, correct generation.

**Failure-mode findings (the number is measured on the wrong face, and it is tautological).**

1. **[elegant-reduction, DECISIVE] The decisive gen-vs-hand number was measured on the ONE face that was
   already built; the three faces that ARE the family are still 0%.** VERIFIED: `emit_tape_layout`,
   `emit_tape_decoder_skeleton`, `emit_oracle`, `LayoutSpec` — **grep returns empty tree-wide.** PROTO-2's
   own §2 concedes it "measured the existing generation" (`emit_type_definitions_impl`, already live). So
   Face I (layout — the spec's own "where drift actually lives") and Face III (oracle — "highest-leverage")
   contributed *zero* lines to the "1.00 ratio." The family's verdict number is measured on the surface that
   was never in doubt and 0%-measured on the surface that is the work. This is the pass-1 gap-2 finding
   ("all three new faces 0% built") **unchanged** — the prototype did not touch it.

2. **[vacuous / spec-cites-itself] The 1.00 ratio is definitional for 2 of 3 grammars and tautological by
   framing.** Only JSON's faithful hand baseline (9 lines) was independently authored and tsc-verified;
   sheets (37) and css (184) are "≡ by construction" — i.e. **asserted equal to the generated output, not
   authored**. And the framing itself rigs the result: "faithful consumer type ≡ `parse()` return type ≡
   emitted union," so hand==gen is true *by definition*, n≈1 measured. The only *non*-tautological
   comparison — the idiomatic baseline — went the **other way** (json 1.29, sheets 3.08, ">1.0"), and the
   spec dismisses it as "a faithfulness tax hand pays equally." That dismissal is the load-bearing
   assumption, not a finding: whether generation earns its cost turns entirely on whether consumers want the
   faithful union or a domain AST, and the spec *chooses* faithful because "the isomorphism plane requires
   the faithful mirror." Plausible, but it makes the headline number circular. (The honest defense of 6B is
   the **drift/author-twice** axis — author-once-both-languages beats author-twice-and-sync even at ratio
   1.0 — which the spec underuses in favor of a tautological LOC number.)

3. **[elegant-reduction, UNCHANGED] The AoS↔SoA reconciliation is still parked "GN-1 reconciles."** VERIFIED:
   no unified `LayoutSpec` exists; `MaterializationClass` yields "only a class/tier" (PROTO-2's own words),
   not a byte layout emitting both the AoS-stored-discriminator dialect and the SoA-derived dialect. The
   pass-2 directive — "demonstrate one-spec-two-dialects expressibility OR file it as the reconciliation
   blocker" — was discharged by **neither**. The load-bearing hard core is exactly as unconverged as
   pass-1, still dressed "definitional."

4. **[SPEC_ONLY perf claim, UNCHANGED] The `%HaveSameMap` V8 monomorphism gate never ran.** PROTO-2 ran
   `tsc`, not `node --allow-natives-syntax`. The claim that the generated skeleton is single-hidden-class
   shape-stable — on which the 4.4× stop-early depends — is still asserted, not measured (constitution vii /
   prototype-evidence-misuse: a perf-load-bearing invariant on SPEC_ONLY footing).

5. **[cross-lock, OPEN by the spec's own admission] 6B-as-layout-gen ⊗ LOCK-4C VM is flagged "the lock's
   sharpest unresolved cross-lock adjudication" and left unresolved.** The re-opening (the ts-rs type-gen
   refutation does not bind a shared bytecode-VM instruction stream) is a *sound* correction of pass-1's
   over-foreclosure — but it is stated, not adjudicated, and it is owed jointly to LOCK-4/LOCK-8 who cannot
   answer inside this lock. Honestly labelled OPEN; counts against convergence, not against soundness.

6. **[cross-lock, carried] LayoutSpec + emit_* faces annex LOCK-3 driver / LOCK-2 CSP substrate** — no
   supersession named (pass-1 gap 6, unmoved).

**Exact open gaps:** (i) the decisive number is 0%-measured on Face I and Face III (the actual new work) —
measured only on the pre-existing schema face; (ii) the 1.00 ratio is definitional for 2/3 grammars and
tautological by the faithful==parse()-output framing, while the only non-circular baseline is >1.0; (iii)
AoS↔SoA one-spec-two-dialects neither demonstrated nor filed as blocker (directive unfulfilled); (iv) V8
`%HaveSameMap` shape probe unrun — the monomorphism/perf invariant is SPEC_ONLY; (v) 6B-as-layout-gen ⊗
LOCK-4C VM unadjudicated (owed to LOCK-4/8); (vi) LOCK-3/2 substrate annexation unnamed; (vii) cross-repo
regen gate (F6) does not exist.

---

## 6C — WASM-CORE · BANKED · **63%** (was 60)

**What genuinely advanced (credit).** (1) The **Q6 sync-instantiation correction** is a sound, well-cited
sharpening: the 4 KB synchronous-`new WebAssembly.Module()` limit was removed in Chrome 114 (V8 lazy
compilation), so the constraint survives only as a **one-time boot obligation**, not a per-parse async tax —
this correctly de-escalates pass-1's over-strong R-E severity without disturbing the disposition. (2) The
**Q7 re-attribution** (the wasm128 kernel cost belongs on LOCK-9's ledger; the row-09 refutation binds NEON,
not the wasm arm where `v128` is the only SIMD) is a clean interlock naming
(`6C.wasm-scan ⇐ 9A.kernel-home; 9B is the cost-bearing fallback`). (3) The V4 finding surfaces LIVE x86
(`avx2.rs`/`avx512.rs`) in a probed crate against constitution iv and routes it to GESTALT rather than
burying it — honest.

**Failure-mode findings.**

1. **[sharpest-uncertainty-skipped, TWICE] The one experiment that would close the surviving bbnf-side
   decision was not run — for the second consecutive pass.** The disposition confines 6C to "zero-copy
   crossing on the bbnf side," but whether the bbnf parser *should* live in wasm turns on **wasm-SIMD128 vs
   a TS-native scalar scan** (both browser-deployable). PROTO-1 measured wasm-SIMD128 vs **native aarch64
   NEON** — not a deployment option — and FAMILY-SPECS §3 gap (i) *admits* "the decision-relevant comparator
   was never run." The pass-2 directive named this exact experiment; it was skipped again in favor of
   re-confirming the already-settled NEON comparison. 6C-on-the-bbnf-side is **undecided**, and the pass did
   not attempt to decide it.

2. **[inherited-not-remeasured] 6C's *surviving* role rests entirely on PROVE-14, not on any pass-2
   measurement.** Gap (ii) concedes PROTO-1 measured the scan kernel (source→tape), not the crossing
   (planes→typed-arrays). The 4.4×/0.9× facts that justify 6C-as-crossing are legitimate prior evidence but
   inherited; nothing this pass re-touched the crossing.

3. **[unbuilt gate that actually bites, UNCHANGED] R-B memory-grow view-invalidation** — a `memory.grow()`
   detaches every outstanding typed-array view; the two-parses-through-one-instance corruption case (the one
   hazard that bites the *residual* 6C, not the retired maximal 6C) is still never exercised. This is the
   gate the pass-2 directive named as second priority; unrun.

4. **[F6-bound assumption, UNCHANGED] R-E rests on the unverifiable "value.js has 59 sync `all()` sites"** —
   a parse-that READ-ONLY datum the confinement hinges on, not re-confirmed. The gestalt must carry it on
   faith; unflagged as such in the disposition line.

5. **[carried] Homing collision (V5) unpriced** — `wasm/Cargo.toml` already claims `name="bbnf-wasm"`; one
   consolidated cdylib vs two was asserted-better (Q6) but not measured.

**Exact open gaps:** (i) the decision-relevant wasm-SIMD-vs-TS-native-scalar comparison unrun for a 2nd
pass — bbnf-side viability undecided; (ii) the crossing role is PROVE-14-inherited, unre-measured this pass;
(iii) R-B memory-grow corruption gate unbuilt; (iv) R-E confinement rests on an unverified cross-repo count
(F6); (v) homing-collision cost unpriced. *(Fate — BANKED-not-RETIRED — is well-converged; viability of the
surviving role is not.)*

---

## 6D — ORACLE-ISOMORPHISM · the mandatory FLOOR · SPEC_ONLY · **61%** (was 55)

**What genuinely advanced (credit).** (1) PROTO-1 RAN the floor decider — the cheapest experiment in the
lock, skipped in both prior passes — and produced the designed green-structural / red-behavioral pair. As a
floor-*existence* proof (name-set equality does not imply behavioral equality) it is sound and creditable.
(2) The **Q10 IEEE-754 correction** — fold source-byte number spans / RFC-8785 canonical decimal, **never
raw IEEE-754 bytes** (which re-import the NaN/±0 hazard and manufacture false divergences) — is genuinely
correct reasoning and dissolves a real hazard; the LE-vs-BE catch on the as-built fold is a real bug spotted.

**Failure-mode findings (the floor is proven necessary; the family's own mechanism is 0% exercised).**

1. **[proxy, and the digest never folded] The floor proof establishes necessity via a Rust↔Rust taxonomy
   divergence — 6D's actual artifact (the cross-language differential digest) was never run.** VERIFIED: the
   RED compares two Rust host paths (see 6A finding 3); no fold, no digest, no TS-side arithmetic entered
   the behavioral test. So "6D is a mandatory floor" is confirmed, but "6D's digest works cross-language" is
   exactly as SPEC_ONLY as pass-1. Gap (iii) — "the Rust-side fold does not exist" — is unmoved; the folded
   comparison the family *is* was not built.

2. **[design flip-flop, unresolved empirically] The digest design reversed between passes without either
   version being folded cross-language.** Pass-1's registry called the "8-byte IEEE-754-bit digest" 6D's
   "single most important sharpening"; pass-2 (Q10) reverses it to "NEVER fold raw bits." The pass-2
   reasoning is better — but this is a load-bearing substrate oscillating across passes, and **neither the
   bit-fold nor the span-fold has ever been run cross-language**. An internal supersession named in prose is
   not an empirically settled digest.

3. **[gate-cannot-fail, UNCHANGED] Gate-(a) "100% digest agreement" today exercises only the EASY direction
   — agreement among correct impls.** The family's own gap (iv) admits the teeth (coverage-guided grammar
   fuzzer + delta-debug shrink) are unbuilt, so the passing gate has no adversary. A conformance gate that
   only ever sees inputs it passes is a gate that cannot fail — the exact failure-mode the whole lock is
   supposed to police.

4. **[substrate change unbuilt] u32→u64 fp widening — "the single most load-bearing substrate change" — is
   spec text, not a diff.** The u32 collision at ~77k inputs is the stated silent-escape hazard; the fix is
   asserted, unbuilt.

5. **[carried] Both verdict numbers (drift-escape rate, marginal maintenance-cost-per-grammar) remain
   UNMEASURED** — 6D's entire disposition per its own §6.4. The floor's *necessity* is now proven; its
   *cost/benefit as the chosen floor* is still 0%-measured.

**Exact open gaps:** (i) the cross-language digest fold — 6D's actual mechanism — was never built or run;
the floor proof is a Rust↔Rust proxy; (ii) the digest design (bits vs spans) flip-flopped across passes and
neither variant was folded cross-language; (iii) gate-(a) exercises only agreement-among-correct-impls — the
fuzzer teeth are unbuilt, so the gate cannot fail; (iv) u32→u64 widening unbuilt; (v) both verdict numbers
(drift-escape, maintenance-per-grammar) UNMEASURED.

---

## Cross-family / constitution findings (within-lock)

- **[prototype-evidence-misuse, the pass's structural weakness] Both pass-2 prototypes measured a softer
  target than their family's sharpest uncertainty.** PROTO-1: floor-via-Rust↔Rust-proxy instead of a
  Rust↔TS fold, on a latent bug rather than a divergence class. PROTO-2: gen-vs-hand on the pre-existing
  schema face instead of the three unbuilt faces that ARE 6B, with a ratio that is definitional for 2/3
  grammars. Neither is *wrong*; both are *less decisive than presented*. The registry's own pass-1 warning —
  "do not read two green probes as convergence on the crown" — applies again with the polarity kept: the
  probes moved the *feasibility* frontier, not the *mechanism* frontier.

- **[consumer-less-substrate, adequately handled] The 6A⊥6B tautology** (a linter comparing a generator's
  two outputs) is correctly identified (§5 incompat 1, Y.13) and dissolved by the surface partition — no
  violation, provided Class-1 provenance genuinely has two independent consumers (it does:
  `bbnf-path/src/registry.rs` + `bbnf-path-ts/src/fixture.rs`).

- **[grammar-totality, honestly conceded] 6B's "impossible-by-construction drift" thesis covers only the
  generated sub-surface;** PROTO-2's own finding that the flag-bit is hand-authored policy means Face I is
  "PARTIALLY generated" — the constants fall back to a 6A linter. This is honest and is the measured
  partition line, not a violation — but it means the strongest 6B claim ("drift impossible on the
  declarative half") is narrowed by measurement to "on the schema+tier+payload sub-half."

- **[constitution iv, surfaced-not-buried] LIVE x86 `avx2.rs`/`avx512.rs`** in `simd-scan` against "x86
  dead" is correctly surfaced to GESTALT/LOCK-9 (V4). No burial. Good.

**Net (within-lock).** The stratification (§5) is a well-formed, now-partly-evidenced hypothesis — stronger
than pass-1 on 6A's gate design (impedance cap), 6B's anchor (partial-derivation measured), and 6D's floor
(necessity proven). It is **not converged**: the partition's two reasonable-point numbers are one
tautological (6B's 1.00, definitional for 2/3 grammars, measured on the wrong face) and one now-measured but
soft (6A's ratio). No family clears 70. The plane's crown mechanisms (6A's linter, 6B's three faces + AoS/SoA
spec, 6D's cross-language digest) are all still SPEC_ONLY; the pass produced feasibility evidence, not
mechanism evidence.
