# CRIT-codex-architecture — SK-V23 SPEC-DRAFT, the architecture + interlock lens

**Agent:** critique, adversarial/numerically-conservative. **Target:** `sk-v23/SPEC-DRAFT.md`
(821 lines, read whole). **Walked against:** the FROZEN `sk-v22/SPEC.md` (GW0–GW6, archive
sequence §2.1, the seam records, the FORWARD PROMPT §7), the `sk-v21/DEFERRED-LEDGER.md` (the
empty-chronic assertion, §0 REDRESS registry, N1–N27), the `sk-v21/ADDENDUM-GREENFIELD-PRUNE.md`,
and the settled negative results. **Convergence: 80%.** The GW0–GW6 → TB mapping is ~90% carried,
the archive sequence is fully carried, the x86 design is thoughtful, and the greenfield
delineation is largely consistent with the settled negatives — but three open design questions and
several interlock/right-sizing gaps keep it off 100.

---

## The mapping is mostly complete (what carries cleanly)

Every GW letter has a home, verified row-by-row:

- **GW0.b/c/d/e/f** → TB1.W1/W2 (removals+re-found) / TB1 close (Lock-14 re-key fires on plant) /
  TB1.W4 (six docs + EVIDENCE-INDEX full-tag-relative-path rule + the four seam records) / TB1
  close (H1 twitter reconciliation, 2.10× vs 1.59×, +36% drift disclosed). All present, SPEC-DRAFT
  lines 184–222.
- **Archive sequence §2.1 steps 1–5** → TB1.W0/W1/W2 verbatim (SPEC-DRAFT:594–595 states it, and
  the codex self-adds the sk-v23 tranche-dir copy to the rescue checks — good).
- **GW1** → TB2 (W0 edition-2024, W1 75/34 inline-test, W4 Lock-1-two-ways + grep-zero +
  simd-scan stub + channel-ratio ledgered). Complete.
- **GW2a/GW2b** → TB3.W0/W1 (regen host, WART-C fold, N1/N6/N25) / TB3.W2 (one-regex-home).
- **GW3** → TB5.W0; **GW4** split value→TB4.W2, decision→TB5.W4 with all four named gates (N22,
  N23, the perf_cost fix at `passes/src/lib.rs:590`, the PGS trigger — SPEC-DRAFT:368–372);
  **GW6** → TB6, gated on TB3 + TB1.W1 as GW6 was gated on GW2a + GW0.b.
- The settled negatives are honored: AoS carried (line 97), fused-sink default (310), vshrn
  obviates hand-asm/do-not-re-attempt (356), N*=3 no-flip → CSV binds the win-clause (399–400),
  Eisel-Lemire for the +1.48 cyc/B canada tax (334, 344). The type-plane greenfield override is
  charter-authorized and honestly flagged (102–104).

This is a genuinely well-built codex. The findings below are the residue.

---

## BLOCKING — three open design questions

### B1. The `@host` / hybrid-grammar-host surface is entirely orphaned.

SK-V22 GW5 = "type resolution **+ @host** on skinny's grammar/ir" (`sk-v22/SPEC.md:84,611`).
DEFERRED-LEDGER **N13**: "`@host` declaration surface moves INTO TW5 with its consumer; the wider
closure surface is land-or-delete | **TW5 (@host) + TW6 (rest)** | Host-signature gate fires on
falsifier grammar (TW5); zero live host hatches not expressible as grammar (TW6)"
(`DEFERRED-LEDGER.md:73`). DEFERRED-LEDGER §6 further binds "HostSigTable + `@host` surface land in
the SAME wave with a falsifier grammar" (line 120).

The SK-V23 codex contains **zero** occurrences of `@host`, HostSigTable, host-signature, or the
land-or-delete closure surface (`grep -niE '@host|host sig|hostsig|grammarcall|closure'` returns
only unrelated TB2 "port-in CLOSURE" and PT-R3 "Hazard closure" hits). TB4.W1 — the type plane,
the natural home — carries the sk-v21 §6 order, `TypeDesc`, and the width patch, but not `@host`
(SPEC-DRAFT:302–307, confirmed by read). The hybrid-grammar-host precept (memory:
hybrid-grammar-host — "host functions for context-dependent/recursive; strong typing") makes this
load-bearing, not cosmetic: without a home for `@host`, the type plane cannot type host signatures
and the N13 TW6 gate ("zero live host hatches not expressible as grammar") has no owner. **This is
an open design question:** does the greenfield type plane subsume `@host`, carry it, or does the
codex delete the closure surface? The codex must decide and name the wave + falsifier gate.

### B2. DEFERRED-LEDGER continuity is not asserted under the GW→TB re-lettering; the REDRESS rejected-route registry is absent.

SK-V22 carried an explicit continuity clause across its GW re-lettering: "Ledger continuity: every
DEFERRED-LEDGER row's wave coordinate re-maps by this table; no row is orphaned; the
empty-chronic-ledger assertion (DEFERRED-LEDGER §7) stands under the new letters"
(`sk-v22/SPEC.md:87–88`). SK-V23 performs a **second** re-lettering (GW→TB) and provides **no
equivalent**. Exactly one DEFERRED-LEDGER row is explicitly re-homed in the whole draft — the
channel-ratio row at TB2 (SPEC-DRAFT:252). The ~40 other rows (C1–C6, N1–N27, the §6 discoveries)
are left to implicit inference through the GW-map, which is now one indirection removed.

More sharply, DEFERRED-LEDGER **§0** entered four REDRESS ids (51/53/246/247) as **permanent
rejected routes**: "any future proposal matching a family below must cite this entry and name why
its coupling differs, **or it does not open**" (`DEFERRED-LEDGER.md:19`). REDRESS-53 = "a
structural index retained as a parallel vector alongside the ONE tape — a second substrate by
another name." SK-V23 introduces four **net-new greenfield** subsystems and a **new structural
index** (TB5.W3, below) — precisely the "future proposals" §0 exists to gate — yet never references
the registry. **The open question:** does the empty-chronic-ledger assertion still hold under TB?
The codex must carry a continuity statement (every C/N/REDRESS row → TB coordinate, no orphan) or
the assertion is unproven.

### B3. TB3.W2's one-regex-home close gate binds a campaign-gating bbnf wave to non-compellable parse-that actions, with a hidden deferral and no fallback.

TB3 blocks the entire back half of the bbnf campaign (TB2→TB3→{TB4∥TB5}→TB6). TB3.W2's close
requires "skinny's `parse-that-regex` fork (3,022 LOC) **+ local recognizer crate delete
post-fold**" (SPEC-DRAFT:278, 284). The vendor mirror + fork deletion bbnf can do **unilaterally**
(copy parse-that's engine at a pin). But the **recognizer-crate deletion** is conditional on
PT-R3.W1 folding "skinny's 322-LOC recognizer tier upstream as the engine's fast front"
(SPEC-DRAFT:512) — a parse-that action the non-clobber law makes **non-compellable** (§4.0:
review-not-execute, proposals only). If parse-that never folds the recognizer, bbnf's vendored
engine lacks it and the local crate **cannot** delete — so "delete **post-fold**" silently gates a
bbnf deletion on an external voluntary act, violating no-silent-deferrals. The codex specifies **no
bbnf-side fallback**. It must either (a) decouple the unilateral one-regex-home (vendor + fork
delete) from the recognizer-upstream-fold and make the latter an explicit conditional/deferred
item, or (b) retain the recognizer as a bbnf-owned patch-on-vendor — which breaks the byte-identity
gate. As written, the campaign's spine depends on an act it cannot force.

---

## MODERATE — interlock and right-sizing

### M1. The SK-V21 "TW4.1 lands before TW3 closes" ordering law is dropped inside {TB4 ∥ TB5}.

SK-V21 **split TW4 specifically** to sequence the decode coupling: DEFERRED-LEDGER §6, "TW3↔TW4
decode coupling ... TW4 split: **TW4.1 lands before TW3 closes**; coupling carried openly | CLOSED
(manifest order)" (`DEFERRED-LEDGER.md:114`). SK-V23 puts Eisel-Lemire (TW4.1) at **TB5.W0** and
the value plane (TW3) at **TB4.W2**, with **TB4 ∥ TB5** and only "TB4 optional-parallel (the value
plane furnishes `materialize_f64`)" (SPEC-DRAFT:341) to note the coupling. The two greenfields both
author `materialize_f64` concurrently. The ordering law that SK-V21 paid a tranche-split to encode
is now a parenthetical. Wave-ownable (add "TB5.W0 lands before TB4.W2 closes" or name a shared
`materialize_f64` seam owner), but as written it reintroduces the exact hazard the split dissolved.

### M2. PT-R3.W0 is mis-homed: its tranche entry-dep contradicts edge-1's temporal clamp.

PT-R3 entry deps = "**PT-R2** (the SOTA engine is the fold target) AND bbnf TB1.W1"
(SPEC-DRAFT:504, confirmed by read). Yet PT-R3.**W0** (the patch-table repoint) is "**Temporally
clamped inside bbnf TB1's window: GW0.b → PT-R.5 → GW0.c**" (SPEC-DRAFT:508–510) — i.e. it must run
during TB1, before TB1.W2, which is far upstream of TB3.W2/PT-R2. The interlock diagram
(SPEC-DRAFT:590) draws PT-R1→PT-R2→PT-R3 as a linear chain, placing PT-R3 (containing W0) **after**
PT-R2. Read literally, TB1.W2 (crates/ removal — which gates the whole bbnf campaign) is
transitively blocked by PT-R1→PT-R2→PT-R3.W0, a three-tranche parse-that serialization. The prose
escape (the repoint is a "one-minute edit to a gitignored file," owner-executed, decoupled) is
correct — but the tranche STRUCTURE contradicts it. Sever PT-R3.W0 into a standalone coordination
action (deps: TB1.W1 + the letter only), or the entry-dep must explicitly except W0 from the PT-R2
gate. The graph is acyclic; this is a declaration inconsistency, not a cycle.

### M3. x86 allowlist-emptiness is a one-time TB2 checkpoint, not a standing invariant.

C.2 (the load-bearing gate) "asserts zero hits **outside a const `ALLOWLIST`**" (SPEC-DRAFT:152).
"TB2.W2 empties the allowlist ... post-TB2 the allowlist holds only the gate file's self-reference"
(SPEC-DRAFT:167–168). But nothing makes allowlist-emptiness a **standing** gate re-checked every
wave — a later tranche can silently re-grow the const array and re-open the x86 channel the gate
exists to close. "Regrowth-proof" (charter axis 1) requires the meta-invariant: post-TB2, assert
`ALLOWLIST == {self-reference}` as a hard clause in **every** subsequent wave's close, not just at
TB2. The planted-phantom discipline proves the gate *fires*; it does not prevent *widening the
allowlist*. This is the "who guards the guard" hole.

### M4. C.2 coverage over the vendored engine is undefined; axis-1 vs the byte-identity vendor mirror can conflict.

The fold-back makes bbnf consume parse-that's regex engine as a **byte-identical** vendor mirror at
`crates-infra/bbnf-regex` (TB3.W2). Parse-that's x86 abrogation is only a **proposal** (PT-R1.W2,
non-compellable). If parse-that's engine retains or regrows an x86 arm (`portable_simd`, an SSE
number path), then a byte-identical sync pulls it into bbnf's tree. Does C.2 walk `crates-infra/`?
The gate scans "git ls-files over src + generated + emitters" (SPEC-DRAFT:148) — unspecified for
vendored mirrors. Two outcomes, both unhandled: (a) C.2 **does** scan it → the byte-identical sync
**fails** the x86 gate (vendor cannot be byte-identical AND x86-clean); (b) C.2 **excludes** the
vendor → an x86 regrowth channel opens straight through the mirror. The codex resolves neither. The
axis-1 enforcement and the vendor-mirror mechanism need an explicit reconciliation (e.g. the pin is
only advanced when the upstream engine is x86-clean, gate-verified at sync).

### M5. TB5 (and TB4) are overloaded by the six-tranche cap, without the schedule-risk acknowledgement SK-V22 gave GW0.

TB5 carries GW3 (Eisel-Lemire) **+** the GW4/TW4.2 decision-engine half **+** the entire net-new
SIMD-modernization axis (charter axis 2: vshrn movemask, multi-block ILP, padded tails, bulk
unescape, multi-quote vectorization) **+** two speculative research kernels (TB5.W3 CSS
tokenize-once, explicitly unknown-outcome; TB5.W2 P1 bulk-unescape). Five waves, several net-new
and risky — this exceeds the C3 "3 is the measured integration-quality optimum" precept
(SPEC-DRAFT:70). TB4 similarly fuses the two largest forward greenfields (value + type, ~2,400 LOC
net-new) plus the feature-fold wave. The value/kernel cleave is sound and the six-tranche cap
(research/08, ratified) genuinely forces the concentration — but SK-V22 explicitly booked GW0's
concentration as "a knowingly accepted schedule risk" (`CONVERGENCE-LEDGER.md:87,143`). SK-V23
books nothing. Either name TB4/TB5 as the known-heavy tranches with declared risk, or push the
two speculative kernels (CSS index, bulk-unescape) to the post-codex shopwindow, keeping TB5 to
settled kernel work.

---

## LOW — mention

- **L1.** TB5.W3's "CSS tokenize-once structural index" (SPEC-DRAFT:363–367) is dangerously
  adjacent to the **twice-buried structural-bitmap pre-scan**, DEFERRED-LEDGER **N20**: "RESOLVED
  NEGATIVE ... DELETE decided; a **CONSUMED** stage1→stage2 index (different design, never refuted)
  may return only via the §5 cost table with a measured win" (`DEFERRED-LEDGER.md:85`), and to
  REDRESS-53. The codex runs it as a research sub-wave with a falsifier corpus + negative-result
  exit — the right discipline — but never **cites N20/REDRESS-53** nor **asserts the index is
  consumed (stage1→stage2), not retained (a second substrate)**, the exact distinction separating
  the open door from the rejected route. Add the citation and the consumed-not-retained assertion.
- **L2.** Lock-15 + target-cpu profile config (TW0 W0.4 → GW1) is only partially carried: TB2.W2
  strips x86 target-cpu **strings** from bench, but the graduated-workspace profile config / Lock-15
  falsifier is not re-homed. Likely subsumed by the aarch64-only posture; confirm and note.
- **L3.** GW0.a's specific content — the graduation declaration as commit 0 and the `sk-v19-frozen`
  tag-semantics inversion — is implicit in TB1.W0 but not stated; folds naturally into the B2 fix.
- **L4.** The GW0.e tag-read authoring mechanism (`git show <tag>:path` over the emptied tree,
  `sk-v22/SPEC.md:209–212`) is not restated at TB1.W4; the docs are distilled post-removal, so the
  mechanism should carry.

---

## Why 80

Nothing here reopens a frozen SK-V21/22 architecture decision, and the codex's spine — archival,
x86 census + four-layer enforcement, greenfield delineation, the GW-split ratifications — is
sound and mostly complete. But convergence is capped by **three open design questions** (B1 `@host`
has no home; B2 ledger continuity + the rejected-route registry are unasserted under TB; B3 the
campaign spine binds to a non-compellable parse-that fold with no fallback) plus **five moderate
interlock/right-sizing gaps** (M1 dropped decode-ordering, M2 mis-homed PT-R3.W0, M3 non-standing
allowlist, M4 vendor-vs-x86-gate conflict, M5 unacknowledged TB5/TB4 overload). Per the charter's
100 = "zero open design questions, every residual wave-owned," B1–B3 are design decisions the codex
still owes, not execution residue. 80.
