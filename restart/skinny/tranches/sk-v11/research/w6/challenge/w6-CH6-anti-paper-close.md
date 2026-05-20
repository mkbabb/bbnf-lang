# SK-V11 W6 CH6 - Anti-Paper-Close

Pass: W6 Phase 2.5 CHALLENGE.
Lens: CH6 anti-paper-close / next-tranche impact.
Date: 2026-05-20.
Output: this file only.
Source edits: none.

## Disposition

REVISE.

The plan is close to an admissible W6 shape because it does not claim an x4
production win, does not edit `runtime/src/grammars/json/sink.rs`, selects a
floor-bearing direct row, and names a direct-output consumer rather than the
already-consuming `unescape_string` path. It still cannot enter redress as
written because two anti-paper-close bindings are missing: measured row
movement attributable to the W6 delta, and `SKINNY-TRIUMVIRATE.md` Section 8
same-wave consumer evidence in the sampled hot path.

## Findings

### 1. The gate is measurable, but the plan can still close on a threshold

`unicode_mixed/direct_to_struct` is a legitimate W6 candidate, and the floor is
concrete: Track 1 and Track 2 must both clear 2588 Mbps. The paper-close risk is
that Track 1 already clears the floor at SK-V11-open, while Track 2 is short by
only 161 Mbps. The current exit gate requires post-patch Criterion values above
the floor, but it does not require a before/after movement packet proving that
the W6 source delta, not host noise or run drift, moved Track 2 across the gate.

Required change:

- Add a movement-evidence sub-gate before `RESULTS.md` may change. The REDRESS
  packet must include pre-patch or same-binary legacy-control measurements,
  post-patch measurements, Mbps delta, run ids, flags, sample counts, and the
  selected source delta for `unicode_mixed` Track 1 and Track 2. A W6 admit
  requires Track 2 to cross the 2588 floor with margin and show positive
  movement attributable to the escaped-segment digest fold. Floor crossing alone
  is not enough.

### 2. The same-wave consumer proof is underspecified

The plan names `JsonDigestSink::{key_source,string_source,array_string_source,object_string_source}`
overrides and a hand Track 2 escaped-string digest path. That is the right
consumer class, but the exit gate only asks gate/report to name the consumer.
`SKINNY-TRIUMVIRATE.md` Section 8 requires the consumer call to show in the
`samply` symbol path on affected rows. Without that, W6 could close by changing
bench-local code and reporting a row number while never proving that the new
escaped fold replaced the hot `unescape_string -> decoded String/Cow -> hash`
route.

Required change:

- Add a required `samply --save-only` or equivalent saved `samply` packet for
  `unicode_mixed` Track 1 and Track 2 after the patch. The REDRESS entry must
  cite the symbol path showing the new direct digest source-method consumer and
  the independent Track 2 escaped-string digest path on the affected row. If the
  new helper is fully inlined, the plan must name the visible caller frame that
  proves the same route. If the sampled path still shows `unescape_string` as
  the selected-row escaped-string materializer, W6 rejects.

### 3. The Track 2 independence decision is still deferred

The plan says CHALLENGE must decide whether a shared local helper in
`direct_struct.rs` is an acceptable output-plane scalar oracle. CHALLENGE is the
decision point; the plan cannot enter redress with that left as a future-phase
promise. Exact Track 1/Track 2 digest equality is not enough if both tracks call
the same new folding helper and can share the same bug.

Required change:

- Choose the Track 2 independence rule in the plan before redress. Either use
  separate generated Track 1 and hand Track 2 escaped-fold implementations, or
  add a non-shared oracle path that verifies decoded bytes and digest fields
  against serde/sonic fixture outputs without calling the generated sink helper.
  The REDRESS entry must state which independence rule was used.

### 4. x4 language is mostly safe, but must be excluded from PASS evidence

The plan correctly says the default route does not route x4 and treats the
existing x4 path as background proof only. That is acceptable only if the exit
gate prevents REDRESS 107/108 from being cited as production evidence. W6 cannot
admit from x4 parity, the W8 micro-proof, a cosmetic wrapper, a feature re-gate,
or the already-wired `unescape_string -> unescape_four_unicode_escapes ->
unescape_uxxxx_x4_neon` path.

Required change:

- Add a negative x4 clause to the exit gate: W6 PASS evidence is limited to the
  new escaped-segment digest consumer, row movement, direct/typed guard floors,
  parity, and sampled consumer proof. REDRESS 107/108, x4 checkasm, and x4
  micro-proof evidence may appear only as background risk context unless the
  whole plan is revised through CHALLENGE to route x4 with a new scalar oracle,
  strict checkasm, and a same-wave product consumer.

## Required Plan Revisions

Before redress, revise `w6-plan-escaped-segment-digest-fold.md` to add:

1. A movement-evidence sub-gate requiring before/after or same-binary legacy
   control measurements for `unicode_mixed` Track 1 and Track 2.
2. A `samply` same-wave consumer proof for the new direct digest source-method
   overrides and the hand Track 2 escaped digest path.
3. A resolved Track 2 independence rule; no "CHALLENGE must decide" language
   may remain.
4. A negative x4/pass-evidence clause excluding REDRESS 107/108 and existing
   x4 proof from W6 admission.
5. A REDRESS template line stating that `skinny/RESULTS.md` moves only when the
   selected row clears the floor, shows attributable movement, preserves all
   guards, and has sampled same-wave consumer evidence.

With those revisions, CH6 can accept the W6 plan. Without them, W6 risks a
paper close: a threshold-only admit, a named-but-unsampled consumer, or an x4
proof replay dressed as production.
