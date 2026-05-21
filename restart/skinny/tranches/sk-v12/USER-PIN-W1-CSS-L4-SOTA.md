# USER PIN — SK-V12 W1 (2026-05-20)

Authority: user directive `2026-05-20`, reproduced verbatim.

> We MUST push all numbers to be >SOTA. CSS is the authoritative, not
> sheets — whatever that takes for semantic parity with lightningcss —
> we've trivially beaten this before, we can again. In short, SIMD
> utilization must be perfected, Parse time / >SOTA is an absolute top
> priority, and the ASM gen + Rust union facilities unblocked.

This pin amends `restart/skinny/tranches/sk-v12/SPEC.md` and
`restart/skinny/tranches/sk-v12/HANDOFF.md` at the clauses named below.
It does NOT open SK-V13; SK-V12 is the campaign that delivers the
pinned outcome.

> **Addendum 2026-05-21** — see
> `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md`. The
> addendum extends this pin campaign-wide (SK-V13 → SK-V14 → ...),
> raises the bar to FULL lightningcss parity + every JSON path
> > sonic-rs strict, re-pins `parse_only` as admission-eligible, lifts
> the REDRESS-119 fixpoint, and binds the campaign indefatigably.

## Directives

### D1. CSS L4 authoritative

W1 admits the **CSS L4 generated baseline**. Sheets and BBNF-self
become fallbacks ONLY after a CSS L4 *redress attempt* fails — not
after preflight failure. The W1 V2 selection of Sheets (commit
`e24a7e01`) is OBSOLETED by this pin. The bar is semantic parity with
the lightningcss CSS L4 surface on the selected corpus.

Supersedes: SPEC §0.5 W1 target-candidate ordering treating CSS / Sheets
/ BBNF-self as preflight-equivalent; HANDOFF §3 priority 1 wording.

### D2. Close target raised to >SOTA

SK-V12 closes when the generated CSS L4 parser **beats lightningcss**
on parse throughput at the same corpus, same output plane, with strict
equality. The prior bar `>= ceil(baseline_mbps * 1.01)` is rescinded.
The new admission floor on the CSS L4 row is `lightningcss_mbps + 1`.

Supersedes: SPEC §0.1 close-condition clause 4; SPEC §5 (W2) exit-gate
floor formula; HANDOFF §3 priority 2.

### D3. Rust union substrate UNBLOCKED at architectural-category level

SPEC §1, §6 (W3), §8 pre-blocks on the architectural category
"union / event-model / class-column / streaming cursor / class-lane /
sidecar substrate / `UnionTape` / retained structural vector /
W4-through-W3 cascade / parser-owned structural projection / retained
structural cursor or cursor list / aux density table / aux projection
column / event side vector / whitespace bitmap / retained class lane /
structural-position vector / decoded-byte sidecar" are RESCINDED at the
category level.

REDRESS 96 / 97 / 98 remain on file as measured-rejected
implementations of specific union variants (V1 class-column, V2
streaming cursor, V3 class-lane). New implementations attempting the
same architectural goal MAY dispatch under the standard scalar
reference + parity/checkasm + same-wave consumer gate (§2.2). A new
implementation must cite the prior REDRESS, name the material
differential, and pass CHALLENGE.

### D4. ASM gen UNBLOCKED at category level

SPEC §8 specific-REDRESS pre-blocks bound to:

- REDRESS 88 — PMULL prefix-XOR default body
- REDRESS 89 — CSSC CTZ bulk consumer
- REDRESS 90 — canary hardening as row movement

are RESCINDED at the category level. Specific measured-rejected
implementations remain historical. New consumers wiring these
primitives may dispatch under §2.2 micro-prove + scalar reference +
parity/checkasm + same-wave consumer.

### D5. SIMD utilization perfected

The 5 orphan aarch64 primitives surfaced in
`restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md`
(`bitmap_prefix_xor_64`, `bitmap_next_set_bit`,
`bulk_emit_positions_64`, `byte_context`, `cache_hints`) are
wave-eligible if a same-commit consumer wires them per Lock 16 / §2.2.
The campaign target is zero orphan kernels by SK-V12 close.

### D6. Parse time / >SOTA absolute top priority

All numbered priorities in SPEC §0.1 and HANDOFF §3 rebind:

1. Generated CSS L4 parser > lightningcss on the admitted row.
2. JSON guard floors (SPEC §0.5) hold; demotion requires measured gate
   disposition.
3. JSON direct residual reopen rule (REDRESS 119) unchanged.
4. `parse_only` remains diagnostic-only.

## Carried (unchanged by this pin)

- x86 implementation work remains OUT OF SCOPE.
- `parse_only` remains diagnostic-only — no SOTA admission from it.
- JSON direct + typed guard floors (SPEC §0.5) hold; demotion requires
  measured gate disposition.
- Strict-vs-strict comparator discipline.
- Lock 14 grammar-neutrality + §2.1 generic-crate exit gate.
- Lock 16 admission gate (scalar reference + checkasm + same-wave
  consumer).
- §2.2 micro-prove-first discipline.
- The seven Lock-14 leaks surfaced in
  `skv12-value-api-audit.md` must be resolved by W1's `GrammarConfig`
  surface before CSS L4 emission is legal.
- The `escape_mask_64` NEON correctness bug (xorshift falsifier
  `0xCAFEF00DBAADF00D`) surfaced in `skv12-totality-fold-scout.md` §2
  must be verified and resolved before any new SIMD admission.

## Disposition of REDRESS-blocked routes

| REDRESS | Subject | Pin disposition |
|---|---|---|
| 88 | PMULL prefix-XOR default body | UNBLOCKED at category; measured-rejected implementation remains historical |
| 89 | CSSC CTZ bulk consumer | UNBLOCKED at category |
| 90 | canary hardening as row movement | UNBLOCKED at category |
| 96 | W3 union event-model V1 (class-column substrate) | UNBLOCKED at category; V1 implementation remains historical |
| 97 | W3 union event-model V2 (streaming cursor) | UNBLOCKED at category; V2 implementation remains historical |
| 98 | W3 union event-model V3 (class-lane-only) | UNBLOCKED at category; V3 rejection at CHALLENGE remains historical |
| 111 | non-JSON report lane as generated baseline | UNCHANGED — still requires generated Track 1 |
| 112 / 113 | generated non-JSON baseline blocker / intervention block | SUPERSEDED — CSS L4 is the explicit mandate |
| 114 – 119 | JSON direct fixpoint rows | UNCHANGED — `>lightningcss` is on the CSS L4 plane; JSON direct rows remain guard-only |
| 120 | SK-V11 close | UNCHANGED |

## Status of running W1 CHALLENGE V2

The in-flight CHALLENGE evaluating the W1 V2 plan (Sheets baseline,
commit `e24a7e01`) is evaluating an OBSOLETED plan. Its disposition is
historical only and does not lift the W1 dispatch block. The next move
is one of:

1. **Steer-on-disk**: this pin is on-disk before the CHALLENGE commits
   its disposition. If the CHALLENGE re-reads HANDOFF.md or its inputs
   before disposing, it may incorporate the pin; otherwise the
   disposition is recorded as obsolete.
2. **Stop + redispatch**: the user stops the running CHALLENGE process;
   S-P3 re-derives the W1 plan under this pin; CHALLENGE V3 evaluates
   the new plan.
3. **Let CHALLENGE finish + redispatch**: the V2 CHALLENGE finishes
   blind to the pin; its disposition is annotated OBSOLETED-BY-USER-PIN
   in `skv12-W1-challenge.md`; S-P3 then re-derives W1 plan V3.

## Required follow-up

- S-P3 re-converges the W1 plan under the pin (CSS L4 target;
  lightningcss-beating bar; union + ASM-gen waves admissible).
- SPEC §1 / §6 / §8 amended to record the category-level unblocks.
- HANDOFF §3 / §7 rebound to the CSS L4 first-target and the
  lightningcss admission floor.
- `GrammarConfig` trait + per-grammar config extraction lands inside
  the W1 redress so CSS L4 emission is legal (per
  `skv12-value-api-audit.md` §5).
- The `escape_mask_64` NEON correctness bug is verified and resolved.
