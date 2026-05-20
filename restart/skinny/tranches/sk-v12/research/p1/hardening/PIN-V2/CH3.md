# SK-V12 S-P1 PIN-V2 CH3 - Regression / REDRESS

Verdict: ACCEPT
Score: 95%

## Blocking Findings

None.

## Nonblocking Notes

1. Parse-only status remains fail-closed. The user pin and folded handoff both
   keep `parse_only` diagnostic-only: no parse row can supply SOTA admission
   (`USER-PIN-W1-CSS-L4-SOTA.md:80-95`; `HANDOFF.md:30-68`). P1-A, P1-C,
   P1-E, and P1-F repeat the same boundary, and P1-F reports the live surface as
   16 `S / NO-GO` plus 1 `L / NO-GO` parse diagnostic, not row movement
   (`p1f-results-delta.md:80-93`, `:198-205`).

2. Union and ASM-gen are unblocked only as campaign categories, not as
   implementation waivers. The pin preserves REDRESS 88/89/90 and 96/97/98 as
   historical measured entries while requiring REDRESS citation, material
   differential, scalar/parity or checkasm evidence, same-wave consumer, and
   CHALLENGE for new attempts (`USER-PIN-W1-CSS-L4-SOTA.md:39-69`,
   `:108-121`). The folded S-P1 docs carry the same rule (`p1c-samply-mode-3.md:83-87`,
   `p1e-hot-leaf-attribution.md:186-193`; `HANDOFF.md:66-68`, `:126-128`).

3. Historical REDRESS entries are preserved rather than rewritten. REDRESS 88
   rejects PMULL prefix-XOR default-body admission, REDRESS 89 rejects the CSSC
   CTZ/bulk consumer, and REDRESS 90 admits only the canary hardening slice while
   keeping the bitmap bodies rejected (`skinny/REDRESS.md:2510-2540`,
   `:2544-2585`, `:2589-2618`). REDRESS 96/97/98 still record the measured
   union attempts and gate retirement (`skinny/REDRESS.md:2852-2906`,
   `:2910-2925`). The pin changes future dispatch eligibility only.

4. No S-P1 artifact scopes a behavior intervention without proof. The revised
   profile docs record JSON PMU/xctrace/samply and CSS L4 absence only; P1-C
   explicitly says W0 diagnostics do not become wave authority and a union or
   ASM-gen candidate still needs material differential, micro-prove-first
   evidence, scalar/parity or checkasm coverage, same-wave consumer, and
   CHALLENGE (`p1c-samply-mode-3.md:74-87`). P1-E says it proposes no
   intervention and routes later work through the same REDRESS boundaries
   (`p1e-hot-leaf-attribution.md:180-193`).

5. JSON guard floors and result deltas are reported honestly. P1-F states
   `skinny/RESULTS.md` and `skinny/REDRESS.md` have no diff from SK-V11 close,
   that every SK-V12-open JSON delta is unchanged, and that JSON rows cannot
   populate the CSS L4 `lightningcss_mbps + 1` close bar
   (`p1f-results-delta.md:68-99`, `:145-169`, `:171-210`). REDRESS 119/120
   remain the direct residual/fixpoint authority, with no direct row, W0-clamped
   row, generated runtime, benchmark body, gate semantic, or RESULTS movement
   admitted (`skinny/REDRESS.md:3495-3553`).

6. Generated-size and O(N) routing no longer bypass REDRESS. The PIN-V1 fold
   routed generated CSS runtime size, generated LOC, module byte size,
   regen/check command, and O(N) grammar-size guard into the handoff
   (`PIN-V1/CONSOLIDATED.md:29-35`). The current handoff requires those fields
   before W1b redress and includes generated LOC, module byte size, O(N)
   grammar-size status, JSON guard state, gate status, wave id, and REDRESS id
   in the CSS L4 telemetry contract (`HANDOFF.md:121-123`, `:144-155`). That
   makes size/growth evidence part of the same fail-closed gate surface rather
   than a route around REDRESS.

## Exact Fold Edits If REVISE

None; ACCEPT.
