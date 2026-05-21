# SK-V12 W1b-2 A6 - Outcomes And Fixpoint

Date: 2026-05-20.
Phase: W1b-2 research.
Scope: W1b-2 dispositions and downstream routing.

## Outcome Meanings

- `PASS-ADMIT-CANDIDATE`: W1b-2 has a same-plane CSS row with strict equality
  across generated Track 1, cssparser oracle, and lightningcss; telemetry and
  JSON guards are gate-consumed; and
  `track1_mbps > lightningcss_mbps + 1`. This satisfies the user-pin CSS
  admission surface, but SK-V12 close still waits for W4 disposition,
  zero-orphan state, guard disposition, and W5 reconciliation.
- `PASS-MEASURED-BASELINE`: W1b-2 has a strict-equal, measurable CSS row, but
  it does not beat `lightningcss_mbps + 1`. This is not ADMIT. It is the
  measured CSS evidence needed to continue into W3/W4 and, if no later ADMIT
  lands, to support FIXPOINT.
- `BLOCKED/FAIL`: comparator, equality, oracle independence, generated-size,
  throughput, or gate consumption failed. Record REDRESS. This unlocks fallback
  consideration only if it is a measured W1b-2 CSS lightningcss
  comparator/admission attempt.

## Fallback Legality

Sheets/BBNF-self remain illegal before W1b-2 records a measured CSS L4
lightningcss comparator/admission redress. W1b-1 does not satisfy this
condition because it admits only the generated CSS scaffold.

After a measured W1b-2 miss, Sheets fallback is legal only through a subsequent
S-P3 or wave-plan revision, not as an automatic same-wave pivot.

## W3 / W4 Routing

After any measured W1b-2 CSS row:

- W3 may dispatch only with CHALLENGE acceptance, a fresh material differential
  from REDRESS 96/97/98, and a same-tape single-substrate CSS-local union plan.
- W4 routes after W1b-2 close, W2 close, and CHALLENGE. It must micro-prove
  first, provide scalar/checkasm/parity, wire a same-wave CSS or JSON-guard
  consumer, and dispose the aarch64 orphan set.

Use rejected patch path:

```text
/tmp/skv12-waveW1b-2-rejected.patch
```

The next REDRESS entry is `REDRESS-124`.
