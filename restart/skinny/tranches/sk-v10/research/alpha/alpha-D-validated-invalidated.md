# Alpha-D Validated And Invalidated Ledger For SK-V9 -> SK-V10

Date: 2026-05-19.

Role: PASS-ALPHA alpha-D ledger. This artifact identifies the facts that carry
forward into SK-V10 and the hypotheses that must be retired.

## Validated

| Item | Evidence | SK-V10 implication |
|---|---|---|
| Typed measured-row admission | REDRESS 94; Apache/CITM rows in `skinny/RESULTS.md:10`, `:15` | Product-plane row-table work is the primary SOTA path. |
| Retained grammar proof | REDRESS 95; W2 proof docs | The proof remains a correctness constraint, but it is not a row-moving gate. |
| Typed row-table provenance | W1 required same-run run id, comparator plane, Track 1/Track 2/oracle parity | Future typed rows need the same gate, not source-only evidence; bbnf strictness remains deferred until the gate changes. |
| REDRESS honesty | W3 V1/V2 patches were saved and reverted after measurement | Failed waves can close as measured rejects without corrupting the next tranche. |

## Invalidated

| Item | Evidence | SK-V10 implication |
|---|---|---|
| Substrate-ceiling / union event-model thesis | REDRESS 96/97 missed every W3 and W10b floor | No W3 gate in SK-V10. |
| Class-lane-only fallback as W3 redress | W3 CHALLENGE V4 rejected it before source | Do not paper-close by proving a non-row-moving retained class. |
| W4 cascade-lock | REDRESS 98 retires W3 | W4 can return only as existing-substrate work. |
| Parse-only SOTA scoreboard | All parse rows remain `S / NO-GO` | Parse-only raw wins are planning signals, not close conditions. |

## Lock Amendment Candidate

Pass Omega should receive the substrate-ceiling falsification as a lock
amendment. SK-V7 kernel work, SK-V8 W3, and SK-V9 W3 all implemented
profile-derived structural/substrate hypotheses that measured as regressions.
Future skinny cycles should not dispatch structural/substrate rewrites from
profile evidence alone. They need a same-host micro proof and a live output
plane before S-P3 wave scoping.

## Demoted

- W4a string-block widening: demoted from W3 consumer to existing
  `match_string_at_quote_trusted_utf8` candidate.
- W4b unicode codec: demoted from union-substrate codec route to current
  `unescape_four_unicode_escapes` / sink-call-site candidate.
- W4d CTZ: demoted to a possible successor only after a live existing-substrate
  W4a caller exists.
- W4c EOR3: retired with W3 unless a future Alpha/S-P3 plan names a non-union
  prefix-XOR consumer.

## Still Open

- Direct-specific diagnosis for the 14 `N-direct / NO-GO` rows. This is the
  primary JSON frontier for SK-V10.
- Typed schema admission for `instruments`.
- Root-type typed schema generalization for `github_events` and `gsoc-2018`.
- Full-fixture Canada typed proof, still blocked by prior decimal-coordinate
  parity failures.
- Direct output/control-path contract, still separate from typed product proof.
- Same-run C++ sidecar manifest, gate-only unless downstream S-P3 creates a
  measured comparator gate.

## Alpha Verdict

SK-V10 should stop trying to repair the parse-only substrate plane. The primary
JSON frontier is `direct_to_struct`; the validated SOTA surface is measured
typed direct product output; the invalidated path is moving the SIMD structural
index into retained parsing.
