# SK-V9 S-P3 V1 CHALLENGE consolidation

Date: 2026-05-18.
Cycle: V1.
Inputs: `restart/skinny/tranches/sk-v9/research/p3/hardening/V1/CH{1..6}.md`.

## Verdict — V2 fold required (one root cause across five lenses)

| Lens | ACCEPT-rate | Verdict |
|---|---:|---|
| CH1 CORRECTNESS | 62.5% (30A / 13 REVISE / 5 REJECT) | REVISE |
| CH2 GENERALITY | ~81% (26A / 5 NOTE / 1 REVISE) | ACCEPT-WITH-NOTE |
| CH3 REGRESSION | ~83% (24 HOLD / 2 REVISE / 5 DEFECT) | REVISE |
| CH4 COST | ~37% (12A / 13 REVISE / 7 REJECT) | REJECT |
| CH5 HIDDEN COUPLING | 44% (12A / 9 REVISE / 6 REJECT) | REVISE |
| CH6 ANTI-PAPER-CLOSE | 66.7% (24 PASS / 5 NEAR / 7 FAIL) | REVISE |

## The single root cause

Five of six lenses fail for **one reason**: P3-F (`skv9-p3-F-spec-draft.md`
+ `skv9-p3-F-dispatch-draft.md`) was drafted from S-P2 evidence alone,
before the P3-A..E siblings landed, and its `[INTEGRATE P3-x]` markers
were never resolved. The P3-A..E content is **sound and accepted**:

- CH3: "P3-E's pre-block ledger content is correct and accepted ... all
  five material differentials cross-checked verbatim ... hold."
- CH5: "The W3 union event-model is coupling-clean ... the P2-A
  architecture stands."
- CH2: "The S-P3 wave plan respects Lock 14."
- CH1: "The citation layer is strong ... citation accuracy is high."
- CH4: "The P3-A LOC envelopes are themselves realistic and
  well-sourced — the defect is in P3-F's carry-through."

The failure is the unintegrated SPEC. Concretely:

1. **Three irreconcilable wave manifests** — P3-B (W0-W5), P3-C (W0-W5,
   different bundling), P3-F (W0-W6). The codec, the 32-byte
   string-block widening, and the ASM kernels each land in a different
   wave number in each artefact.
2. **The codec/string-block split is a REDRESS-82 reopen** — P3-F's SPEC
   §8 manifests a standalone codec wave. P2-E §6.4 + P3-C §4.3 establish
   the codec and string-block widening MUST be paired (neither closes
   the four uncloseable rows alone); CH5 shows the split ships "exactly
   the parser-owned-helper owner paths" — the REDRESS-82 shape.
3. **Stale outcome enum** — P3-F SPEC §0.3 names 7 outcomes; P3-D
   established `validate_w0_outcome` admits 10 (`A C G I J K L M
   N-direct S`). Verified accurate by CH1.
4. **Wrong telemetry field count** — P3-F §0.4 names 31; P3-D's
   canonical count is 36. P3-F also adds three columns P3-D forbids.
5. **Stale `canada` sonic-strict floor** — P3-A/C/F cite `canada ≥
   15871`; live RESULTS sonic-strict for canada parse_only is 12723
   (SK-V8-era carryover; CH1 #23).
6. **CH5 cascade-lock ambiguity** — "same-wave" is overloaded across
   three distinct relations. P2-D §0 "the wave may not be split" must
   be disambiguated: it means a P2-D kernel must not land WITHOUT the
   union substrate existing — satisfied by W3 (union) preceding the
   kernel waves, NOT by forcing one monolithic wave.
7. **CH4 wave-too-big** — a monolithic codec+string-block+ASM wave is
   ~1,595-1,860 LOC, exceeding a 75-min redress cap. Resolved by
   sub-wave structure (W4a/W4b/… per SK-V6/V7 precedent): each sub-wave
   is a fresh triumvirate that wires its kernel into the already-landed
   W3 union — the kernel's consumer (the union) exists, the caller is
   wired same-commit, so no orphan.

## V2 fold — the integration

### F-MAIN — re-author the P3-F SPEC + DISPATCH-PROMPT drafts

Integrate P3-A..E into one coherent pair. Resolve every
`[INTEGRATE P3-x]` marker. Specifically:
- ONE unified wave manifest. Recommended shape:
  - W1 Apache/CITM measured-row admission (P2-C; independent; first).
  - W2 retained class/event grammar + `ValueRef` proof (P2-B;
    proof-only; unlocks W3).
  - W3 union event-model (P2-A + the P2-D §5 dead-scanner structural-
    bitmap chain folded in per P3-A C3; deletes `consume_structural`).
  - W4 the substrate consumers — sub-waved W4a (32-byte string-block
    widening), W4b (`escape_codec_hex_unit` codec — PAIRED with W4a,
    same triumvirate or strictly adjacent), W4c (SHA3 EOR3 prefix-XOR),
    W4d (CSSC CTZ string-mask consumer). Each W4 sub-wave wires its
    kernel into the W3 union; each carries the W10b six-row maintain
    gate; the codec+string-block pairing is preserved (W4a+W4b are not
    separable — the codec alone closes zero rows).
  - W5 close + Alpha feedback.
- The 10-outcome enum + 36-field telemetry schema per P3-D.
- The corrected `canada` sonic-strict floor (live, not 15871).
- Per-wave LOC + risk + hard cap reconciled with P3-A's envelopes;
  the sub-wave structure resolves the 75-min redress ceiling.
- The cascade-lock disambiguated explicitly (per item 6 above).
- Per-wave pre-blocked routes from P3-E's ledger verbatim.
- The honest P2-E verdict (codec closes zero of four rows alone;
  conditional same-wave-pairing rule) carried into the W4 gate.

### F-AUX — surgical touch-ups to P3-B / P3-C / P3-A

- P3-B: add the hard-cap column its own §2 scope mandates.
- P3-C: extend the W4 maintain envelope to the 3 direct-GO rows
  (citm_catalog, marine_ik, unicode_basic); name the 7 GO rows
  explicitly in the W3 envelope.
- P3-A: record an explicit disposition for C6/C7 (CH6 flagged they
  were silently dropped from P3-F — they belong in W4c/W4d).

## V2 path

1. Dispatch one F-MAIN agent (re-author P3-F drafts) + one F-AUX agent
   (P3-B/C/A surgical) in parallel.
2. Commit `docs(sk-v9-p3-v2): integrate P3-A..E into the SPEC drafts`.
3. Re-dispatch CHALLENGE V2 (all six lenses).
4. Expected: all six clear ≥95% — the underlying P3-A..E content is
   already accepted; V2 is an integration pass, not a re-research.

## Convergence forecast

S-P3 V1's failure is narrow and well-understood: an unintegrated draft.
V2 is a single integration fold. The S-P3 trajectory should be
V1 (fail) → V2 (clear) → V3 (second-consecutive confirm) — three
cycles, faster than S-P1's V3-V6 or S-P2's V1-V4, because nothing in
P3-A..E needs rework.
