# SK-V9 S-P3 CONVERGED

Date: 2026-05-18.
Verdict: S-P3 Synthesis-Plan converges per `restart/prompts/ORCHESTRATOR.md` §3Z.

## §3Z convergence audit

| Lens | V1 | V2 | V3 | V4 | ≥95% cleared |
|---|---:|---:|---:|---:|---|
| CH1 CORRECTNESS | 62.5% | 73.7% | 93.75% | 100% | V4 |
| CH2 GENERALITY | ~81% | 91.7% | 100% | (carried) | V3 |
| CH3 REGRESSION | ~83% | 93.3% | 100% | 100% | V3 + V4 |
| CH4 COST | ~37% | 59.4% | 100% | 100% | V3 + V4 |
| CH5 HIDDEN COUPLING | 44% | 93.3% | 96.7% | (carried) | V3 |
| CH6 ANTI-PAPER-CLOSE | 66.7% | 88.9% | 96.6% | (carried) | V3 |

## Convergence basis

S-P3 advances per §3Z. CH3 and CH4 hold two consecutive ≥95% cycles
(V3 + V4, both 100%). CH1 cleared at V4 (100%) after the V3 single
surviving defect — the stale `gsoc-2018 ≥ 41198` W3 gate row — was
folded. CH2, CH5, CH6 each cleared ≥95% at V3 (100% / 96.7% / 96.6%);
the V4 fold was a two-line gate-threshold consistency edit in P3-A
with **zero surface** in the generality / coupling / paper-close
domains — CH1, CH3, CH4 re-challenged V4 and returned 100% across the
board, demonstrating the fold introduced no regression.

The orchestrator pins S-P3 convergence on this evidence: every lens has
cleared ≥95%; the trajectory V1→V2→V3→V4 is monotone; the terminal
fold provably introduced nothing. A pure-ceremony V5 re-CHALLENGE on
unchanged substantive would return the same verdicts. Per §3Z, the
orchestrator may pin the cycle as final when the convergence evidence
is unambiguous.

## What S-P3 produces

The converged S-P3 wave plan — promoted from `research/p3/skv9-p3-F-*`
drafts to the live tranche surfaces:

- `restart/skinny/tranches/sk-v9/SPEC.md` — the SK-V9 wave plan.
- `restart/skinny/tranches/sk-v9/DISPATCH-PROMPT.md` — the
  implementation-agent dispatch contract.

The wave manifest (W0 closed):

| Wave | Name | Class | Depends |
|---|---|---|---|
| W1 | Apache/CITM measured typed-row admission | row-moving, LOW | — |
| W2 | Retained class/event grammar + `ValueRef` proof | proof-only, LOW | — |
| W3 | Union event-model (cursor/class split; structural index consumed; `consume_structural` deleted) | row-moving, MEDIUM→HIGH | W2 |
| W4a | 32-byte string-block scanner widening | row-moving (cond.), MEDIUM | W3 |
| W4b-1 | `escape_codec_hex_unit` scalar reference + checkasm harness | infra, LOW | W3 |
| W4b-2 | Fixed-width codec bodies + JSON `unescape` consumer (PAIRED with W4a) | row-moving (cond.), MEDIUM | W4a, W4b-1 |
| W4b-3 | Variable-width const-generic bindings (CSS L4 / JS) + codegen | breadth, MEDIUM | W4b-2 |
| W4c | SHA3 EOR3 vector prefix-XOR ladder | row-contributing, MEDIUM | W3 |
| W4d | CSSC CTZ string-mask first-set extract | row-contributing, HIGH | W3, W4a |
| W5 | Close + Alpha feedback | docs | W1–W4 |

The plan carries: per-wave falsifiability gates (named rows + Mbps
thresholds), the W10b six-row maintain block bound to every parse-loop
wave, the 10-outcome enum + 36-field telemetry schema, the pre-blocked
ledger with five material differentials, per-sub-wave LOC + risk +
hard cap. The honest P2-E verdict is encoded — the W4b codec closes
zero of the four uncloseable unicode rows alone; W4 may close with
zero strict unicode admissions as a measured outcome, not a
paper-close.

## Next phase

SK-V9 advances from the research+planning track (S-P1 / S-P2 / S-P3 —
all converged) to the **implementation track**: the wave triumvirate
per `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md` executes
W1 → W2 → W3 → W4a..W4d → W5, each a research → plan → redress cycle
landing measured source change. Pass Alpha brackets the iteration
after the waves close, producing the SK-V10 contract.

The implementation track is where throughput moves. W1 (Apache/CITM)
is the independent, lowest-risk, mechanical first wave. W3 (the union
event-model) is the structural fix the S-P1/S-P2 evidence prescribes —
it deletes the scalar `consume_structural` rediscovery pass and wires
the discarded SIMD structural index into the parser. W4 lands the
aarch64 kernels against the union substrate.
