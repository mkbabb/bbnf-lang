# SK-V12 Pass Alpha CHALLENGE V2 - CH2 Generality / Lock 14

Date: 2026-05-20.
Lens: CH2 generality / Lock 14.
Scope: Pass Alpha SK-V11 -> SK-V12 alpha E/F and revised SK-V12
SYNTHESIS/HANDOFF after commit `18f4b931`, checked against V1 CH2 and the V1
consolidated fold requirements.

## Disposition

ACCEPT.

V1 CH2 blocked on two gaps: fallback Sheets/BBNF-self baselines had no matching
intervention path, and the known JSON-profiled codegen blocker was not yet an
executable selected-grammar baseline pre-gate. The current packet folds both
gaps. It also preserves the existing CH2 refusals: no JSON-policy leak into
generic crates, no W1a/witness/prose-only generality claim, no parse-only SOTA
movement, and no JSON direct retry before the non-JSON priority resolves.

## Sources Read

- `restart/skinny/tranches/sk-v12/research/alpha-hardening/V1/CH2.md`.
- `restart/skinny/tranches/sk-v12/research/alpha-hardening/V1/CONSOLIDATED.md`.
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`.
- `restart/skinny/tranches/sk-v12/HANDOFF.md`.
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md`.
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md`.
- `git show 18f4b931 --` for the revised alpha E/F, SYNTHESIS, and HANDOFF
  fold.
- Targeted cross-checks against current alpha A-D, SK-V11 close-redress, and
  `skinny/REDRESS.md` through REDRESS 120 for W1a/W1b/W2 authority.

## Verification Matrix

| Check | Result | Evidence |
|---|---|---|
| Selected-grammar executable baseline pre-gate | ACCEPT | V1 required generated emission or generated per-grammar runtime, build, same-plane oracle, equality smoke, and non-JSON gate consumption (`V1/CH2.md:71-97`; `V1/CONSOLIDATED.md:25-29`). Alpha-E now requires exactly one selected grammar to prove generated emission or per-grammar runtime path, avoid JSON-only `json_provider::ensure_runtime_profile`, run a fixture corpus and independent oracle, pass compile/equality smoke, and show REDRESS 111 gate consumption without producer-only telemetry (`alpha-E-candidate-shortlist.md:53-68`). Alpha-F and SYNTHESIS carry the same pre-gate before redress (`alpha-F-contract-draft.md:71-83`; `SYNTHESIS.md:35-46`; `HANDOFF.md:50-55`). |
| Sheets/BBNF-self intervention variants | ACCEPT | V1 required a concrete second-stage intervention path for whichever baseline admits (`V1/CH2.md:46-69`, `:111-118`). Alpha-E changed E4 from CSS-only to selected-baseline intervention, with owner paths for CSS L4, Sheets, and BBNF-self runtimes, explicit Sheets and BBNF-self fallback variants, and a threshold keyed to `W1_selected_baseline_mbps` (`alpha-E-candidate-shortlist.md:225-284`). Alpha-F and SYNTHESIS now state that the intervention follows CSS L4, Sheets, or BBNF-self, whichever baseline row admits (`alpha-F-contract-draft.md:84-91`; `SYNTHESIS.md:47-53`, `:172-179`; `HANDOFF.md:56-60`). |
| No JSON-policy leakage | ACCEPT | The baseline must have no JSON policy leak into generic crates or runtime outside generated per-grammar modules (`SYNTHESIS.md:35-46`; `alpha-F-contract-draft.md:71-83`). The gate fails missing fields, producer-only telemetry, oracle coupling, and JSON policy leakage (`SYNTHESIS.md:219-223`; `alpha-F-contract-draft.md:179-180`). Alpha-E blocks JSON-provider emission as generality proof for the pre-gate, baseline candidates, and intervention (`alpha-E-candidate-shortlist.md:58-65`, `:119-124`, `:220-223`, `:281-284`). JSON direct work remains conditional after the non-JSON priority and cannot become the Lock 14 proof surface. |
| No W1a/witness/prose-only generality | ACCEPT | The revised packet keeps W1a as a non-admitting report lane only (`HANDOFF.md:66-71`; `alpha-F-contract-draft.md:38-42`). Alpha-E says W1a is schema consumption, not baseline authority, and forbids the CSS oracle from using `sheets_witness` or benchmark-private parser code (`alpha-E-candidate-shortlist.md:90-101`, `:119-123`). Sheets explicitly rejects `sheets_witness` inventory and witness-only admission (`alpha-E-candidate-shortlist.md:143-153`, `:170-173`). BBNF-self requires generated Track 1 and an independent oracle rather than prose (`alpha-E-candidate-shortlist.md:177-181`, `:193-200`). Alpha-F and SYNTHESIS refuse prose, hand-only parser code, stale witness modules, producer-only telemetry, and parse-only admissions (`alpha-F-contract-draft.md:182-186`, `:217-226`; `SYNTHESIS.md:59-72`, `:257-266`). |

## Findings

### CH2-1 - ACCEPT: V1 fallback-intervention gap is folded

The current E4 is no longer CSS-only. It is explicitly a selected-baseline
intervention that may consume an admitted CSS L4, Sheets, or BBNF-self baseline.
It binds the selected row, independent oracle, equality, same-wave gate
consumer, and threshold `ceil(W1_selected_baseline_mbps * 1.01)`. This satisfies
the V1 requirement that S-P3 not invent a fallback intervention outside the
Alpha shortlist if E2 or E3 admits first.

### CH2-2 - ACCEPT: V1 executable pre-gate gap is folded

Alpha-E, Alpha-F, SYNTHESIS, and HANDOFF now put the known W1b blocker in the
entry shape: the selected grammar must prove generated emission or a generated
per-grammar runtime path, module build, runnable fixture/oracle, equality smoke,
and REDRESS 111 gate consumption before behavior redress. If that cannot pass,
the packet requires a generator/runtime unblock split or a measured `BLOCKED`
route. That is an executable fail-closed shape, not a prose Lock 14 claim.

### CH2-3 - ACCEPT: JSON policy remains quarantined

The revised packet allows JSON work only as a later conditional companion after
the non-JSON priority succeeds or honestly blocks. It does not route JSON role
policy into generic crates, does not treat `json_provider` as generality proof,
and makes JSON-policy leakage a gate/refusal failure.

### CH2-4 - ACCEPT: report-only and witness-only paths fail closed

The packet continues to demote W1a to infrastructure only, rejects
`sheets_witness` as generated Track 1, requires a generated BBNF-self Track 1
plus independent oracle, and refuses grammar generalization by prose,
hand-only parser code, stale witnesses, parse-only rows, or producer-only
telemetry.

## CH2 Verdict

ACCEPT. The revised SK-V12 Alpha packet now meets CH2 / Lock 14 for the
selected-grammar pre-gate, fallback intervention variants, JSON-policy
containment, and non-prose generality proof shape.

Changed path:

- `restart/skinny/tranches/sk-v12/research/alpha-hardening/V2/CH2.md`
