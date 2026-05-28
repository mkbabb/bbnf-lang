# SK-V15 S-P3 V2 CH5 HIDDEN COUPLING

Pass: S-P3 Synthesis-Plan CHALLENGE. Cycle: V2. Lens: CH5.
Date: 2026-05-28.
HEAD: `39e186ee3`.
Scope: audit the committed S-P3 V2 packet for hidden broadcast admission,
gate self-exemption, EventTape sidecar relapse, FNV production migration,
Track 1 / Track 2 coupling, and generic Decision/lowerer coupling.

## Verdict

REVISE.

The V2 packet fixes the main V1 hidden-coupling defects: the W8R CSS tuple is
diagnostic-only, gate reports must be consumed, EventTape is constrained to
the five-shape BackendShape canon, FNV is bench-only, and Decision/lowerer
work has non-JSON and same-wave consumer gates. One blocker remains: the full
CH5 forbidden vocabulary is still not load-bearing in the final SPEC and
DISPATCH surfaces. P3-C and P3-E carry the richer bans, but SPEC/DISPATCH are
the wave dispatch surfaces and omit several CH5 terms called out by the pass
contract.

## Findings

| id | status | finding | evidence | required follow-up |
|---|---|---|---|---|
| CH5-V2-01 | REVISE | SPEC/DISPATCH still do not carry the full hidden-coupling vocabulary. PASS-3 CH5 asks whether SPEC exit gates forbid parser-owned structural projection, retained cursor, aux density table, and sidecar event vector. P3-C/P3-E cover density tables, aux projection tables, parser-owned structural cursors, Track 1 == Track 2 coupling, and EventTape sidecar bans, but SPEC Section 15 narrows the REDRESS 50-55/96-98 block to retained sidecar tables, cursor streams, class columns, public `UnionTape`, and second tape; DISPATCH only names the EventTape sidecar subset. | PASS-3 CH5 criterion: `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:134-138`; P3-C forbidden set: `restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md:313-328`; P3-E broader hidden-coupling ledger: `restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md:20-24`, `:45-55`, `:213-225`; SPEC narrower gates: `restart/skinny/tranches/sk-v15/SPEC.md:202-209`, `:414-431`; DISPATCH EventTape-only wording: `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md:182-200`. | Fold the full CH5 set into SPEC non-negotiables, SPEC global gates, SPEC pre-block table, DISPATCH pre-dispatch checks, DISPATCH same-wave mandate, and affected W2/W3/W7-W10 envelopes: parser-owned structural projection, retained cursor/list, aux density/projection table, sidecar event vector, parallel source pass, second tape, public `UnionTape`, retained class/structural/cursor streams, Track 1 == Track 2 sidecar, new substrate API, new/sixth BackendShape, alternate document projection, production FNV arbiter, and production hash correctness proof. |
| CH5-V2-02 | ACCEPT | Hidden broadcast admission is blocked. The packet treats the W8R CSS tuple as a diagnostic negative fixture, rejects one-to-N measurement stamps, and requires fresh typed CSS measurements before CSS live admission. | P3-A excludes W8R metrics from live floors (`p3a-candidate-shortlist.md:16`, `:29`, `:40`, `:57`); P3-C bans W8R floors and clone rows (`p3c-falsifiability-gates.md:27-38`, `:111-126`, `:197-212`); P3-D defines `measurement_row_id`, `measurement_origin`, and `broadcast_group_id` rejection rules (`p3d-telemetry-schema.md:44-55`, `:57-81`); SPEC rejects hidden one-to-N measurement stamps (`SPEC.md:119-122`, `:223-226`, `:314-329`); DISPATCH rejects hidden broadcast measurements (`DISPATCH-PROMPT.md:137-147`, `:248-250`). | Preserve. |
| CH5-V2-03 | ACCEPT | Gate-exclusion self-exemption is materially blocked, though CH5-V2-01 still requires vocabulary duplication in DISPATCH. P3-C defines the exclusion schema and rejects silent allowlists; SPEC requires exclusions, self-scan status, primitive status, gate consumer, affected rows, and disposition; DISPATCH requires the W2 plan to name the schema and redress to prove gates consume their own reports. | P3-C schema and self-exemption ban: `p3c-falsifiability-gates.md:71-89`, `:128-144`; P3-D rejects `gate_exclusion_report=self-exempting:*` (`p3d-telemetry-schema.md:54`, `:65`, `:94`); SPEC gate wording: `SPEC.md:119-122`, `:202-209`, `:247-263`; DISPATCH W2 gate: `DISPATCH-PROMPT.md:94-102`. | Preserve, and include the exact schema columns in DISPATCH while folding CH5-V2-01. |
| CH5-V2-04 | ACCEPT | EventTape relapse is blocked in the active packet. W9 owns EventTape only as one of the five BackendShape lowerers and explicitly forbids sidecar vector, sixth shape, retained stream, public substrate API, or alternate projection. | P3-B W9 row: `p3b-wave-sequencing.md:59-60`; P3-C W9 gate: `p3c-falsifiability-gates.md:254-273`; P3-E W9 pre-block: `p3e-preblocked-ledger.md:213-225`; SPEC global/EventTape gates: `SPEC.md:202-209`, `:363-378`; DISPATCH W9 envelope: `DISPATCH-PROMPT.md:182-200`. | Preserve. |
| CH5-V2-05 | ACCEPT | FNV production migration is blocked. W10 is quarantine-only, requires production scans and adversarial semantic fixtures, and forbids FNV as runtime selector, arbiter, or correctness proof. | P3-B W10 row: `p3b-wave-sequencing.md:61`; P3-C W10 gate: `p3c-falsifiability-gates.md:275-291`; P3-E FNV pre-block: `p3e-preblocked-ledger.md:227-243`, `:297`; SPEC W10 gate: `SPEC.md:380-394`; DISPATCH W10 envelope: `DISPATCH-PROMPT.md:202-215`. | Preserve, and add `production hash correctness proof` to the full CH5 vocabulary fold. |
| CH5-V2-06 | ACCEPT | Generic Decision/lowerer coupling is bounded by wave sequencing and same-wave consumers. W7 owns only the Decision spine, W8/W9 split lowerers, generic facts cannot be `json_*`/`css_*`, and lowerer fixtures must fail against the old scaffold. | P3-B split: `p3b-wave-sequencing.md:57-60`; P3-C W7-W9 consumers: `p3c-falsifiability-gates.md:214-273`; SPEC W7-W9 gates: `SPEC.md:331-378`; DISPATCH required consumers: `DISPATCH-PROMPT.md:149-200`. | Preserve. |

## Required Follow-Up

Before S-P3 V2 can lock, fold CH5-V2-01 into the final dispatch surfaces. This
is a text-only hardening fold, not an implementation wave: no source redress is
authorized by this CH5 file.

## Verification

Commands run:

```sh
git rev-parse --short HEAD
rg -n "P3-B does not exist|PRUNE-WAVE|REBUILD-WAVE|W0-W9|W1-W9|Cycle: V1" restart/skinny/tranches/sk-v15/research/p3 restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
rg -n "2319\\.041|2362\\.037|929\\.281" restart/skinny/tranches/sk-v15/research/p3 restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md
rg -n "included roots|excluded roots|Reason|Owner|Self-scan status|Primitive status|Gate consumer|Affected rows|Disposition|exclusion report schema|scan roots|consume their own exclusion" restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md
rg -n 'parser-owned structural projection|structural projection|aux density|density table|aux projection|projection table|parser-owned structural|cursor stream|retained cursor|sidecar event vector|parallel source pass|second tape|public `UnionTape`|public UnionTape|Track 1 == Track 2|production FNV|production hash|new substrate API|new BackendShape|sixth BackendShape' restart/skinny/tranches/sk-v15/SPEC.md restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md
```

Result: the first search only found stale references inside V1 hardening and
redeploy notes, not active S-P3 V2 packet files. The W8R numbers appear only
as diagnostic-negative language. The exclusion schema exists in P3-C/SPEC and
is consumed by DISPATCH wording. The final vocabulary search shows the
remaining blocker: several CH5 forbidden forms appear only in P3-C/P3-E, not
in SPEC/DISPATCH.
