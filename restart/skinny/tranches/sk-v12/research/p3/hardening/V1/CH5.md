# SK-V12 S-P3 CHALLENGE V1 - CH5 Hidden Coupling

Disposition: REVISE

## Lens

CH5 asks whether any wave introduces a parallel substrate, sidecar producer,
renamed scanner, or Track 1 == Track 2 dishonesty, and whether the SPEC exit
gates forbid parser-owned structural projections, retained cursors, aux density
tables, and sidecar event vectors
(`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:134`-`:138`). The
orchestrator applies the same lens to new BIR/substrate surfaces
(`restart/prompts/ORCHESTRATOR.md:87`,
`restart/prompts/ORCHESTRATOR.md:202`-`:203`). Lock 1 keeps tape as the single
retained substrate and treats SIMD masks as transient only
(`restart/locks/LOCKS.md:52`); Lock 14 forbids grammar-specific code in generic
crates and hand-written per-grammar runtime policy
(`restart/locks/LOCKS.md:78`).

## Findings

1. REVISE: SPEC Section 2.1 leaves a hidden host-schema/provider escape hatch.
   The packet correctly makes generated non-JSON Track 1 the first material
   target and requires independent Track 2/oracle evidence
   (`restart/skinny/tranches/sk-v12/SPEC.md:42`-`:64`,
   `restart/skinny/tranches/sk-v12/SPEC.md:399`-`:408`). It also blocks
   `sheets_witness`, hand-only parsers, and JSON-provider cloning
   (`restart/skinny/tranches/sk-v12/SPEC.md:413`-`:416`). However, Section 2.1
   currently allows "per-grammar providers/templates" and "host/API schema
   facts" as grammar-specific surfaces
   (`restart/skinny/tranches/sk-v12/SPEC.md:291`-`:294`), while W1 still names
   `json_provider.rs` or a "successor profile provider" as an owner surface
   (`restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:70`) and
   tasks W1 with breaking the JSON-provider-only blocker
   (`restart/skinny/tranches/sk-v12/SPEC.md:389`-`:397`). That wording can be
   read as permission for a hand-written per-grammar provider or hidden host
   schema to supply parser behavior outside generated Track 1. That conflicts
   with P2-F's accepted diagnosis that the current blocker is JSON-profiled
   runtime emission and that generic crates must not learn grammar policy
   (`restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:14`-`:19`,
   `restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:42`-`:53`).

   Exact fold revision:
   - In `restart/skinny/tranches/sk-v12/SPEC.md` Section 2.1, replace the allowed
     grammar-specific surface sentence with: "Allowed grammar-specific inputs are
     grammar source, workspace metadata, tests, fixtures, independent oracle
     code, and optional per-grammar declaration-crate host functions explicitly
     named by the W1 plan and gate-consumed. Templates are shared
     grammar-neutral generator code; per-grammar providers/templates must not
     carry handwritten parser policy. Host/API schema facts must be listed in the
     companion report with source path/checksum and cannot supply parser control,
     generated Track 1 output, or an admission shortcut."
   - In P3-B / DISPATCH W1 owner wording, replace "`json_provider.rs` or
     successor profile provider" with "`json_provider.rs` only to remove the
     JSON-only gate, or a grammar-neutral profile provider fed solely by grammar
     source/workspace metadata; no hand-written per-grammar provider is an
     admitted parser surface."

2. REVISE: the SPEC blocks sidecars broadly, but does not name the full CH5
   sidecar vocabulary at the exit-gate level. P2-D is explicit: structural side
   vectors, event side vectors, class lanes, `UnionTape`, parser-local structural
   cursors, whitespace bitmaps, aux projection columns, and second retained
   representations violate Lock 1
   (`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:53`-`:59`,
   `restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:124`-`:127`).
   The P3 packet carries much of this into P3-E
   (`restart/skinny/tranches/sk-v12/research/p3/p3e-preblocked-ledger.md:62`,
   `restart/skinny/tranches/sk-v12/research/p3/p3e-preblocked-ledger.md:124`-`:150`)
   and SPEC Section 8 blocks substrate surfaces, `UnionTape`, parser-owned
   cursor/facts, sidecar substrates, and parallel substrates
   (`restart/skinny/tranches/sk-v12/SPEC.md:589`-`:591`). The SPEC still omits
   the exact CH5 terms "aux density table" and "sidecar event vector" from its
   global non-negotiables and pre-blocks. Because REDRESS 50/51/53 and W3
   failures repeatedly reappeared under renamed scanner/projection phrasing,
   broad "sidecar substrate" language is not quite enough for CH5.

   Exact fold revision:
   - In `restart/skinny/tranches/sk-v12/SPEC.md` Section 1 and Section 8, expand
     the sidecar/substrate ban to explicitly include: "parser-owned structural
     projection, retained structural cursor or cursor list, aux density table,
     aux projection column, event side vector, whitespace bitmap, retained class
     lane, structural-position vector, decoded-byte sidecar, or renamed scanner
     that retains facts outside the single tape/direct sink contract."
   - Mirror that exact phrase in `p3e-preblocked-ledger.md`,
     `p3f-spec-draft.md`, and `DISPATCH-PROMPT.md` so W1-W3 plans cannot satisfy
     CH5 by citing the broader term while implementing a narrower side vector.

3. No CH5 revision is required for companion-report consumption or Track 1 /
   Track 2 independence. P3-D requires a same-wave companion gate if non-JSON
   rows are not rendered through `skinny/RESULTS.md` and `gate-json`
   (`restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:107`-`:165`).
   It also rejects generated Track 1 / Track 2 shared source, helper calls,
   generated runtime internals, generated SinkOnly helpers, benchmark digest
   shortcuts, stale placeholder schemas, hand-only parsers, and missing gate
   status
   (`restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:197`-`:223`).
   P3-C carries the same oracle-coupling rejection before redress
   (`restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:140`-`:155`,
   `restart/skinny/tranches/sk-v12/research/p3/p3c-falsifiability-gates.md:281`-`:294`).
   With Finding 1's host-schema provenance fold, those rules are strong enough.

4. No CH5 revision is required for W1 fallback order itself. P3-B keeps CSS L4,
   Sheets, and BBNF-self fallback inside W1 and requires any skipped earlier
   target to fail the same executable pre-gate, not to bypass generated Track 1
   (`restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:86`-`:91`).
   SPEC Section 4 requires the W1 plan to name generated Track 1, runtime module,
   fixture, independent oracle/Track 2, strict equality, gate command, and
   rollback slice (`restart/skinny/tranches/sk-v12/SPEC.md:376`-`:385`). Finding
   1's wording change is sufficient to prevent the fallback from becoming a
   hand-provider or hidden-schema bypass.

## Fold Revisions

Apply the two revisions above before V2:

1. Tighten SPEC Section 2.1 and the W1 owner wording so "provider",
   "template", and "host/API schema facts" cannot become hidden handwritten
   parser policy or a generated Track 1 bypass.
2. Expand the SPEC/P3-E/P3-F/dispatch pre-block wording to name aux density
   tables, sidecar event vectors, retained cursor lists, parser-owned structural
   projections, and renamed retained scanners explicitly.

After those folds, CH5 should be able to ACCEPT: the packet already preserves the
single substrate, keeps W3/substrate routes closed, requires generated Track 1
plus independent Track 2/oracle, and makes companion-report evidence executable
through a same-wave gate.
