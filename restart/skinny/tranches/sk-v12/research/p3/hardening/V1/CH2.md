# SK-V12 S-P3 CHALLENGE V1 - CH2 GENERALITY

Disposition: ACCEPT.

Lens: CH2 GENERALITY. PASS-3 requires every shortlisted candidate to carry the
S-P2 grammar-neutral verdict, and requires SPEC Section 2.1 to gate every
generic-crate edit on executable non-JSON proof rather than prose
(`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:112`;
`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:153`). Lock 14 forbids
grammar-specific behavior in generic crates and allows grammar variation only
through grammar source, metadata, optional per-grammar declarations, and
generated per-grammar runtime output (`restart/locks/LOCKS.md:78`).

## Findings

1. ACCEPT - the candidate shortlist carries the S-P2 grammar-neutral verdict
   surface for all eight candidates.

   P3-A first narrows the S-P2 pool through P2-F: six conditional
   parser/support families survive, while F7 is oracle-only and F8 is
   accounting-only/ineligible as a parser primitive
   (`restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md:44`;
   `restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md:48`;
   `restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:25`;
   `restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md:40`).
   C1-C3 are generated non-JSON baseline candidates and each requires generated
   Track 1 plus an independent oracle/Track 2, not a JSON row or stale witness
   (`restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md:66`;
   `restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md:88`;
   `restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md:102`;
   `restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md:116`;
   `restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md:130`;
   `restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md:142`).
   C4-C8 then carry explicit P2-F grammar-neutral verdicts for generated
   FIRST/prefix tables, byte-set masks, bounded string spans, digit spans, and
   escape/hex segments
   (`restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md:164`;
   `restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md:191`;
   `restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md:214`;
   `restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md:239`;
   `restart/skinny/tranches/sk-v12/research/p3/p3a-candidate-shortlist.md:266`).

2. ACCEPT - SPEC Section 2.1 is the required Lock 14 gate for generic edits.

   The SPEC makes the global close condition fail if a generic crate or shared
   runtime path learns JSON policy, and requires executable non-JSON proof for
   any generic edit (`restart/skinny/tranches/sk-v12/SPEC.md:58`). Section 2.1
   then defines the concrete gate: public API scan, grammar-branch scan,
   generated/caller-owned primitive policy, generated per-grammar runtime
   ownership, and selected CSS L4 / Sheets / BBNF-self compile/run/strict-oracle
   equality proof for generic codegen, runtime, parse-that, or bbnf-simd edits
   (`restart/skinny/tranches/sk-v12/SPEC.md:275`;
   `restart/skinny/tranches/sk-v12/SPEC.md:280`;
   `restart/skinny/tranches/sk-v12/SPEC.md:287`). The allowed surfaces are
   grammar input files, generated per-grammar output, per-grammar
   providers/templates, tests, fixtures, and host/API schema facts; generic code
   consumes grammar-derived facts and may not hide JSON policy under neutral
   names (`restart/skinny/tranches/sk-v12/SPEC.md:291`;
   `restart/skinny/tranches/sk-v12/SPEC.md:294`).

3. ACCEPT - executable non-JSON evidence is required; prose, report-only, and
   stale witness shortcuts are rejected.

   W1 admits exactly one generated non-JSON direct or typed baseline, in CSS L4,
   Sheets, then BBNF-self order, and the plan must name generated Track 1,
   generated runtime path, fixture corpus, independent oracle/Track 2, strict
   equality command, gate command, and rollback slice
   (`restart/skinny/tranches/sk-v12/SPEC.md:379`;
   `restart/skinny/tranches/sk-v12/SPEC.md:383`). Its exit gate requires
   generated Track 1, independent oracle/Track 2 evidence, positive Mbps, strict
   equality, full Section 0.4 gate consumption, selected generated runtime
   compiled from grammar facts, and Lock 14/Section 2.1 pass
   (`restart/skinny/tranches/sk-v12/SPEC.md:399`;
   `restart/skinny/tranches/sk-v12/SPEC.md:408`). The same section explicitly
   pre-blocks REDRESS 111 report fixture as baseline, REDRESS 112/113
   future-phase promise, hand-only non-JSON parser, stale `sheets_witness`, JSON
   provider cloning under a neutral name, generic JSON policy, directive/BIR
   additions, and source-only baseline claims without measured Mbps
   (`restart/skinny/tranches/sk-v12/SPEC.md:413`;
   `restart/skinny/tranches/sk-v12/SPEC.md:416`).

4. ACCEPT - W2 preserves generality by consuming the W1 row, not by admitting an
   orphan primitive or JSON-only helper.

   W2 can dispatch only after W1 records `W1_baseline_track1_mbps`; it must
   select one S-P2-surviving intervention tied to that selected baseline hot
   leaf and include scalar reference, microbench, parity/checkasm when
   applicable, same-wave generated consumer, strict oracle equality, and guard
   floors (`restart/skinny/tranches/sk-v12/SPEC.md:442`;
   `restart/skinny/tranches/sk-v12/SPEC.md:447`). Its tasks keep grammar policy
   in generated code or caller-owned policy, and its exit gate requires the same
   generated consumer, gate-consumed telemetry, and Lock 14/Section 2.1 pass
   (`restart/skinny/tranches/sk-v12/SPEC.md:454`;
   `restart/skinny/tranches/sk-v12/SPEC.md:456`;
   `restart/skinny/tranches/sk-v12/SPEC.md:462`;
   `restart/skinny/tranches/sk-v12/SPEC.md:471`).

5. ACCEPT - telemetry and pre-block ledgers fail JSON policy leaks closed.

   P3-D requires non-JSON rows to use generated Track 1 and generated runtime
   paths, rejects `json` grammar ids for the generated non-JSON close axis, and
   rejects hand-only parsers or stale witness modules as generated Track 1
   (`restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:123`;
   `restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:130`;
   `restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:207`;
   `restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:215`).
   The same companion gate fails on JSON policy in generic crates or runtime
   outside generated per-grammar modules
   (`restart/skinny/tranches/sk-v12/research/p3/p3d-telemetry-schema.md:222`).
   P3-E carries generic JSON policy leaks as a hard pre-block, identifies
   `json_provider::ensure_runtime_profile` as the current blocker, and allows
   generic helpers only as grammar-neutral byte/string/digit/hex mechanics with
   grammar policy in generated per-grammar modules
   (`restart/skinny/tranches/sk-v12/research/p3/p3e-preblocked-ledger.md:68`).

6. ACCEPT - JSON-only shortcuts are fenced as conditional or pre-blocked, not
   generalization evidence.

   P3-B sequences W1 generated non-JSON baseline before any primitive or JSON
   companion work, and W2 consumes the W1 row
   (`restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:70`;
   `restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:77`;
   `restart/skinny/tranches/sk-v12/research/p3/p3b-wave-sequencing.md:80`).
   W3 is late and conditional: it can reopen a JSON direct residual only after
   W1/W2 resolve the non-JSON priority and only with fresh material evidence
   beyond REDRESS 114-119 (`restart/skinny/tranches/sk-v12/SPEC.md:499`;
   `restart/skinny/tranches/sk-v12/SPEC.md:503`;
   `restart/skinny/tranches/sk-v12/SPEC.md:517`). The dispatch prompt preserves
   the same boundary: no generated non-JSON close by prose/report fixture/hand
   parser/stale witness, no JSON direct work before W1/W2 resolves, no
   generic-crate JSON policy, no W3 substrate route, and no parse_only admission
   (`restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:144`;
   `restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:146`;
   `restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:150`;
   `restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:151`;
   `restart/skinny/tranches/sk-v12/DISPATCH-PROMPT.md:152`).

## Fold Revisions

None required for CH2. Preserve the V1 packet's current Lock 14 structure:
generated non-JSON baseline first; same-row generated intervention second;
generic edits gated by executable CSS L4 / Sheets / BBNF-self proof; grammar
policy confined to generated per-grammar modules or caller-owned metadata; and
JSON direct residuals fenced until the non-JSON priority admits or records a
measured block.
