# SK-V12 S-P2 PIN-V4 CH5 Hidden Coupling / Substrate Challenge

Pass: S-P2 Research CHALLENGE. Cycle: PIN-V4.
Date: 2026-05-20.
Scope: Hidden-coupling review of the unchanged folded Cycle V3 S-P2 packet after commit `b407583e`, research fold `75233b2b`, with emphasis on Lock 1 substrate cardinality, USER PIN D3 category unblocking, Track 1 / Track 2 independence, fact-stream ownership, and REDRESS 96/97/98 boundaries.
Output: this file.

Verdict: ACCEPT.
Score: 97%.

## Blocking Findings

None.

## Review

1. The one-retained-substrate boundary is explicit and intact. P2-D states that live retained parsing owns a single offset tape: `Tape` owns source, one offset stream, sparse flag vectors, payload arena, and tape id; `ValueRef` remains `&Tape + cursor`; `ParserState` owns one `TapeBuilder` sealed into `JsonRoot` (`p2d-substrate-tape.md:11`-`:19`). The current `StructuralIndex` is not retained beside the tape, generated JSON `attach_structural_index` remains a no-op, and any future CSS structural facts must be the tape or generated same-tape metadata, not a parallel side vector (`p2d-substrate-tape.md:21`-`:30`). This matches Lock 1's rule that SIMD mask streams are transient producers and retained structural projection is the tape (`restart/locks/LOCKS.md:52`).

2. USER PIN D3 is carried correctly as a category unblock, not a sidecar permission slip. D3 rescinds pre-blocks at the architectural-category level but keeps REDRESS 96/97/98 as measured-rejected implementations requiring material differential, scalar reference, parity/checkasm, same-wave consumer, and CHALLENGE (`USER-PIN-W1-CSS-L4-SOTA.md:39`-`:56`). P2-D mirrors that boundary: the pin changes REDRESS posture but not Lock 1, does not authorize a second retained substrate, and still treats sidecars/substrate splits as CH5 failures (`p2d-substrate-tape.md:32`-`:41`). P2-F likewise says REDRESS 96/97/98 remain historical failures even though the category is unblocked for materially different attempts (`p2f-grammar-neutral.md:24`, `:111`).

3. The packet rejects the banned sidecar/class-lane/cursor/vector shapes rather than laundering them. P2-D's table marks `parallel_structural_cursor_or_class_lane` as any retained structural-position vector, streaming cursor, whitespace bitmap, auxiliary density/projection column, decoded-byte sidecar, parser-owned structural projection, or `UnionTape`; its cardinality is +1 and it is "not a candidate" (`p2d-substrate-tape.md:112`). P2-F separately rejects `structural_class_lane_union / retained cursor / sidecar stream` unless a later Alpha/S-P3 contract proves a materially different same-tape formulation (`p2f-grammar-neutral.md:56`, `:107`). This preserves the D3 unblock while blocking the historical REDRESS 96/97/98 shapes.

4. Track 1 / Track 2 independence is not hidden-coupled. HANDOFF requires one canonical CSS fact stream shared by generated Track 1, independent Track 2/oracle, and lightningcss, with strict equality and telemetry fields for oracle path, lightningcss comparator, Track 1 Mbps, oracle Mbps, equality, and provenance (`HANDOFF.md:54`-`:58`, `:77`-`:83`, `:145`-`:156`). P2-A's `grammar_output_event_sink` requires a scalar generated output product plus independent oracle comparator and says digest/hash alone is not a parser primitive (`p2a-sota-teardown.md:36`). P2-E requires canonical CSS fact stream equality across generated Track 1, independent oracle/Track 2, and lightningcss, and rejects JSON direct digest as a reusable CSS policy (`p2e-parse-that-gaps.md:18`, `:41`-`:42`, `:68`). I found no `Track 1 == Track 2` shortcut or shared-source oracle claim.

5. Fact stream is row-owned output, not a hidden parser sidecar. P2-E says `pt_fact_event_emit` and `pt_fact_stream_digest` are admissible only as grammar-derived fact-stream primitives and are the CSS L4 output-plane bridge, not a JSON digest shortcut; Lock 1 holds only when fact events are the tape/projection or sink-only event stream, with no parser-owned sidecar or retained class vector (`p2e-parse-that-gaps.md:54`). P2-F defines tape/fact stream as emitting offsets, flags, direct sinks, output facts, and row-owned digests into the single substrate/output plane, and names retained structural sidecars, parser-owned cursors, decoded-byte caches, and digest-only proofs as drop triggers (`p2f-grammar-neutral.md:97`). Its CH4 supplement requires emitted offsets/facts to be the canonical output, not a side vector (`p2f-grammar-neutral.md:73`).

6. The category-unblock boundaries are consistent across P2-D and P2-F. P2-D keeps `css_fact_stream_same_tape_kind` as a conditional post-W1b aperture only after CSS generated Track 1, same-plane fact stream, lightningcss comparator, equality evidence, and CSS hot-leaf attribution exist; it is not shortlist-ready in this S-P2 cycle (`p2d-substrate-tape.md:54`-`:78`, `:102`-`:108`). P2-F collapses the current surface into candidate, template, support-only inventory, and drop classes; support/inventory rows cannot enter S-P3 without fresh P1 antecedent, scalar oracle, micro-proof, and same-wave consumer (`p2f-grammar-neutral.md:28`, `:61`-`:81`). This does not re-block D3; it prevents a historical or orphaned substrate shape from being admitted without the pin-required material differential.

## Nonblocking Notes

- The packet properly treats JSON P1 hot leaves as primitive nomination evidence only. P2-D and P2-E both state CSS L4 has no generated Track 1, same-plane lightningcss comparator, strict equality oracle, or CSS hot-leaf attribution yet (`p2d-substrate-tape.md:54`-`:66`; `p2e-parse-that-gaps.md:12`).
- The current fact-stream wording is sufficiently strict for CH5: the row output can be a CSS fact stream, but not a retained auxiliary vector, digest-only proof, or Track 2-coupled comparator shortcut.
- This review is scoped to the pinned `PIN-V*` hardening path and the folded Cycle V3 packet. Older unpinned `V*` hardening artifacts were not treated as current authority.

## Exact Fold Edits

N/A. ACCEPT.

## Commands Used

```sh
git status --short && git log --oneline -5
pgrep -fl 'cargo|rustc|xctrace|samply' || true
rg -n "CH5|hidden coupling|Lock 1|substrate|sidecar|parallel|USER PIN|D3|REDRESS 96|REDRESS 97|REDRESS 98|Track 1|Track 2|fact stream|fact-stream" restart/prompts/ORCHESTRATOR.md restart/prompts/skinny/PASS-2-RESEARCH.md restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md restart/skinny/tranches/sk-v12/HANDOFF.md skinny/REDRESS.md restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md
sed -n '1,220p' restart/skinny/tranches/sk-v12/research/p2/hardening/PIN-V3/CH5.md
sed -n '1,220p' restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md
sed -n '1,180p' restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md
sed -n '45,125p' restart/locks/LOCKS.md && sed -n '2780,2955p' skinny/REDRESS.md
nl -ba restart/prompts/ORCHESTRATOR.md | sed -n '70,122p'
nl -ba restart/prompts/skinny/PASS-2-RESEARCH.md | sed -n '220,245p'
nl -ba restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md | sed -n '35,95p'
nl -ba restart/skinny/tranches/sk-v12/HANDOFF.md | sed -n '50,175p'
nl -ba restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md | sed -n '25,85p'
nl -ba restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md | sed -n '25,75p'
nl -ba restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md | sed -n '45,105p'
rg -n "sidecar|parallel|class lane|class_lane|cursor|UnionTape|Track 1|Track 2|Track1|Track2|fact stream|fact_stream|row-owned|independent|same-tape|Lock 1|D3|REDRESS 96|REDRESS 97|REDRESS 98|category" restart/skinny/tranches/sk-v12/research/p2/*.md restart/prompts/ORCHESTRATOR.md restart/prompts/skinny/PASS-2-RESEARCH.md restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md restart/skinny/tranches/sk-v12/HANDOFF.md restart/locks/LOCKS.md skinny/REDRESS.md
git status --short && find restart/skinny/tranches/sk-v12/research/p2/hardening -maxdepth 2 -type f | sort | sed -n '1,160p'
git rev-parse --short HEAD && git show --stat --oneline --decorate -1 b407583e && git show --stat --oneline --decorate -1 75233b2b
nl -ba restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md | sed -n '1,130p'
nl -ba restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md | sed -n '1,125p'
nl -ba restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md | sed -n '1,75p'
mkdir -p restart/skinny/tranches/sk-v12/research/p2/hardening/PIN-V4
```
