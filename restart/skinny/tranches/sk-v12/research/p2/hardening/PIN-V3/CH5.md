# SK-V12 S-P2 PIN-V3 CH5 Hidden Coupling / Substrate Challenge

Pass: S-P2 Research CHALLENGE. Cycle: PIN-V3.
Date: 2026-05-20.
Scope: Hidden-coupling review of the folded Cycle V3 S-P2 packet after commit `75233b2b`, with emphasis on Lock 1 substrate cardinality, USER PIN D3 category unblocking, Track 1 / Track 2 independence, fact-stream ownership, and CH4 accounting boundaries.
Output: this file.

Verdict: ACCEPT.
Score: 96%.

## Blocking Findings

None.

## Review

1. The one-retained-substrate boundary holds. P2-D states that live retained parsing owns one offset tape and that `ValueRef` remains `&Tape + cursor`, while `ParserState` owns one `TapeBuilder` sealed into `JsonRoot` (`p2d-substrate-tape.md:11`-`:19`). It also records that the current `StructuralIndex` is not retained beside the tape and that any future CSS structural facts must be tape-owned or generated same-tape metadata, not a parallel side vector (`p2d-substrate-tape.md:21`-`:30`). This matches Lock 1's rule that a SIMD mask stream is transient and retained structural projection is the tape (`restart/locks/LOCKS.md:52`).

2. USER PIN D3 is preserved as a category unblock, not as a blanket sidecar allowance. P2-D explicitly distinguishes the pin's category-level rescission from the still-binding Lock 1 shape rule (`p2d-substrate-tape.md:32`-`:41`) and keeps REDRESS 96/97/98 as historical falsifiers requiring a material differential in consumer, row, and fact ownership (`p2d-substrate-tape.md:43`-`:52`). The candidate table rejects `parallel_structural_cursor_or_class_lane` as any retained structural-position vector, streaming cursor, whitespace bitmap, aux density/projection column, decoded-byte sidecar, parser-owned structural projection, or `UnionTape` (`p2d-substrate-tape.md:112`). That is exactly the D3 boundary: new union attempts may dispatch only when materially different and CHALLENGE-approved; the historical class-column/streaming-cursor/class-lane variants remain measured failures (`USER-PIN-W1-CSS-L4-SOTA.md:39`-`:56`, `skinny/REDRESS.md:2795`-`:2950`).

3. The packet does not couple Track 1 and Track 2. HANDOFF requires a single canonical CSS fact stream shared by generated Track 1, independent Track 2/oracle, and lightningcss (`HANDOFF.md:54`-`:58`), and P2-A's `grammar_output_event_sink` candidate requires a scalar generated output product plus independent oracle comparator, with digest/hash alone barred from parser admission (`p2a-sota-teardown.md:36`, `:53`, `:62`). P2-F repeats that a CSS digest/fact-stream may be used only if the same report carries independent oracle/lightningcss equality (`p2f-grammar-neutral.md:51`) and that output digest/typed/direct projection may be row-owned oracle evidence but not parser primitive proof (`p2f-grammar-neutral.md:113`). I found no `Track 1 == Track 2` shortcut, shared-source oracle claim, or comparator laundering.

4. Fact stream is row-owned output, not a hidden parser sidecar. P2-D's only plausible material differential is a same-tape CSS fact stream consumed by equality/visitor/comparator paths, not JSON parse-only delimiter replacement (`p2d-substrate-tape.md:68`-`:78`). P2-E says `pt_fact_event_emit` and `pt_fact_stream_digest` are legal only as grammar-derived fact-stream primitives: they are the CSS L4 output-plane bridge, not a JSON digest shortcut, and Lock 1 holds only when fact events are the tape/projection or sink-only event stream (`p2e-parse-that-gaps.md:54`-`:58`). P2-F's CH4 supplement similarly requires emitted offsets/facts to be the canonical output in the existing tape/fact stream, not a side vector (`p2f-grammar-neutral.md:73`).

5. CH4 accounting text preserves the category-unblock boundaries. P2-B's process gates require scalar oracle, checkasm/differential proof, Lock 16 provenance, micro-proof, same-wave consumer, strict comparator, and Lock 1/14/16 close before admission (`p2b-dav1d-process.md:29`-`:37`). The row accounting table marks PMULL prefix-XOR, CSSC next-bit, bulk emit, byte context, cache hints, digest/hash, and other support surfaces as explicit N/A or support-only unless a named same-wave consumer exists (`p2b-dav1d-process.md:50`-`:57`). P2-F's CH4 supplement keeps the same boundary for tape/fact-stream, bitmap support, output digest/oracle, diagnostic tape policies, and rejected/inventory shapes (`p2f-grammar-neutral.md:61`-`:81`). This does not re-block D3/D4 at category level; it prevents old measured-rejected implementations and orphan support bodies from being smuggled in as current candidates.

## Nonblocking Notes

- P2-D correctly keeps `css_fact_stream_same_tape_kind` as a post-W1b aperture rather than a current S-P3 shortlist candidate, because CSS generated Track 1, same-plane comparator, equality evidence, and CSS hot-leaf attribution do not exist yet (`p2d-substrate-tape.md:54`-`:66`, `:106`-`:112`).
- The current artifact set consistently treats JSON P1 hot leaves as nomination evidence only. That avoids the hidden coupling failure where JSON structural telemetry silently becomes CSS substrate proof.
- The older unpinned `hardening/V3` artifacts exist, but this review is scoped to the pinned path and folded packet after `75233b2b`.

## Exact Fold Edits

N/A. ACCEPT.

## Commands Used

```sh
git status --short && git rev-parse --short HEAD && git show --stat --oneline --decorate -1 75233b2b
rg -n "CH5|hidden|substrate|sidecar|Track 1|Track1|Track 2|Track2|fact stream|fact_stream|Lock 1|D3|REDRESS 96|REDRESS 97|REDRESS 98|category" restart/prompts/ORCHESTRATOR.md restart/prompts/skinny/PASS-2-RESEARCH.md restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md restart/skinny/tranches/sk-v12/HANDOFF.md restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md skinny/REDRESS.md
rg -n "Cycle: V3|PIN-V3|candidate|fact stream|fact_stream|Track1|Track2|sidecar|substrate|orphan|category|unblock|unblocked|Lock 1|row-owned|same-wave|consumer" restart/skinny/tranches/sk-v12/research/p2
nl -ba restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md | sed -n '1,180p'
nl -ba restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md | sed -n '1,150p'
nl -ba restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md | sed -n '1,130p'
nl -ba restart/locks/LOCKS.md | sed -n '45,125p'
nl -ba restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md | sed -n '35,125p'
nl -ba restart/skinny/tranches/sk-v12/HANDOFF.md | sed -n '50,180p'
nl -ba restart/prompts/ORCHESTRATOR.md | sed -n '70,125p'
nl -ba restart/prompts/skinny/PASS-2-RESEARCH.md | sed -n '36,130p'
nl -ba restart/prompts/skinny/PASS-2-RESEARCH.md | sed -n '220,255p'
nl -ba skinny/REDRESS.md | sed -n '2790,2955p'
nl -ba restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md | sed -n '25,80p'
nl -ba restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md | sed -n '45,110p'
ls -la restart/skinny/tranches/sk-v12/research/p2/hardening
find restart/skinny/tranches/sk-v12/research/p2/hardening -maxdepth 2 -type f | sort | sed -n '1,120p'
ps -axo pid,comm,args | rg 'cargo|rustc|xctrace|samply' | rg -v 'rg cargo|exec_command' || true
mkdir -p restart/skinny/tranches/sk-v12/research/p2/hardening/PIN-V3
```
