# SK-V12 S-P2 PIN-V2 CH5 Hidden Coupling / Substrate Challenge

Pass: S-P2 Research CHALLENGE. Cycle: PIN-V2.
Lens: CH5 hidden coupling / substrate.
Date: 2026-05-20.
Reviewed commit: `31859478`.
Output: this file.

## Verdict

ACCEPT.

Score: 96%.

## Blocking Findings

None.

## Acceptance Findings

1. The folded packet preserves one retained substrate. Lock 1 says retained
   structural projection is the tape, not a side substrate
   (`restart/locks/LOCKS.md:52`), and PASS-2 CH5 rejects a second source scan,
   retained cursor, aux density table, parser-owned projection, or sidecar event
   vector (`restart/prompts/skinny/PASS-2-RESEARCH.md:126`-`:131`,
   `:237`-`:240`). P2-D follows that rule: it identifies the live runtime as
   one offset tape (`Tape + ValueRef + ParserState`) and says any retained CSS
   structural facts must be the tape or generated same-tape metadata, not a
   parallel side vector (`restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md:11`-`:30`).

2. USER PIN D3 is handled as a category unblock, not as permission to smuggle
   the old sidecar shapes back in. The pin rescinds category-level union
   pre-blocks but keeps REDRESS 96/97/98 as measured-rejected implementations
   requiring citation, material differential, scalar/parity/checkasm,
   same-wave consumer, and CHALLENGE (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:39`-`:56`).
   P2-D repeats that D3 does not authorize a second retained substrate
   (`p2d-substrate-tape.md:32`-`:41`), and P2-C names the required differentials:
   no retained class column, no parser-owned cursor/list, no second scan stream,
   and producer plus consumer in the same wave (`p2c-arch-esoterica.md:196`-`:202`).

3. The actual P2-D union aperture is same-tape and not shortlist-ready. The
   only plausible future route is `css_fact_stream_same_tape_kind`, defined as
   opaque generated per-grammar facts stored in the existing tape or generated
   same-tape metadata, with explicit rejection of structural-position vectors,
   cursor sidecars, `UnionTape`, and public substrate APIs
   (`p2d-substrate-tape.md:102`-`:112`). The packet also keeps that route
   conditional until W1a/W1b create generated CSS Track 1, a same-plane fact
   stream, a lightningcss comparator, equality evidence, and CSS hot-leaf
   attribution (`p2d-substrate-tape.md:54`-`:78`, `:144`-`:158`).

4. The SIMD and parse-that candidates keep masks/cursors transient. P2-A C1
   returns transient class masks with no retained class lane, and C6 is an
   output-plane/oracle contract rather than a parser row mover
   (`p2a-sota-teardown.md:29`-`:37`). P2-B's admission process makes "no
   sidecar substrate" and same-wave consumer part of the close gate
   (`p2b-dav1d-process.md:29`-`:37`). P2-C demotes LD4, SHA3, cache hints, and
   support shapes when there is no current consumer, and explicitly rejects
   manufacturing an interleaved stream for LD4 under Lock 1
   (`p2c-arch-esoterica.md:38`-`:59`, `:70`-`:77`). P2-E forbids retained
   whitespace bitmaps, token class columns, structural cursor lists, decoded
   sidecars, and fact side vectors (`p2e-parse-that-gaps.md:54`-`:66`).

5. Track 1 and Track 2 independence is preserved. The live result surface states
   that Track 1 is `runtime::generated_json::parse`, Track 2 is the independent
   hand-coded parser over `runtime::tape`, and Track 2 never calls Track 1
   (`skinny/RESULTS.md:143`-`:146`). The SK-V12 handoff requires the CSS row to
   use one canonical fact stream shared by generated Track 1, independent
   Track 2/oracle, and lightningcss (`restart/skinny/tranches/sk-v12/HANDOFF.md:54`-`:58`,
   `:75`-`:83`). P2-A and P2-E keep digest/fact-stream work as row-owned output
   or oracle/accounting, not proof of parser speed or a substitute comparator
   (`p2a-sota-teardown.md:36`-`:37`, `:53`; `p2e-parse-that-gaps.md:41`-`:42`,
   `:54`, `:68`).

6. The fact stream is row-owned output, not a hidden sidecar. P2-E defines
   `pt_fact_event_emit` as the canonical CSS L4 fact stream shared by generated
   Track 1, independent oracle/Track 2, and lightningcss, and requires Lock 1
   proof (`p2e-parse-that-gaps.md:41`). P2-F's neutral boundary says tape/fact
   stream operations must emit offsets, flags, direct sinks, output facts, and
   row-owned digests into the single substrate/output plane; a retained
   structural sidecar, parser-owned cursor, decoded-byte cache, or digest-only
   proof is the drop trigger (`p2f-grammar-neutral.md:67`-`:77`).

## Nonblocking Notes

1. The score is not 100% because S-P3 must make `same-tape metadata` concrete
   before W3 redress. P2-D already supplies the guardrail: cardinality is +0 only
   if the facts are opaque/generated-owned on the retained tape, while any extra
   retained vector is cardinality +1 and fails Lock 1 (`p2d-substrate-tape.md:106`-`:112`).

2. Output digest remains a coupling hazard in later waves, but the PIN-V2 packet
   correctly labels digest-only work as parser-candidate-ineligible and requires
   independent same-plane equality before it can support CSS admission
   (`p2e-parse-that-gaps.md:41`-`:42`; `p2f-grammar-neutral.md:51`, `:91`).

3. CSS L4 is still absent as a measured generated row. That is not a CH5 defect
   in S-P2 because the packet treats JSON hot leaves as nomination evidence only
   and routes CSS evidence creation to S-P3/W1a/W1b.

## Fold Edits Required

None. ACCEPT.

## Commands Used

- `git status --short && git rev-parse --short HEAD && git show --stat --oneline --decorate --no-renames -1 31859478`
- `rg -n "CH5|hidden coupling|Lock 1|sidecar|substrate|Track 1|Track1|Track 2|Track2|REDRESS 96|REDRESS 97|REDRESS 98|D3|union" restart/prompts/ORCHESTRATOR.md restart/prompts/skinny/PASS-2-RESEARCH.md restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md restart/skinny/tranches/sk-v12/HANDOFF.md skinny/REDRESS.md`
- `rg -n "Cycle: V2|Pass: S-P2|substrate|sidecar|class lane|cursor|vector|Track 1|Track1|Track 2|Track2|fact stream|union|REDRESS 96|REDRESS 97|REDRESS 98|Lock 1" restart/skinny/tranches/sk-v12/research/p2 restart/skinny/tranches/sk-v12/research/p2/hardening/PIN-V2`
- `nl -ba restart/locks/LOCKS.md | sed -n '45,62p;72,116p'`
- `nl -ba restart/prompts/skinny/PASS-2-RESEARCH.md | sed -n '118,136p;232,242p'`
- `nl -ba restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md | sed -n '37,58p;110,120p'`
- `nl -ba restart/skinny/tranches/sk-v12/HANDOFF.md | sed -n '52,84p;90,98p;125,141p;154,172p'`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/p2d-substrate-tape.md | sed -n '1,170p'`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/p2f-grammar-neutral.md | sed -n '1,130p'`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md | sed -n '1,80p'`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/p2b-dav1d-process.md | sed -n '1,115p'`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/p2c-arch-esoterica.md | sed -n '1,230p'`
- `nl -ba restart/skinny/tranches/sk-v12/research/p2/p2e-parse-that-gaps.md | sed -n '1,120p'`
- `nl -ba skinny/RESULTS.md | sed -n '1,170p'`
- `nl -ba skinny/REDRESS.md | sed -n '2788,2955p;3288,3310p'`
