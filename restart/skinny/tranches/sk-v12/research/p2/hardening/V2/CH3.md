# SK-V12 S-P2 CHALLENGE V2 - CH3 Regression

Pass: S-P2 Research.
Cycle: V2.
Lens: CH3 REGRESSION.
Date: 2026-05-20.
Disposition: ACCEPT.

## Scope

This lens rechecked the folded S-P2 research packet after V1 hardening. The
question is whether any candidate or clarification reopens a `skinny/REDRESS.md`
route, or treats profile/diagnostic evidence as implementation authority, under
`restart/prompts/skinny/PASS-2-RESEARCH.md:109` through `:118`.

Read set: all six folded S-P2 artifacts, V1 CH1-CH6 and
`HARDENING-S-P2-V1-CONSOLIDATED.md`, `skinny/REDRESS.md` entries 28+33,
50-55, 60-72, 80, 82-84, 88, 89, 96-120, `skinny/RESULTS.md`,
`restart/locks/LOCKS.md`, and
`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`.

## Findings

1. **V1 fold tightened the packet instead of reopening a route.** The V1
   consolidated file required P2-A scalar/checkasm/consumer accounting, demoted
   P2-C speculative LD4 and SHA3 entries, reframed P2-D tape-shape items as
   diagnostic/ineligible, and split P2-F oracle/accounting-only families from
   parser candidates
   (`restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md:22`-`:45`).
   The folded artifacts now carry that narrowing: P2-A says candidates are not
   selected waves (`p2a-sota-teardown.md:27`), P2-C counts six current
   candidates plus two ISA-inventory entries (`p2c-arch-esoterica.md:38`-`:42`),
   P2-D records zero selectable S-P1 candidates (`p2d-substrate-tape.md:69`-`:72`),
   and P2-F marks F7 oracle-only and F8 accounting-only
   (`p2f-grammar-neutral.md:25`-`:29`).

2. **Sidecar, retained structural, and W3 union routes remain closed.** Lock 1
   allows transient SIMD masks but forbids parallel retained substrates
   (`restart/locks/LOCKS.md:52`). P2-A treats simdjson's retained structural
   index as comparator evidence only (`p2a-sota-teardown.md:20`, `:155`).
   P2-B requires transient masks and rejects retained class columns, side tables,
   retained cursors, and second scans (`p2b-dav1d-process.md:34`, `:38`-`:46`,
   `:72`-`:74`). P2-D explicitly rejects `structural_class_lane_union` with
   REDRESS 96/97/98 and no legal same-wave consumer
   (`p2d-substrate-tape.md:53`-`:59`, `:79`). This does not reopen REDRESS
   50/51/53 or the REDRESS 96/97/98 retired union-substrate thesis
   (`skinny/REDRESS.md:715`-`:813`, `:2910`-`:2934`).

3. **String, tiny-string, Unicode, and escaped-segment work is still
   constrained by prior rejections.** REDRESS 28/33 rejected active tiny-string
   dispatch despite primitive parity, REDRESS 54/55 and 60-72 block decoded
   string/sidecar/materialization families, REDRESS 82/83/84 block
   single-quartet/StringBlock/object-pair metadata routes, and REDRESS 116/117
   block the SK-V11 string-span and escaped-segment direct entries
   (`skinny/REDRESS.md:324`-`:337`, `:815`-`:846`, `:1380`-`:1439`,
   `:3411`-`:3460`). The folded packet does not bypass those results: P2-A
   requires generated same-wave consumers and forbids decoded-byte sidecars
   (`p2a-sota-teardown.md:32`-`:33`, `:83`-`:85`, `:157`); P2-B keeps string and
   hex primitives proof-only until a real caller exists (`p2b-dav1d-process.md:41`-`:42`,
   `:75`); P2-C requires row evidence/source delta for C4/C5
   (`p2c-arch-esoterica.md:78`-`:98`, `:150`-`:151`); P2-E requires generated
   consumers and excludes StringBlock16, sidecars, eager materialization, and
   host-sink dependency (`p2e-parse-that-gaps.md:147`-`:151`, `:278`-`:282`,
   `:354`-`:358`).

4. **Numeric, container-tail, PMULL, CTZ, and digest routes remain pre-blocked.**
   REDRESS 80 and 114 block mantissa/numeric-slot reuse; REDRESS 115 blocks the
   container-tail direct route; REDRESS 88 and 89 block PMULL prefix-XOR and CTZ
   bulk-consumer production from primitive proof; REDRESS 118 blocks digest/hash
   host-sink proof without a legal row, consumer, and oracle
   (`skinny/REDRESS.md:2215`-`:2250`, `:2508`-`:2580`, `:3357`-`:3494`).
   The folded packet preserves those boundaries: P2-A C4/C5/C6 reject numeric
   slot, JSON container-tail, and unowned digest reuse (`p2a-sota-teardown.md:34`-`:36`,
   `:97`, `:109`, `:121`); P2-B marks digit, bitmap, and digest paths as
   constrained or oracle-only (`p2b-dav1d-process.md:43`-`:49`, `:76`-`:78`);
   P2-C keeps PMULL blocked and CTZ narrow/support-only (`p2c-arch-esoterica.md:28`,
   `:111`-`:120`, `:152`); P2-E and P2-F repeat that digit/digest/dispatch
   evidence is not row movement by itself (`p2e-parse-that-gaps.md:216`-`:220`,
   `:376`-`:392`; `p2f-grammar-neutral.md:39`, `:59`-`:62`).

5. **Generated non-JSON baseline priority is consistently carried, so diagnostics
   are not treated as implementation authority.** S-P1 convergence says JSON-only
   telemetry may nominate primitive families but does not prove CSS L4, Sheets,
   or BBNF-self behavior
   (`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:60`-`:63`).
   RESULTS remains `N-direct / NoGo` with JSON direct residuals and guard rows
   only (`skinny/RESULTS.md:143`-`:146`). REDRESS 119/120 route SK-V12 to the
   generated non-JSON baseline first and exhaust the direct residual rows unless
   a future packet names fresh material evidence beyond REDRESS 114-119
   (`skinny/REDRESS.md:3497`-`:3553`). The folded P2 packet repeats this route
   in P2-A, P2-B, P2-C, P2-D, P2-E, and P2-F
   (`p2a-sota-teardown.md:12`, `:163`; `p2b-dav1d-process.md:72`-`:73`;
   `p2c-arch-esoterica.md:14`, `:153`; `p2d-substrate-tape.md:63`-`:65`;
   `p2e-parse-that-gaps.md:46`, `:376`; `p2f-grammar-neutral.md:15`, `:63`-`:64`).

## Verdict

ACCEPT. The V2 folded S-P2 packet does not reopen a REDRESS-blocked route and
does not convert diagnostics, comparator architecture, profile leaves, or
primitive parity into implementation authority. S-P3 may consume this research
only with the carried regression guards: generated non-JSON baseline before JSON
residual work; no retained sidecar or parallel substrate; scalar reference,
strict parity, and same-wave generated/runtime consumer before any SIMD or
native primitive can move behavior; output digest remains oracle or row-owned
product evidence; direct residual JSON rows remain fixpoint/pre-blocked unless a
later packet supplies fresh P1 evidence and a materially new framing beyond
REDRESS 114-120.

## Revise List

None for CH3 REGRESSION V2.
