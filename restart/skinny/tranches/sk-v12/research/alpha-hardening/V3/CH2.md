# SK-V12 Pass Alpha Hardening V3 - CH2 Generality / Lock 14

Date: 2026-05-20.
Lens: CH2 generality / Lock 14.
Verdict: PASS.

Scope: current `SYNTHESIS.md`, `HANDOFF.md`, Alpha-B, Alpha-E, and Alpha-F
under `USER-PIN-W1-CSS-L4-SOTA.md`, checked against PASS-ALPHA, V1/V2
consolidated hardening, and `skv12-value-api-audit.md`.

## Standard

PASS-ALPHA defines CH2 as the Lock 14/generalization review: the goalset must
work for non-JSON grammars including CSS L4, Sheets, and BBNF-self
(`restart/prompts/pass-contracts/PASS-ALPHA.md:37-40`).

Under the USER PIN, the valid generality proof is CSS-first and executable:
W1 admits the CSS L4 generated baseline; Sheets and BBNF-self are legal only
after a CSS L4 redress attempt fails; and CSS closes only when generated Track 1
strictly beats `lightningcss_mbps + 1` on the same corpus, same output plane,
with strict equality
(`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:18-34`).
The pin also requires the seven Lock 14 JSON leaks to be resolved through W1's
`GrammarConfig` surface before CSS L4 emission is legal
(`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:90-103`).

V1 required CSS L4 emission to leave the JSON-named generic template path and
blocked new public substrate/directive/BIR/BackendShape expansion
(`restart/skinny/tranches/sk-v12/research/alpha-hardening/V1/CONSOLIDATED.md:46-52`,
`restart/skinny/tranches/sk-v12/research/alpha-hardening/V1/CONSOLIDATED.md:71-74`).
V2 passed CH2 but required V3 to preserve CSS-first authority while folding CH5's
Alpha-E local JSON guard and pre-pin authority qualifiers
(`restart/skinny/tranches/sk-v12/research/alpha-hardening/V2/CONSOLIDATED.md:20-24`,
`restart/skinny/tranches/sk-v12/research/alpha-hardening/V2/CONSOLIDATED.md:27-38`).

## Findings

### CH2-1 - PASS: CSS L4 generality is executable and first

The current contract does not rely on prose generality, Sheets preflight, or a
hand-only witness. ADMIT requires G-Alpha and pin-aware S-P1/S-P2/S-P3
reconvergence, then a generated CSS L4 row whose Track 1 throughput is strictly
greater than `lightningcss_mbps + 1` on the same corpus, same output plane, same
host, and strict equality semantics
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:39-57`). Alpha-B specifies the
required CSS comparator fields and says missing same-host CSS/lightningcss data
is `UNMEASURED`, not a pass
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-B-competitor-deltas.md:84-113`).
Alpha-E splits the legal path into W1a `GrammarConfig` extraction followed by
W1b CSS L4 baseline plus lightningcss comparator, with gate-consumed equality
and report evidence
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:56-75`,
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:77-128`).

### CH2-2 - PASS: Sheets and BBNF-self are fallback-only

The packet consistently blocks Sheets/BBNF-self before measured CSS redress.
`SYNTHESIS.md` makes CSS L4 the first target and permits Sheets/BBNF-self only
after CSS records BLOCKED or REJECTED evidence
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:77-78`,
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:173-180`,
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:243-250`). `HANDOFF.md` carries the
same priority order and requires the downstream S-P3 plan to select CSS first
while carrying Sheets/BBNF-self only as post-CSS-redress fallbacks
(`restart/skinny/tranches/sk-v12/HANDOFF.md:51-65`,
`restart/skinny/tranches/sk-v12/HANDOFF.md:110-124`). Alpha-E excludes
Sheets/BBNF-self from the shortlist and records them only as non-shortlisted
fallbacks after a CSS L4 redress failure
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:347-351`).

### CH2-3 - PASS: Generic JSON policy leak bans are explicit

The value API audit identifies the load-bearing leaks: JSON structural alphabet,
value dispatch, string/escape policy, number policy, key/member assumption,
OffsetFlags semantics, and JsonSink callbacks
(`restart/skinny/tranches/sk-v12/research/skv12-value-api-audit.md:63-108`).
The current contract makes those leaks admission blockers: CSS emission is legal
only after `GrammarConfig` or equivalent grammar-derived metadata resolves them
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:54-57`,
`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:140-184`,
`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:85-88`).
The current JSON template still contains those JSON-specific policies, for
example the JSON structural alphabet and dispatch/key/string/number logic
(`skinny/crates/codegen/src/json_templates/generated.rs:10-17`,
`skinny/crates/codegen/src/json_templates/generated.rs:47-58`,
`skinny/crates/codegen/src/json_templates/generated.rs:83-100`,
`skinny/crates/codegen/src/json_templates/generated.rs:205-217`), so the
contract is correct to treat extraction as a prerequisite rather than as
already-complete implementation evidence.

### CH2-4 - PASS: `json_templates/generated.rs` is limited, not promoted

Alpha-E now constrains `skinny/crates/codegen/src/json_templates/generated.rs`
to preserving or extracting existing JSON parity. It may not become the
polymorphic CSS provider and may not branch on CSS/JSON grammar names
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:145-160`).
The W1a gate rejects new public JSON-named generic APIs, generic branches on
grammar/corpus/object/array/field/string/layout roles, and requires the
structural alphabet, primary dispatch set, escape policy, number policy,
key/member policy, flag interpretation, and sink trait to come from generated
metadata or per-grammar modules
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:171-184`).
This folds V1 CH2's template-path blocker without making the JSON template a
cross-grammar policy table.

### CH2-5 - PASS: Pre-pin authority is properly qualified

`SYNTHESIS.md` demotes `SPEC.md` to pre-pin context only where it does not
conflict with the user pin
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:13-24`) and requires fresh
G-Alpha -> S-P1 -> S-P2 -> S-P3 under the pin before implementation authority
exists
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:235-254`). `HANDOFF.md` gives the
same qualifier for the earlier implementation packet and pre-pin pass artifacts
(`restart/skinny/tranches/sk-v12/HANDOFF.md:5-26`,
`restart/skinny/tranches/sk-v12/HANDOFF.md:103-125`). The V2 blocker in
Alpha-E is folded: its authority list now marks `SPEC.md` as pre-pin context
only where non-conflicting, and pre-pin S-P1/S-P2/S-P3 artifacts as context only
after measured revalidation under the user pin
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:9-29`).
Alpha-F still lists `SPEC.md` as read input, but it qualifies the existing packet
as stale wherever it treats CSS/Sheets/BBNF-self as preflight-equivalent or keeps
union/ASM-gen blocked, and leaves `SPEC.md`/`DISPATCH-PROMPT.md` to downstream
S-P3
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:11-16`,
`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:235-237`).

### CH2-6 - PASS: No directive/BIR/BackendShape/public substrate expansion

The contract blocks new directive, BIR variant, BackendShape variant, public
substrate API, parser-owned sidecar, and x86 implementation work
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:206-217`,
`restart/skinny/tranches/sk-v12/HANDOFF.md:153-166`,
`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:194-209`).
E4 is bounded to generated, CSS-local, output-plane-owned work consumed by the
CSS direct parser; `event_grammar.rs`/`tape/mod.rs` may only consume existing
sealed/internal bounds without public API expansion; and the gate requires a
public API diff proving no directive, BIR variant, BackendShape variant,
`UnionTape`, generic event side vector, retained cursor/list, or parser-owned
fact slot was added
(`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:231-278`).
The current substrate evidence matches that boundary: `BackendShape` remains the
five-shape enum
(`skinny/crates/ir/src/lib.rs:401-408`), the Lock 14 baseline gate checks that
variant count and rejects `UnionTape`
(`skinny/crates/bbnf-bench/src/lock14_baseline.rs:565-593`), and grammar
directives remain limited to the skinny-accepted `import` and `token` forms
(`skinny/crates/grammar/src/lib.rs:80-99`).

## Required Folds

None for CH2. The current V3 packet passes Lock 14/generalization under the user
pin. Later consolidation must preserve the CSS-first executable route, the
post-CSS-redress fallback rule, the `json_templates/generated.rs` limit, and the
ban on public substrate/directive/BIR/BackendShape expansion.

## Changed Path

- `restart/skinny/tranches/sk-v12/research/alpha-hardening/V3/CH2.md`
