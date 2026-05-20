# SK-V12 Pass Alpha Hardening V2 - CH2 Generality / Lock 14

Date: 2026-05-20.
Lens: CH2 generality / Lock 14.
Verdict: PASS.

Scope: V2-folded `SYNTHESIS.md`, `HANDOFF.md`, Alpha-B, Alpha-E, and
Alpha-F under `USER-PIN-W1-CSS-L4-SOTA.md`, checked against PASS-ALPHA,
V1 consolidated hardening, and `skv12-value-api-audit.md`.

## Standard

PASS-ALPHA defines CH2 as the Lock 14 review: the goalset must respect
generality and work for non-JSON grammars, not only JSON
(`restart/prompts/pass-contracts/PASS-ALPHA.md:33-40`).

Under the USER PIN, that generality proof is not "any non-JSON grammar first."
It is the executable CSS L4 path first: CSS L4 is authoritative,
Sheets/BBNF-self are fallback-only after a measured CSS L4 redress attempt, and
the CSS close bar is same-plane strict equality with generated Track 1
`> lightningcss_mbps + 1`
(`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:18-34`).
The pin also keeps the seven Lock 14 JSON leaks as W1 blockers before CSS L4
emission is legal
(`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:90-103`).

V1 required the V2 fold to isolate CSS L4 emission from the JSON-named generic
template path, add a generic-crate branch scan, and bound union work away from
new public substrate/directive/BIR/BackendShape expansion
(`restart/skinny/tranches/sk-v12/research/alpha-hardening/V1/CONSOLIDATED.md:46-52`,
`restart/skinny/tranches/sk-v12/research/alpha-hardening/V1/CONSOLIDATED.md:71-74`).

## Verification Matrix

| Check | Result | Evidence |
|---|---|---|
| Executable CSS L4 path proves grammar generality | PASS | `SYNTHESIS.md` requires G-Alpha -> S-P1/S-P2/S-P3 reconvergence under the pin, then a generated CSS L4 row with Track 1 strictly `> lightningcss_mbps + 1`, same corpus, same output plane, same host, strict equality, generated/runtime/oracle/comparator provenance, and `GrammarConfig` or equivalent before CSS emission (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:39-57`). Alpha-E splits legality and row movement into W1a `GrammarConfig` followed by W1b CSS L4 generated baseline plus lightningcss comparator (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:56-70`). E1 names generated CSS runtime paths, same-plane lightningcss oracle, equality artifact, gate consumer, and `G-W1b-CSS-L4-LIGHTNINGCSS-BASELINE` (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:75-126`). Alpha-B supplies the required comparator floor, fields, and symmetric CSS fact stream (`restart/skinny/tranches/sk-v12/research/alpha/alpha-B-competitor-deltas.md:32-58`, `restart/skinny/tranches/sk-v12/research/alpha/alpha-B-competitor-deltas.md:84-113`). |
| Sheets and BBNF-self remain fallback-only | PASS | `SYNTHESIS.md` makes CSS L4 the authoritative first target and says Sheets/BBNF-self are fallback candidates only after a CSS attempt records BLOCKED or REJECTED evidence (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:77-78`, `restart/skinny/tranches/sk-v12/SYNTHESIS.md:173-180`). `HANDOFF.md` binds the priority order the same way and requires S-P3 to select CSS first while carrying Sheets/BBNF-self only as post-CSS-redress fallbacks (`restart/skinny/tranches/sk-v12/HANDOFF.md:51-65`, `restart/skinny/tranches/sk-v12/HANDOFF.md:110-124`). Alpha-F repeats that a CSS preflight-only miss cannot skip to Sheets (`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:40-47`, `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:105-110`). Alpha-E does not shortlist Sheets/BBNF-self and states they remain legal only after a recorded CSS L4 redress failure (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:341-345`). |
| Generic JSON policy leaks are forbidden | PASS | The value API audit identifies the seven leak classes in `json_templates/generated.rs`, runtime generated JSON, `OffsetFlags`, and `JsonSink`: structural alphabet, value dispatch, string/escape, number, key/member, flag semantics, and sink callbacks (`restart/skinny/tranches/sk-v12/research/skv12-value-api-audit.md:63-108`). V2 requires those policies to come from `GrammarConfig`, generated grammar metadata, or per-grammar generated modules before CSS emission can compile (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:138-185`; `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:85-88`; `restart/skinny/tranches/sk-v12/SYNTHESIS.md:54-57`). The gate fails generic grammar-name branches, producer-only telemetry, oracle coupling, parse-only admission, and missing lightningcss evidence (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:202-204`; `restart/skinny/tranches/sk-v12/HANDOFF.md:141-151`; `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:190-192`). |
| `json_templates/generated.rs` is correctly constrained | PASS | Alpha-E owner paths permit `skinny/crates/codegen/src/json_templates/generated.rs` only to preserve or extract existing JSON parity; it may not become the polymorphic CSS provider and may not branch on CSS/JSON grammar names (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:143-156`). The W1a gate rejects any generic branch on grammar name, corpus name, object/array role, field name, string role, or layout role, and requires structural alphabet, dispatch primary set, escape policy, number policy, key/member policy, flag interpretation, and sink trait to be supplied by generated metadata or per-grammar modules (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:169-178`). This folds V1 CH2's `json_templates/generated.rs` blocker. |
| No new directive/BIR/BackendShape/public substrate expansion | PASS | `SYNTHESIS.md` still blocks new directive, BIR variant, BackendShape variant, public substrate API, parser-owned sidecar, or x86 implementation work (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:206-217`). `HANDOFF.md` refuses dispatches that add a directive, BIR variant, BackendShape variant, public substrate API, parser-owned sidecar, or x86 implementation target (`restart/skinny/tranches/sk-v12/HANDOFF.md:153-166`). Alpha-F repeats the same refusal (`restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md:194-209`). Alpha-E bounds E4 to generated, CSS-local, output-plane-owned work consumed inside the CSS direct parser, allows tape/event files only for existing sealed/internal bounds, and requires public API diff proof that no directive, BIR variant, BackendShape variant, `UnionTape`, generic event side vector, retained cursor/list, or parser-owned fact slot was added (`restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md:225-272`). |

## Findings

### CH2-1 - PASS: Generality is proven through the executable CSS L4 path

V2 no longer proves Lock 14 by prose, W1a report-only infrastructure, or a
hand witness. The proof shape is executable and gate-consumed: W1a legalizes the
grammar-derived metadata surface, W1b emits the generated CSS L4 row, and the
row must compare against lightningcss with same-plane strict equality and full
provenance. That satisfies the USER PIN's stricter CSS-first generality
standard.

### CH2-2 - PASS: Sheets/BBNF-self cannot displace CSS

The folded contract keeps Sheets and BBNF-self out of the Alpha-E shortlist and
out of the first admission route. They are routed remainder only after a CSS L4
redress attempt records measured BLOCKED or REJECTED evidence. A CSS preflight
miss is explicitly not enough to skip to Sheets.

### CH2-3 - PASS: JSON policy is quarantined from generic code

The value API audit's leak inventory is reflected in the V2 gates. CSS policy
facts must originate in grammar source, generated metadata, or per-grammar
generated modules. Generic crates cannot branch on grammar names, corpus names,
JSON roles, CSS roles, or layout/string role policy. JSON guard rows remain guard
state, not the CSS close route.

### CH2-4 - PASS: `json_templates/generated.rs` is not a polymorphic CSS provider

V2 constrains `json_templates/generated.rs` to JSON parity preservation or
extraction. The legal CSS path is the grammar-neutral provider/config surface
plus generated CSS runtime output. That is the specific V1 CH2 fold required for
the seven Lock 14 leaks.

### CH2-5 - PASS: The union/ASM-gen unblocks do not expand public substrate

The USER PIN unblocks architectural categories, not public substrate. V2 keeps
that boundary: E4 is generated CSS-local or uses existing sealed/internal
`EventGrammar` bounds, and the gate requires a public API diff proving no new
directive, BIR variant, BackendShape variant, `UnionTape`, generic event side
vector, retained cursor/list, parser-owned fact slot, sidecar class column, or
public substrate API.

## Required Folds

None. The V2 contract passes CH2 / Lock 14 under the USER PIN.

Any later consolidation must preserve this CSS-first wording and must not revert
to "selected non-JSON grammar" or "CSS/Sheets/BBNF-self preflight-equivalent"
language. The pin-correct contract authority is the folded `SYNTHESIS.md`,
`HANDOFF.md`, Alpha-B, Alpha-E, and Alpha-F lines cited above.

## Changed Path

- `restart/skinny/tranches/sk-v12/research/alpha-hardening/V2/CH2.md`
