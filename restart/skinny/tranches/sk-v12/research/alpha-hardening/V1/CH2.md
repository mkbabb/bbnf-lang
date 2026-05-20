# SK-V12 Pass Alpha CHALLENGE V1 - CH2 Generality / Lock 14

Date: 2026-05-20.
Lens: CH2 generality / Lock 14.
Scope: Pass Alpha SK-V11 -> SK-V12 USER-PIN V1 alpha A-F plus
`SYNTHESIS.md`, `HANDOFF.md`, current `SPEC.md`, and the Lock 14 value API
audit.

## Disposition

REVISE.

The re-bracket correctly repairs the largest pre-pin CH2 failure: CSS L4 is now
authoritative, Sheets/BBNF-self are fallback-only after a CSS L4 redress
attempt, the close floor is `lightningcss_mbps + 1`, and union/ASM-gen are
category-open only with material differentials and same-wave consumers
(`USER-PIN-W1-CSS-L4-SOTA.md:18-56`, `:58-78`; `alpha-F-contract-draft.md:39-60`).

The remaining CH2 defects are narrower but blocking for convergence: the
`GrammarConfig` route still allows a JSON-named generic template to be the CSS
emission owner, and the union candidate needs a sharper generated-private
boundary so the user-pin unblock does not become a public substrate/BIR/directive
reopen.

## Sources Read

- `restart/prompts/ORCHESTRATOR.md` Section 3W.
- `restart/prompts/pass-contracts/PASS-ALPHA.md` Section 3.
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md`.
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-A-results-extraction.md`.
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-B-competitor-deltas.md`.
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-C-redress-digest.md`.
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-D-validated-invalidated.md`.
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-E-candidate-shortlist.md`.
- `restart/skinny/tranches/sk-v12/research/alpha/alpha-F-contract-draft.md`.
- `restart/skinny/tranches/sk-v12/research/skv12-value-api-audit.md`.
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md`.
- `restart/skinny/tranches/sk-v12/HANDOFF.md`.
- `restart/skinny/tranches/sk-v12/SPEC.md`.

## Lens Standard

`ORCHESTRATOR.md` defines CH2 as Lock 14 generality: no grammar-name leak, and
every intervention must be grammar-neutral across CSS L4 / Sheets / BBNF-self,
not only JSON (`ORCHESTRATOR.md:81-85`). `PASS-ALPHA.md` applies that question
directly to alpha A-F (`PASS-ALPHA.md:33-40`).

For this user-pin cycle, the standard is stricter than ordinary non-JSON
coverage:

- CSS L4 is first and cannot be skipped for Sheets/BBNF-self on preflight
  alone (`USER-PIN-W1-CSS-L4-SOTA.md:18-27`).
- CSS admission requires same-plane strict equality and
  `lightningcss_mbps + 1`, not an internal 1% lift
  (`USER-PIN-W1-CSS-L4-SOTA.md:29-37`).
- The seven Lock 14 leaks must be resolved by `GrammarConfig` or equivalent
  generated metadata before CSS emission is legal
  (`USER-PIN-W1-CSS-L4-SOTA.md:90-106`).
- Union and ASM-gen category unblocks do not waive scalar/parity/checkasm,
  CHALLENGE, material-differential, or same-wave-consumer requirements
  (`USER-PIN-W1-CSS-L4-SOTA.md:39-69`).

## Disposition Matrix

| Surface | Disposition | CH2 rationale |
|---|---|---|
| Alpha-A results extraction | ACCEPT | It marks generated CSS L4 absent, makes the old Sheets plan obsolete, carries JSON rows as guard/fixpoint evidence only, and names `GrammarConfig` as the blocker before legal CSS emission (`alpha-A-results-extraction.md:37-45`, `:52-72`, `:207-219`). |
| Alpha-B competitor deltas | ACCEPT | It moves the comparator plane to CSS L4 vs lightningcss, names same-plane strict fields, and refuses to convert missing CSS measurement into a pass (`alpha-B-competitor-deltas.md:32-53`, `:69-100`). |
| Alpha-C REDRESS digest | ACCEPT | It correctly treats REDRESS 112/113 as superseded only for the explicit CSS mandate, keeps Sheets post-CSS-redress, and reopens union/ASM-gen only as categories with prior REDRESS and material-differential accounting (`alpha-C-redress-digest.md:109-128`, `:230-245`, `:249-258`). |
| Alpha-D validated/invalidated ledger | ACCEPT | It records CSS-first, demotes W1a/Sheets, and requires `GrammarConfig`, Lock 16, zero-orphan SIMD, and measured union/ASM-gen attempts for fixpoint (`alpha-D-validated-invalidated.md:12-24`, `:262-284`, `:302-310`). |
| Alpha-E candidate shortlist | REVISE | E1/E2/E3/E5 are directionally Lock 14 clean, but E2 still lists `json_templates/generated.rs` as a generic owner for CSS legalization, and E4's "same-tape event projection" needs an explicit generated-private boundary before CH2 can accept it (`alpha-E-candidate-shortlist.md:122-159`, `:204-245`). |
| Alpha-F contract draft / SYNTHESIS / HANDOFF | REVISE | They carry the right pin order and refusal conditions, but must fold the two Alpha-E defects so S-P3 cannot emit CSS through a JSON-named generic template or reopen a public substrate while calling it a union attempt (`alpha-F-contract-draft.md:133-152`, `:173-192`; `SYNTHESIS.md:178-198`; `HANDOFF.md:91-138`). |
| Current SPEC | ROUTED STALE | It remains pre-pin in several clauses: it still bans union/substrate categories and keeps old REDRESS blocks (`SPEC.md:237-245`, `:666-669`). Alpha-F correctly leaves SPEC for downstream S-P3 and says it is stale where it contradicts the pin (`alpha-F-contract-draft.md:207-209`), so this is not an Alpha-F edit defect, but G-Alpha must not treat current SPEC as dispatch authority for D3/D4. |

## Critical Findings

### CH2-1 - REVISE: CSS emission is not yet isolated from a JSON-named generic template

The value API audit identifies seven Lock 14 leaks in the generated JSON
template surface: hardcoded JSON structural bytes, JSON value dispatch, JSON
escape/quote policy, JSON number policy, JSON member/key policy, JSON escape
flag semantics, and `JsonSink` callbacks (`skv12-value-api-audit.md:63-108`).
Its minimal route is `GrammarConfig` plus generated per-grammar metadata, not
another generic grammar-name branch (`skv12-value-api-audit.md:160-207`).

Alpha-E names the right goal for E2, but its owner list still includes
`skinny/crates/codegen/src/json_templates/generated.rs` beside new
grammar-neutral provider files (`alpha-E-candidate-shortlist.md:122-140`).
That is not automatically illegal, because JSON parity edits may be needed.
It is insufficiently constrained for CH2: CSS L4 emission must not execute
through a JSON-named generic template that branches on grammar name or carries
CSS policy as the next hardcoded case.

Fold required:

1. State that `json_templates/generated.rs` may be edited only to preserve or
   extract JSON parity, not as the CSS L4 provider.
2. Name the grammar-neutral emission surface S-P3 must use for CSS, for example
   `nonjson_profile.rs` / `generated_config.rs` plus generated per-grammar
   runtime output.
3. Add a gate check that rejects `if grammar_name == "css_l4"` /
   `match grammar_name` / grammar-named feature branches in generic crates.
4. Require CSS policy facts to originate from grammar source, generated
   metadata, or generated per-grammar modules, matching SPEC Section 2.1's rule
   that templates are shared grammar-neutral generator code (`SPEC.md:289-312`).

Without that fold, the packet says "GrammarConfig before CSS emission" but does
not prove the CSS emission path is free of the same class of generic
grammar-policy leak that invalidated the Sheets `sheets_direct.rs` route.

### CH2-2 - REVISE: E4 union wording can reopen a public substrate unless bounded to generated-private CSS facts

The user pin unblocks the Rust union-substrate architectural category, but it
does not unblock new directives, BIR variants, BackendShape variants, public
substrate APIs, or parser-owned sidecars (`USER-PIN-W1-CSS-L4-SOTA.md:39-56`,
`:90-100`). Alpha-F repeats that refusal (`alpha-F-contract-draft.md:179-192`),
and SYNTHESIS keeps those shapes blocked while reopening only the category with
REDRESS citation and measured differential (`SYNTHESIS.md:178-198`).

Alpha-E's E4 is close to acceptable because it targets CSS declaration-value
direct parsing and explicitly says it does not retain a second structural
vector, public substrate API, parser-owned sidecar, or parse-only scanner
(`alpha-E-candidate-shortlist.md:204-217`, `:236-245`). The gap is that the same
section also allows a "same-tape event projection" and names generic tape/event
owner paths (`alpha-E-candidate-shortlist.md:213-225`) without saying that any
new union tag/projection must be generated CSS-local and private to the selected
row.

Fold required:

1. Bind E4 to a generated per-grammar CSS enum/projection consumed inside
   `runtime/src/grammars/css_l4_declaration_values/`, or to an existing
   `EventGrammar` bound with no public API expansion.
2. Reject any new `BackendShape`, BIR variant, BBNF directive, `UnionTape`,
   public substrate API, generic event side vector, retained cursor/list, or
   parser-owned fact slot.
3. Require the E4 gate to scan `ir/src/`, `grammar/`, `runtime/src/tape/`, and
   generic codegen for those additions before redress can claim a material
   differential.

This fold preserves the user-pin D3 unblock while keeping Lock 14 and the
public-substrate boundary intact.

## Accepted CH2 Findings

| Check | Result | Evidence |
|---|---|---|
| CSS L4 is authoritative | ACCEPT | Alpha-A marks no current CSS row and says W1 V2 Sheets is obsolete (`alpha-A-results-extraction.md:37-45`, `:227-239`); Alpha-F requires CSS first and post-redress-only fallbacks (`alpha-F-contract-draft.md:39-46`, `:137-143`). |
| `lightningcss_mbps + 1` is the close bar | ACCEPT | Alpha-B binds the formula and required comparator fields (`alpha-B-competitor-deltas.md:32-49`, `:79-100`); Alpha-F and HANDOFF carry the same floor (`alpha-F-contract-draft.md:70-84`; `HANDOFF.md:95-109`). |
| Sheets/BBNF-self fallback ordering | ACCEPT | Alpha-C and SYNTHESIS allow Sheets/BBNF-self only after CSS redress records a measured failure (`alpha-C-redress-digest.md:120-128`; `SYNTHESIS.md:145-153`). |
| `GrammarConfig` precondition recognized | ACCEPT WITH FOLD | Alpha-A, Alpha-D, Alpha-E, Alpha-F, SYNTHESIS, and HANDOFF all say CSS emission is illegal until the leaks are resolved (`alpha-A-results-extraction.md:217-219`; `alpha-D-validated-invalidated.md:262-266`; `alpha-F-contract-draft.md:81-84`; `SYNTHESIS.md:48-50`; `HANDOFF.md:103-104`). The fold is CH2-1's path isolation. |
| Union/ASM-gen category reopen does not erase historical REDRESS | ACCEPT WITH FOLD | Alpha-C and Alpha-D keep REDRESS 88/89/90/96/97/98 as historical implementation evidence and require material differential plus measurement (`alpha-C-redress-digest.md:49-107`; `alpha-D-validated-invalidated.md:202-238`). The fold is CH2-2's no-public-substrate boundary. |
| SIMD is not admitted before the escape-mask bug | ACCEPT | Alpha-E makes E3 a prerequisite before E5 or any new SIMD admission (`alpha-E-candidate-shortlist.md:62-65`, `:165-179`, `:289-301`). |

## Required Fold

1. Revise Alpha-E E2 and Alpha-F/SYNTHESIS/HANDOFF dispatch text to state that
   CSS L4 emission uses a grammar-neutral generator/config path plus generated
   CSS runtime output, not `json_templates/generated.rs` as a polymorphic CSS
   provider.
2. Add a generic-crate branch scan to the Alpha-F telemetry/refusal text:
   reject grammar-name, corpus-name, object/array-role, field-name, string-role,
   layout-role, or CSS-specific branches in generic crates.
3. Revise Alpha-E E4 so any union tag/projection is generated CSS-local or uses
   an existing non-public bound. Explicitly reject public substrate API, BIR,
   directive, BackendShape, `UnionTape`, sidecar, retained cursor/list, and
   parser-owned fact-slot additions inside the E4 gate.
4. Keep current SPEC clauses that conflict with D3/D4 marked stale/non-authority
   until S-P3 rewrites SPEC under the pin. Current `SPEC.md` cannot be cited to
   block the category-level union/ASM-gen routes, nor can it be cited to permit
   public substrate expansion.

## CH2 Verdict

REVISE. The USER-PIN re-bracket is substantially aligned with Lock 14, but V1
does not yet prove the CSS emission path is free of generic JSON-template policy
or that the reopened union category remains generated-private. Fold CH2-1 and
CH2-2 before CH2 can converge to ACCEPT.

Changed path:

- `restart/skinny/tranches/sk-v12/research/alpha-hardening/V1/CH2.md`
