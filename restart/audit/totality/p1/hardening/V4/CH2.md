# T-P1 V4 CH2 Generality

Verdict: ACCEPT.

Score: 5 / 5 CH2 checks pass. The SK-V15 V4 packet keeps Lock 14
generality explicit across the six live inventories. The V3 CH2 accepted
surface is preserved: `P1-1B-D9` / `P1-1B-D10` remain grammar-neutral Lock 14
failures with non-JSON proof receivers, and the V4 receiver cost carrier plus
primitive/kernel receiver table do not demote those rows, or the primitive
rows, into JSON-only lessons.

V4 is still only the first possible clean T-P1 cycle: the V4 dispatch context
states V3 returned 3 / 7 ACCEPT and V4 cannot lock T-P1 alone
(`restart/audit/totality/p1/hardening/V4/CHALLENGE-CONTEXT.md:24-26`).

## Evidence

Authority read:

```sh
nl -ba restart/prompts/totality/PASS-1-EXCAVATION.md | sed -n '104,132p'
nl -ba restart/prompts/ORCHESTRATOR.md | sed -n '74,126p'
nl -ba restart/audit/totality/p1/hardening/V4/CHALLENGE-CONTEXT.md | sed -n '1,90p'
nl -ba restart/audit/totality/p1/hardening/HARDENING-T-P1-V3-CONSOLIDATED.md | sed -n '1,68p'
nl -ba restart/audit/totality/p1/hardening/V3/CH2.md | sed -n '1,68p'
```

Inventory and source checks run:

```sh
nl -ba restart/audit/totality/p1/1A-substrate-evidence.md
nl -ba restart/audit/totality/p1/1B-codegen-evidence.md
nl -ba restart/audit/totality/p1/1C-runtime-evidence.md
nl -ba restart/audit/totality/p1/1D-skinny-lessons.md
nl -ba restart/audit/totality/p1/1E-locks-evidence.md
nl -ba restart/audit/totality/p1/1F-coherence-scan.md
rg -n "JSON-only|json-only|JSON-empirical|grammar-neutral|P1-1B-D9|P1-1B-D10|non-JSON|Sheets|BBNF-self|primitive same-wave|V4 Primitive|Primitive same-wave|Parse-that vocabulary|source-present primitive|grammar-neutral byte-class" restart/audit/totality/p1/1A-substrate-evidence.md restart/audit/totality/p1/1B-codegen-evidence.md restart/audit/totality/p1/1C-runtime-evidence.md restart/audit/totality/p1/1D-skinny-lessons.md restart/audit/totality/p1/1E-locks-evidence.md restart/audit/totality/p1/1F-coherence-scan.md
nl -ba restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md | sed -n '20,62p;60,90p'
nl -ba restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md | sed -n '10,65p;220,240p'
```

Material evidence:

- CH2's required standard is Lock 14 generality: no JSON-only cataloguing of
  grammar-neutral facts, 1C must flag grammar-name leaks, 1D must separate
  JSON-empirical from grammar-neutral findings, and no grammar-name leak may
  pass uncited (`restart/prompts/totality/PASS-1-EXCAVATION.md:110-114`).
  The universal CH2 lens also requires grammar-neutral interventions that work
  for CSS L4 / Sheets / BBNF-self, not only JSON
  (`restart/prompts/ORCHESTRATOR.md:81-85`).
- The V4 dispatch focus is exact: verify receiver tables remain
  grammar-neutral and do not demote `P1-1B-D9` / `P1-1B-D10` or primitive rows
  into JSON-only lessons
  (`restart/audit/totality/p1/hardening/V4/CHALLENGE-CONTEXT.md:53-59`).
  The same context supersedes stale prior SK-V14 V4 CH files in place
  (`restart/audit/totality/p1/hardening/V4/CHALLENGE-CONTEXT.md:3-7`) and
  declares `1F-anti-pattern.md` and `1F-past-corpora.md` historical only
  (`restart/audit/totality/p1/hardening/V4/CHALLENGE-CONTEXT.md:19-22`).
- V3 already accepted CH2 on this axis:
  `HARDENING-T-P1-V3-CONSOLIDATED.md:26` says `P1-1B-D9` / `P1-1B-D10` are
  grammar-neutral Lock 14 failures with non-JSON proof receivers, and
  `HARDENING-T-P1-V3-CONSOLIDATED.md:49-50` says that state plus the Lock 14
  owner/receiver map must be preserved. V3 CH2's own required fold was
  "None" except preserving the `G-10` row and owner/receiver map
  (`restart/audit/totality/p1/hardening/V3/CH2.md:64-68`).
- `1B-codegen-evidence.md` still names the live leaks as generic pass/codegen
  failures: D9 recognizer mining is JSON-punctuation-coded and D10
  materialization role mining is JSON-role-coded
  (`restart/audit/totality/p1/1B-codegen-evidence.md:52-53`). The V2/V3 fold
  rows explicitly preserve them as grammar-neutral Lock 14 findings with
  Sheets/BBNF-self proof receivers, not JSON-only routing
  (`restart/audit/totality/p1/1B-codegen-evidence.md:71`,
  `restart/audit/totality/p1/1B-codegen-evidence.md:79`). Their divergence rows
  require Sheets/BBNF-self/CSS or non-JSON generated metadata proof
  (`restart/audit/totality/p1/1B-codegen-evidence.md:107-108`).
- `1C-runtime-evidence.md` satisfies the 1C CH2 obligation by flagging generic
  runtime/codegen grammar-name leaks rather than laundering them as generated
  allowance: runtime root generic modules are unimplemented against Lock 14
  (`restart/audit/totality/p1/1C-runtime-evidence.md:61`), the generated vs
  hand-owned audit marks runtime root and Pattern H as Lock 14 failures
  (`restart/audit/totality/p1/1C-runtime-evidence.md:90-94`), and the
  divergence table carries the root/profile/CSS-row leaks as open rows
  (`restart/audit/totality/p1/1C-runtime-evidence.md:118-123`).
- `1D-skinny-lessons.md` keeps JSON-empirical, grammar-neutral, and
  CSS-audit-demoted lessons in separate sections. JSON rows are scoped as
  JSON-empirical at `1D:101-106`; grammar-neutral rows start at `1D:134`, and
  rows `G-3` through `G-10` include Lock 14/16, Pattern H, Decision Engine,
  codegen neutrality, retained sidecars, primitive gates, parse-that gaps, and
  D9/D10 as grammar-neutral findings (`restart/audit/totality/p1/1D-skinny-lessons.md:134-147`).
  The CSS demotion is separate at `1D:149-159`.
- `1D` preserves the D9/D10 fold verbatim: `G-10` says pass-layer recognizer
  mining and materialization role mining are JSON-shaped generic pass logic,
  not JSON-only empirical lessons, with Sheets and BBNF-self as proof receivers
  (`restart/audit/totality/p1/1D-skinny-lessons.md:147`), and the V2 fold row
  repeats the same classification (`restart/audit/totality/p1/1D-skinny-lessons.md:190`).
- The V4 receiver cost carrier remains grammar-neutral where CH2 needs it.
  `RC-07` routes `P1-1B-D8` / `P1-1B-D9` / `P1-1B-D10` to CSS plus
  Sheets-or-BBNF-self generated metadata proof and rejects any branch that lacks
  non-JSON proof (`restart/audit/totality/p1/1D-skinny-lessons.md:180`).
  `RC-10` requires scalar oracle, non-JSON witness or scoped claim, REDRESS
  pre-block check, and generated consumer for parse-that vocabulary
  (`restart/audit/totality/p1/1D-skinny-lessons.md:183`). `RC-11` routes
  primitive same-wave consumers to scalar reference, aarch64/scalar-delegate
  disposition, strict parity, and row-maintain gates with explicit final states
  (`restart/audit/totality/p1/1D-skinny-lessons.md:184`).
- The V4 primitive/kernel receiver table enumerates the concrete primitives
  requested by V3 rather than collapsing them into JSON-only classes
  (`restart/audit/totality/p1/1D-skinny-lessons.md:196-217`). It uses
  grammar-neutral or non-JSON proof where required: byte-class rows require a
  grammar-neutral byte-class use or generated consumer (`1D:200-201`), UTF-8
  requires a non-JSON witness or scoped JSON claim (`1D:206`), parse-that rows
  require non-JSON receiver/generated consumer/grammar-source facts where
  relevant (`1D:212-217`), and product-builder/hash rows are quarantined as
  delete or bench-only, never production equality proof (`1D:211`).
- The cited P2 source rows support that interpretation. P2-B states that the
  primitive admission process is grammar-neutral, but individual primitive
  claims are not grammar-neutral until policy source and consumer prove it
  (`restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:60-65`).
  P2-E says parse-that candidate APIs may expose no JSON/CSS/Sheets/BBNF names
  in generic APIs, lists non-JSON witness requirements for each candidate, and
  explicitly rejects JSON-only wording
  (`restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:223-236`).
- `1F-coherence-scan.md` carries the owner/receiver map required by V3. It maps
  runtime root, codegen profile roster, pass recognizer mining, pass
  materialization mining, Pattern H, CSS `CSS_GENERATED_RS`, CSS FNV hashes, and
  Lock 14 gate exclusions to owners, receivers, and proof expectations
  (`restart/audit/totality/p1/1F-coherence-scan.md:132-143`). D9/D10 specifically
  route through Sheets/BBNF-self/CSS recognizer facts and non-JSON role facts
  (`restart/audit/totality/p1/1F-coherence-scan.md:138-139`).
- `1E-locks-evidence.md` preserves the broader gate discipline: Lock 14 is
  drifted because scan exclusions hide leak-bearing files
  (`restart/audit/totality/p1/1E-locks-evidence.md:103`), and the Lock 14/16
  gate carrier requires included-root/exclusion reporting plus primitive
  source-status classification
  (`restart/audit/totality/p1/1E-locks-evidence.md:197-200`).

The live `rg` scan found no current inventory line that demotes D9/D10 into a
JSON-only lesson. The "JSON-only" tokens that do appear are negative or scoped
evidence: `1D:147` and `1D:190` reject JSON-only treatment for D9/D10, while
`1D:114` describes the current Lock 14 scan token universe as too JSON-only and
therefore a grammar-neutral gate failure.

## Findings

| id | disposition | finding | evidence |
|---|---|---|---|
| CH2-V4-001 | ACCEPT | The V4 packet preserves the V3 CH2 accepted surface. `P1-1B-D9` / `P1-1B-D10` remain grammar-neutral Lock 14 failures with non-JSON proof receivers, not JSON-only empirical lessons. | `restart/audit/totality/p1/1B-codegen-evidence.md:71`, `restart/audit/totality/p1/1B-codegen-evidence.md:79`, `restart/audit/totality/p1/1B-codegen-evidence.md:107-108`, `restart/audit/totality/p1/1D-skinny-lessons.md:147`, `restart/audit/totality/p1/1D-skinny-lessons.md:190`. |
| CH2-V4-002 | ACCEPT | The V4 receiver cost carrier remains grammar-neutral. D9/D10 route through CSS plus Sheets-or-BBNF-self generated metadata, and parse-that/primitive receivers require non-JSON witnesses, scoped claims, generated consumers, or explicit rejection states. | `restart/audit/totality/p1/1D-skinny-lessons.md:170-184`. |
| CH2-V4-003 | ACCEPT | The primitive/kernel receiver table enumerates named primitives and does not turn them into JSON-only lessons. Rows with possible JSON first consumers are explicitly scoped and do not claim fleet-wide closure; P2-B/P2-E require non-JSON witnesses or scoped language before generalization. | `restart/audit/totality/p1/1D-skinny-lessons.md:196-217`; `restart/skinny/tranches/sk-v15/research/p2/p2b-dav1d-process.md:60-65`; `restart/skinny/tranches/sk-v15/research/p2/p2e-parse-that-gaps.md:223-236`. |
| CH2-V4-004 | ACCEPT | 1D preserves the required separation among JSON-empirical guard facts, CSS audit-demoted facts, and grammar-neutral lessons. The primitive and Lock 14 rows sit in the grammar-neutral / unknown receiver path, not in the JSON guard section. | JSON rows at `restart/audit/totality/p1/1D-skinny-lessons.md:101-106`; grammar-neutral rows at `restart/audit/totality/p1/1D-skinny-lessons.md:134-147`; CSS audit-demoted rows at `restart/audit/totality/p1/1D-skinny-lessons.md:149-159`; unknown receiver rows at `restart/audit/totality/p1/1D-skinny-lessons.md:168`, `restart/audit/totality/p1/1D-skinny-lessons.md:225-229`. |
| CH2-V4-005 | ACCEPT | No grammar-name leak passes uncited. 1C flags runtime root/profile leaks and Pattern H; 1F maps leak owners/receivers; 1E requires gate exclusion reporting and primitive source-status classification. | `restart/audit/totality/p1/1C-runtime-evidence.md:61`, `restart/audit/totality/p1/1C-runtime-evidence.md:90-94`, `restart/audit/totality/p1/1C-runtime-evidence.md:118-123`; `restart/audit/totality/p1/1F-coherence-scan.md:132-143`; `restart/audit/totality/p1/1E-locks-evidence.md:103`, `restart/audit/totality/p1/1E-locks-evidence.md:197-200`. |

## Required Fold

None. CH2 is ACCEPT for SK-V15 T-P1 V4. Preserve `1D` rows `G-10`, `RC-07`,
`RC-10`, `RC-11`, the V4 primitive/kernel receiver table, and the `1F` Lock 14
owner/receiver map in V5; do not weaken the non-JSON proof receiver language
or broaden scoped JSON guard language into fleet-wide generality.
