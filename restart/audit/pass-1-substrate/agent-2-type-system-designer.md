# PASS-1 Sub-Agent 2: Type System Designer

## §1 Scope + Framing

Scope: BBNF type inference, bidirectional checking, layout constraints, host function typing, generic rules, map/chain expression typing, and mutation/value API typing.

Verdict: keep HM as the core, add bidirectional local checking for annotated forms, and use CSP-backed constrained unification for finite layout/host/backend choices. Per-grammar declaration crates are not the default architecture.

## §2 Per-Item Table

| Item | Pro | Con | Explication | Challenge | Verdict |
|---|---|---|---|---|---|
| HM core | Keeps grammar authoring annotation-light. | Recursive grammar constraints need staging. | README settles HM plus bidirectional and CSP-backed unification (`restart/README.md:258`-`restart/README.md:268`). | Preserve clear diagnostics. | KEEP |
| Bidirectional checking | Strong fit for `@host fn`, `@layout`, `@error`, and explicit rule signatures. | More span plumbing. | Expected types come from annotations and host signatures; inferred types synthesize from grammar structure. | Every generated constraint needs a source span. | KEEP |
| CSP-backed choices | Handles finite host/layout/materialization choices. | Solver errors can mask type errors. | CSP prunes finite domains; it does not replace type checking. | Separate type constraints from extraction-cost constraints. | KEEP |
| Host fn and chains | Covers reusable extension logic without grammar switches. | Requires purity/error/allocation metadata. | `@host fn`, chaining, and generics are in (`restart/README.md:145`-`restart/README.md:164`). | Avoid per-grammar crates. | REINVENT |
| Per-grammar declaration crates | Can carry special host glue. | Creates workspace sprawl and generic-crate leaks. | Amendment 01 rejects them as default (`restart-archive-2026-05-04/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md:7`-`restart-archive-2026-05-04/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md:24`). | Define rare escape review. | DISCARD DEFAULT |

## §3 Architectural Commitments Ratified

| Decision | Items |
|---|---|
| KEEP | HM inference; bidirectional checking; finite-choice CSP; typed `@host fn`; multi-function chains; generic rules; typed `@layout` and `@error`. |
| REINVENT | `TypeDesc`/layout split; map expressions as typed chain expressions; host registry as generic metadata; value type over tape/direct union. |
| DISCARD | Default per-grammar declaration crates; rewrite-mode type surface; arbitrary mutation side effects; hard-coded grammar profiles. |

## §4 New Facilities Proposed

| Proposed path | Purpose |
|---|---|
| `restart/specs/pass-1/type-system.md` | HM, bidirectional, and constrained-unification algorithm. |
| `restart/specs/pass-1/host-fn-metadata.md` | Signature, purity, allocation, error, and backend metadata schema. |
| `restart/specs/pass-1/generic-rules.md` | Generic rule parameters, instantiation, and diagnostic rules. |
| `restart/specs/pass-1/value-types.md` | Type view over tape/direct value union and read-write visitor. |

## §5 Cross-Cuts To PASS-2 / PASS-3

| Receiver | Handoff |
|---|---|
| PASS-2 | Module layout must separate semantic types, layout facts, host metadata, and backend materialization. |
| PASS-2 | Generic crates must not contain JSON/CSS/Sheets host branches; generated parser crates may contain emitted grammar-specific code. |
| PASS-3 | Backend host dispatch consumes typed host metadata and `HostChain`, not free-form call strings. |
| PASS-3 | User-facing errors need vocabulary from type, host, layout, and extraction failures. |

## §6 Risk + Mitigation Table

| Risk | Mitigation |
|---|---|
| Solver becomes a vague type checker. | Keep HM/bidirectional phases primary; CSP handles named finite-choice variables only. |
| Host metadata too weak, causing per-grammar crates to return. | Require generic primitive library plus workspace metadata and explicit `@host fn` composition (`AMENDMENT-01`: `restart-archive-2026-05-04/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md:34`-`restart-archive-2026-05-04/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md:55`). |
| Layout and semantic type vocabulary collapse. | Keep distinct type, layout, and materialization side tables. |
| Existing registry hard-codes grammars. | Replace `crates/ir/src/registry/strategy.rs:134`-`crates/ir/src/registry/strategy.rs:189` with generated metadata. |

## §7 Inheritance Ledger

| Legacy wave/substance | Survives | Dissolves | Re-anchors |
|---|---|---|---|
| BA.W2 layout lowering | Consumer-coupled layout work survives (`docs/tranches/BA/waves/W2.md:9`-`docs/tranches/BA/waves/W2.md:17`). | Transitional alias arguments do not bind greenfield names. | PASS-1 separates semantic types from layout facts. |
| BB host/generalization work | Grammar-agnostic cost and host decisions survive (`docs/tranches/BB/audit/W1-generated-parser-shape-generalised.md:91`-`docs/tranches/BB/audit/W1-generated-parser-shape-generalised.md:105`). | Per-grammar host namespace as a default generic-crate solution dissolves. | Host facts become metadata and generated code. |
| BC IR contract | Typed IR consumer contract survives (`docs/tranches/BC/BC.md:18`-`docs/tranches/BC/BC.md:24`). | Direct old Layout naming does not decide PASS-1 type names. | Backend IR gets typed side tables. |
| BD backend activation | Per-backend host resolution pressure survives (`docs/tranches/BD/BD.md:45`-`docs/tranches/BD/BD.md:47`). | Publication mechanics are not PASS-1. | Host metadata must support Rust, TS, and WASM later. |
