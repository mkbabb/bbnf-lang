# PASS-1 Sub-Agent 5: Grammar Extension Designer

## §1 Scope + Framing

Scope: BBNF grammar surface, lookbehind, rewrite-mode conflict, Unicode class algebra conflict, `@host fn`, chaining, generics, `@error`, `@layout`, Pratt/SIMD detection, and closure semantics.

Verdict: keep lookbehind, `@host fn`, multi-function chaining, generic rules, `@error`, and `@layout`. Reject rewrite-mode. Defer Unicode class algebra to the regex layer rather than BBNF grammar syntax.

## §2 Per-Item Table

| Item | Pro | Con | Explication | Challenge | Verdict |
|---|---|---|---|---|---|
| Lookbehind | Needed for lexical constraints. | Requires finite-width legality. | README accepts lookbehind (`restart/README.md:124`-`restart/README.md:132`). | Diagnostics must separate semantic error from unsupported backend. | KEEP |
| Rewrite-mode | Could express transforms inline. | Turns BBNF into a rewrite language. | README rejects rewrite-mode (`restart/README.md:134`-`restart/README.md:137`). | Old prompt text still asks about it. | DISCARD |
| Unicode algebra | Useful in regex classes. | Grammar layer would own regex semantics. | README routes it to regex (`restart/README.md:139`-`restart/README.md:143`). | Need clean regex-layer integration. | DEFER |
| `@host fn` | Typed host integration. | Needs metadata. | README keeps `@host fn` (`restart/README.md:145`-`restart/README.md:152`). | Avoid per-grammar crates. | KEEP |
| Chaining/generics | Solves composition and reuse. | Harder inference. | README keeps chaining and generics (`restart/README.md:154`-`restart/README.md:164`); ffuzzy notes chaining is a real gap (`docs/ffuzzy.md:648`-`docs/ffuzzy.md:672`). | Lower through typed expressions. | KEEP |
| `@error` / `@layout` | Gives grammar authors controlled directive surfaces. | Needs side tables and diagnostics. | README accepts both (`restart/README.md:166`-`restart/README.md:174`). | Keep directive effects typed. | KEEP |

## §3 Architectural Commitments Ratified

| Decision | Items |
|---|---|
| KEEP | Lookbehind; `@host fn`; chains; generics; `@error`; `@layout`; Pratt/SIMD auto-detection facts. |
| REINVENT | ffuzzy primitives as typed expressions or regex-layer features; closures/generic rules as compile-time expansion plus typed checking. |
| DISCARD | Rewrite-mode; grammar-level Unicode algebra; `@pratt`/`@simd` mandatory directives; extension-specific generic-crate branches. |

## §4 New Facilities Proposed

| Proposed path | Purpose |
|---|---|
| `restart/specs/pass-1/bbnf-grammar.md` | Canonical grammar surface and EBNF. |
| `restart/specs/pass-1/lookbehind.md` | Finite-width legality and lowering. |
| `restart/specs/pass-1/host-chains.md` | Multi-function chaining semantics. |
| `restart/specs/pass-1/directives.md` | `@host fn`, `@error`, `@layout`, generic rule metadata. |

## §5 Cross-Cuts To PASS-2 / PASS-3

| Receiver | Handoff |
|---|---|
| PASS-2 | Parser/source modules must parse accepted directives and reject rewrite-mode with clear diagnostics. |
| PASS-2 | Regex module ownership must handle Unicode algebra below BBNF. |
| PASS-3 | Host chains lower to backend `HostChain` and typed call metadata. |
| PASS-3 | Error/layout directives become user-facing diagnostics and backend layout operations. |

## §6 Risk + Mitigation Table

| Risk | Mitigation |
|---|---|
| Stale prompt reintroduces rewrite-mode. | Record `restart/prompts/PASS-1-SUBSTRATE.md:3`, `restart/prompts/PASS-1-SUBSTRATE.md:31`, and `restart/prompts/PASS-1-SUBSTRATE.md:66` as stale against README. |
| Unicode algebra gets implemented twice. | Make BBNF reference regex classes; regex owns algebra. |
| Host chains require new e-graph node. | Follow ffuzzy correction that composition can derive through language support (`docs/ffuzzy.md:648`-`docs/ffuzzy.md:672`). |
| Generic rules become macros without types. | Type-check after expansion/instantiation and before Backend IR extraction. |

## §7 Inheritance Ledger

| Legacy wave/substance | Survives | Dissolves | Re-anchors |
|---|---|---|---|
| ffuzzy primitive research | Lookbehind and chaining evidence survive (`docs/ffuzzy.md:258`-`docs/ffuzzy.md:360`, `docs/ffuzzy.md:648`-`docs/ffuzzy.md:672`). | Three-primitives-as-settled dissolves (`docs/ffuzzy.md:616`-`docs/ffuzzy.md:644`). | Rewrite work becomes map/host transforms; Unicode belongs to regex. |
| BB Pratt/SIMD auto-detect | Auto-detect survives (`docs/tranches/BB/BB.md:5`). | User directives for Pratt/SIMD dissolve. | Recognizer facts feed CSP/cost. |
| BC regex endpoint | Regex endpoint pressure survives (`docs/tranches/BC/BC.md:21`-`docs/tranches/BC/BC.md:24`). | Exact endpoint naming is later. | Unicode algebra is a regex-layer contract. |
| BD backend activation | Host-fn per-backend resolution pressure survives (`docs/tranches/BD/BD.md:45`). | TS/WASM implementation is later. | Host directives must be backend-neutral. |
