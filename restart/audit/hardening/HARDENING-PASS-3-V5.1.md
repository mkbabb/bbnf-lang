# HARDENING-PASS-3-V5.1

Phase 0.5 V5.1 verification for the PASS-3 amendment route.

## §1 Target and commits verified

| Field | Value |
|---|---|
| Verification target | `restart/audit/pass-3-runtime/PASS-3.md` |
| Verification output | `restart/audit/hardening/HARDENING-PASS-3-V5.1.md` |
| Amendment commit | `32c3dbf0 docs(restart/pass-3): wave-5.1 narrow amendment — runtime examples and diagnostic provenance` |
| Commit stat | `restart/audit/pass-3-runtime/PASS-3.md`, 83 insertions, 8 deletions |
| Route | PASS-3 narrow packet from `restart/audit/hardening/HARDENING-CONSOLIDATED-V5.md:393-413` |
| Assigned bundles | 3, 5, 6, 7, 9 |
| Verdict scope | Verify PASS-3 route closure only; do not adjudicate synthesis or PASS-1/PASS-2 write surfaces |

Required discipline check:

- `restart/prompts/AMENDMENT-DISPATCH.md:23-32` requires verify-then-patch and delta-only changes.
- `restart/prompts/AMENDMENT-DISPATCH.md:48` fails amendment dispatch when pre-filled surgery is re-authored.
- `restart/README.md:450-452` requires path:line citations and forbids soft hedging.
- `restart/locks/14-LOCKS.md:34` keeps tape as the substrate unioned with direct-to-struct and forbids OpenFrame-style parallel substrate drift.
- `restart/locks/14-LOCKS.md:46` settles `path`, `path-core`, and `path-ts`; stale `bbnf-path` names are legacy only.
- `restart/locks/14-LOCKS.md:48` is the SOTA competitor/dataset/platform authority.
- `restart/locks/14-LOCKS.md:52` rejects user-forced `@pratt` and `@simd`.
- `restart/locks/14-LOCKS.md:54` keeps incubating sister crates as path-deps until stability.
- `restart/locks/14-LOCKS.md:60` forbids grammar-specific code in generic crates and default per-grammar declaration crates.

V5 binding rows read:

- Consolidated V5 routes PASS-3 bundles 3, 5, 6, 7, and 9 at `restart/audit/hardening/HARDENING-CONSOLIDATED-V5.md:156-166`.
- Consolidated V5 names the PASS-3 route and its owned surfaces at `restart/audit/hardening/HARDENING-CONSOLIDATED-V5.md:170-174`.
- Consolidated V5 pre-fill checks for PASS-3 are at `restart/audit/hardening/HARDENING-CONSOLIDATED-V5.md:393-413`.
- Consolidated V5 V5.1 evidence gates are at `restart/audit/hardening/HARDENING-CONSOLIDATED-V5.md:443-461`.
- PASS-3 V5 punch rows are at `restart/audit/hardening/HARDENING-PASS-3-V5.md:274-287`.
- PASS-1 V5 binds runtime language for tape/OpenFrame, query examples, recovery, debug/DAP, WASM host routing, and rare fences at `restart/audit/hardening/HARDENING-PASS-1-V5.md:31-46`.
- PASS-1 V5 binds lookbehind aliases at `restart/audit/hardening/HARDENING-PASS-1-V5.md:89-91`.
- PASS-2 V5 binds runtime language for layout, tape, pointer/select, recovery metadata, and WASM host routing at `restart/audit/hardening/HARDENING-PASS-2-V5.md:72-84`.
- PASS-2 V5 binds the recognizer diagnostic drift risk at `restart/audit/hardening/HARDENING-PASS-2-V5.md:75-77`.

Runtime-language bindings carried into this verification:

| Binding | Upstream V5 anchor | PASS-3 close evidence | Verification note |
|---|---|---|---|
| Tape/direct identity and no `OpenFrame` return | `restart/audit/hardening/HARDENING-PASS-1-V5.md:31`, `restart/audit/hardening/HARDENING-PASS-2-V5.md:74` | `restart/audit/pass-3-runtime/PASS-3.md:80`, `restart/audit/pass-3-runtime/PASS-3.md:184-186` | PASS-3 keeps one visible identity shared by direct roots, tape, `ValueRef`, visitors, debugger, LSP, and playground. |
| `pointer!`/`select!` names and metadata validation | `restart/audit/hardening/HARDENING-PASS-1-V5.md:38`, `restart/audit/hardening/HARDENING-PASS-2-V5.md:76` | `restart/audit/pass-3-runtime/PASS-3.md:84-122`, `restart/audit/pass-3-runtime/PASS-3.md:133` | The amended example uses settled macro names and generated schema; `path!` does not appear. |
| `@error(recover = ...)` and recovery metadata | `restart/audit/hardening/HARDENING-PASS-1-V5.md:39-40`, `restart/audit/hardening/HARDENING-PASS-2-V5.md:80-81` | `restart/audit/pass-3-runtime/PASS-3.md:190`, `restart/audit/pass-3-runtime/PASS-3.md:212-240` | Standalone `@recover` remains alias-only if a migration parser keeps it; the worked path uses canonical recovery syntax. |
| Debug/DAP identity | `restart/audit/hardening/HARDENING-PASS-1-V5.md:43-44` | `restart/audit/pass-3-runtime/PASS-3.md:186` | The amended text uses `must`, names `SnapshotId` and `TapeId`, and routes fallback reasons to the debug-only channel. |
| WASM host primitives | `restart/audit/hardening/HARDENING-PASS-1-V5.md:46`, `restart/audit/hardening/HARDENING-PASS-2-V5.md:84` | `restart/audit/pass-3-runtime/PASS-3.md:437-445`, `restart/audit/pass-3-runtime/PASS-3.md:496` | PASS-3 names the H.W3 ABI surface and keeps measurement numbers out of PASS-3. |
| Recognizer directive drift | `restart/audit/hardening/HARDENING-PASS-1-V5.md:35`, `restart/audit/hardening/HARDENING-PASS-2-V5.md:75` | `restart/audit/pass-3-runtime/PASS-3.md:419-420`; broad scan found no `@pratt` or `@simd` | PASS-3 optimizer diagnostic rows do not teach retired user directives. |
| Lookbehind diagnostic aliases | `restart/audit/hardening/HARDENING-PASS-1-V5.md:89-91`, `restart/audit/hardening/HARDENING-PASS-2-V5.md:77` | `restart/audit/pass-3-runtime/PASS-3.md:429`, `restart/audit/pass-3-runtime/PASS-3.md:435` | PASS-3 binds numeric code, text alias, and vocabulary kind together. |
| Rare declaration-crate fence | `restart/audit/hardening/HARDENING-PASS-1-V5.md:65-66` | `restart/audit/pass-3-runtime/PASS-3.md:16`, `restart/audit/pass-3-runtime/PASS-3.md:37`, `restart/audit/pass-3-runtime/PASS-3.md:547` | PASS-3 rejects default per-grammar declaration crates and routes the rare escape through the architecture review form. |

## §2 Bundle closure table

| Bundle | Current PASS-3 evidence | Verdict | Rationale |
|---:|---|---|---|
| 3. PASS-3 stale citation/provenance cleanup | `restart/audit/pass-3-runtime/PASS-3.md:433`, `restart/audit/pass-3-runtime/PASS-3.md:449`, `restart/audit/pass-3-runtime/PASS-3.md:461`, stale scan command returned zero | CLOSED | The lowerer diagnostic now names `ir::grammar_ir` and `ir::backend_ir`, not `bbnf_ir::`; SOTA provenance cites Lock 8 at `restart/locks/14-LOCKS.md:48`; the benchmark rows keep source and platform attribution without changing thresholds. The explicit stale-pattern scan for `restart/locks/14-LOCKS.md:207`, `waves-v4`, `wave-4`, `Wave 4`, `bbnf_ir::`, `should reuse this identity`, `path!`, `@pratt`, and `@simd` returned zero matches in PASS-3. |
| 5. Complete `pointer!` + `select!` query worked example with diagnostics | `restart/audit/pass-3-runtime/PASS-3.md:94-122`, `restart/audit/pass-3-runtime/PASS-3.md:133`, `restart/audit/pass-3-runtime/PASS-3.md:140`, `restart/audit/pass-3-runtime/PASS-3.md:422-424` | CLOSED | The worked path starts from `Json::parse`, binds `root` as `ValueRef`, validates `pointer!(Json, ["orders", 0, "sku"])`, validates a `select!(Json, ...)` structural query over the same schema, and names the success projection. Failure coverage names `BBNF-POINTER001`, `BBNF-POINTER002`, and `BBNF-POINTER003`; registry deletion remains a hard gate rather than a deferral. |
| 6. Incremental parse + `@error(recover)` + debug/DAP identity walkthroughs | `restart/audit/pass-3-runtime/PASS-3.md:186`, `restart/audit/pass-3-runtime/PASS-3.md:190-240`, `restart/audit/pass-3-runtime/PASS-3.md:244-253`, `restart/audit/pass-3-runtime/PASS-3.md:428`, `restart/audit/pass-3-runtime/PASS-3.md:502-503` | CLOSED | The amendment converts debug/DAP identity to mandatory language, requires breakpoint/step/hover/trace events to carry snapshot/tape/node/span identity, and confines span-only fallback to parse-failed regions. The recovery walkthrough names `DocumentSnapshot`, `ReparsePlan`, dirty range `31..32`, anchors, recovered tape node insertion, `BBNF-RECOVERY001`, fallback reason, bench ledger, and default LSP silence. |
| 7. WASM host primitive runtime route | `restart/audit/pass-3-runtime/PASS-3.md:432`, `restart/audit/pass-3-runtime/PASS-3.md:437-445`, `restart/audit/pass-3-runtime/PASS-3.md:496` | CLOSED | PASS-3 no longer leaves WASM host primitives as an unqualified diagnostic row. It routes grammar-author syntax through block-bodied `@host fn`, host chains, `host::primitives`, workspace metadata, PASS-2 `CallHost`/host-chain records, and the H.W3 ABI descriptor. It names exported function names, host-call shape, marshalling, primitive coverage, scalar/SIMD parity, and `BBNF-HOST003` while explicitly avoiding invented `{N}`/`{M}` measurements. |
| 9. Rare declaration-crate fence and diagnostic alias polish | `restart/audit/pass-3-runtime/PASS-3.md:16`, `restart/audit/pass-3-runtime/PASS-3.md:37`, `restart/audit/pass-3-runtime/PASS-3.md:429`, `restart/audit/pass-3-runtime/PASS-3.md:433-435`, `restart/audit/pass-3-runtime/PASS-3.md:547` | CLOSED | PASS-3 states no per-grammar declaration crates exist by default, keeps declaration crates in DISCARD posture, routes rare host adapters through an eight-field review form including declaration location, deletion path, reviewer, and receiving gate, binds `BBNF1004` / `BBNF-LOOKBEHIND-WIDTH` / `LookbehindWidth` in one ledger row, and removes the stale `bbnf_ir::` prefix. PASS-1 owns the rare-fence wording outside this route; PASS-3 now carries a precise receiver and gate. |

Bundle 3 stale-match classification:

- Required stale scan command returned zero.
- `ParseStream`, `rewrite-mode`, Unicode algebra, and `bbnf-path` still occur in PASS-3 only as stale conflict, discarded surface, or legacy evidence context at `restart/audit/pass-3-runtime/PASS-3.md:16-23`, `restart/audit/pass-3-runtime/PASS-3.md:32`, `restart/audit/pass-3-runtime/PASS-3.md:84`, `restart/audit/pass-3-runtime/PASS-3.md:493`, `restart/audit/pass-3-runtime/PASS-3.md:497`, `restart/audit/pass-3-runtime/PASS-3.md:529`, and `restart/audit/pass-3-runtime/PASS-3.md:542`.
- No positive settled-state use of `ParseStream`, rewrite-mode, grammar Unicode algebra, `path!`, `@pratt`, `@simd`, or `bbnf_ir::` remains in PASS-3.

Per-bundle close checks:

Bundle 3:

- `restart/audit/pass-3-runtime/PASS-3.md:449` cites Lock 8 rather than Lock 14 or stale line 207.
- `restart/audit/pass-3-runtime/PASS-3.md:433` uses current IR vocabulary and preserves the BIR producer/lowerer boundary.
- `restart/audit/pass-3-runtime/PASS-3.md:461` carries attribution discipline for benchmark rows without changing the performance model.
- The stale-match command returned zero, so there is no residual PASS-3 patch item for this route.

Bundle 5:

- `restart/audit/pass-3-runtime/PASS-3.md:94-105` gives one concrete JSON query setup with both macros.
- `restart/audit/pass-3-runtime/PASS-3.md:108-116` explains compile-time validation and runtime `ValueRef` projection.
- `restart/audit/pass-3-runtime/PASS-3.md:118-122` supplies typo, ambiguous-grammar, and stale-schema diagnostics.
- `restart/audit/pass-3-runtime/PASS-3.md:133` keeps generated metadata as the only validation surface.

Bundle 6:

- `restart/audit/pass-3-runtime/PASS-3.md:186` hardens debug/DAP identity from recommendation to requirement.
- `restart/audit/pass-3-runtime/PASS-3.md:212-230` ties canonical recovery syntax to dirty range and recovered tape state.
- `restart/audit/pass-3-runtime/PASS-3.md:235-240` records fallback accounting and debug-only disclosure.
- `restart/audit/pass-3-runtime/PASS-3.md:244-253` turns fallback rates and LSP silence into close gates.

Bundle 7:

- `restart/audit/pass-3-runtime/PASS-3.md:437-439` keeps grammar syntax unchanged.
- `restart/audit/pass-3-runtime/PASS-3.md:440-443` names PASS-2 lowerer records and H.W3 ABI fields.
- `restart/audit/pass-3-runtime/PASS-3.md:444-445` blocks invented latency and size numbers.
- `restart/audit/pass-3-runtime/PASS-3.md:496` repeats the receiving gate in the cross-pass hand-off table.

Bundle 9:

- `restart/audit/pass-3-runtime/PASS-3.md:429` binds `BBNF1004`, `BBNF-LOOKBEHIND-WIDTH`, and `LookbehindWidth`.
- `restart/audit/pass-3-runtime/PASS-3.md:433` removes the stale crate prefix and keeps lowerer import denial intact.
- `restart/audit/pass-3-runtime/PASS-3.md:547` names the rare escape review fields and distinguishes Lock 11 incubation from declaration-crate defaulting.
- PASS-1 wording outside this file remains for the PASS-1/PASS-2 verifier; PASS-3's route has a receiver, blocker, and gate.

## §3 Pathology regression scan

| Lens | Spot check | Evidence | Result | Classification |
|---|---|---|---|---|
| F | Hedged runtime invariant | `restart/audit/pass-3-runtime/PASS-3.md:186`; required stale scan for `should reuse this identity` returned zero | PASS | The V5 advisory phrase was replaced by mandatory debug/DAP identity language and a concrete acceptance gate. |
| F | Closure bias around examples | `restart/audit/pass-3-runtime/PASS-3.md:94-122`, `restart/audit/pass-3-runtime/PASS-3.md:209-240` | PASS | PASS-3 no longer relies on tables alone for pointer/select or incremental recovery; both now include worked paths and diagnostics. |
| F | Plausible but retired recognizer syntax | `restart/audit/pass-3-runtime/PASS-3.md:419-420`; broad scan found no `@pratt` or `@simd` | PASS | Optimizer diagnostics discuss automatic Pratt/SIMD selection and fallback, not user-forced directives. |
| G | Query API overfit to old `path!` shape | `restart/audit/pass-3-runtime/PASS-3.md:84-92`, `restart/audit/pass-3-runtime/PASS-3.md:101-122`, stale scan for `path!` returned zero | PASS | Legacy `bbnf-path` citations are framed as archaeology; current API uses `pointer!`, `select!`, generated metadata, and `ValueRef`. |
| G | Editor-incremental overfit without BBNF-specific recovery | `restart/audit/pass-3-runtime/PASS-3.md:212-240`, `restart/audit/pass-3-runtime/PASS-3.md:248-253` | PASS | The example is grammar-local: a JSON `member` rule, `@error(recover = ...)`, dirty range, anchors, recovered tape node, fallback ledger, and LSP policy. |
| G | WASM parity overfit | `restart/audit/pass-3-runtime/PASS-3.md:437-445`, `restart/audit/pass-3-runtime/PASS-3.md:496` | PASS | PASS-3 routes ABI shape and parity coverage to H.W3 but does not invent host latency or size numbers. |
| H | Wrong lock provenance | `restart/audit/pass-3-runtime/PASS-3.md:449`, `restart/locks/14-LOCKS.md:48`; stale scan for `restart/locks/14-LOCKS.md:207` returned zero | PASS | The Lock 14/line-207 citation fault is gone; SOTA rule now cites Lock 8. |
| H | Lowerer diagnostic stale crate prefix | `restart/audit/pass-3-runtime/PASS-3.md:433`; stale scan for `bbnf_ir::` returned zero | PASS | Diagnostic provenance now uses current `ir::grammar_ir` and `ir::backend_ir` vocabulary. |
| H | Lookbehind alias provenance | `restart/audit/pass-3-runtime/PASS-3.md:429`, `restart/audit/pass-3-runtime/PASS-3.md:435`; PASS-1 V5 alias binding at `restart/audit/hardening/HARDENING-PASS-1-V5.md:89-91` | PASS | PASS-3 binds numeric code, human alias, and vocabulary kind together and names PASS-1 as owner. |
| H | Legacy evidence leaking into authority | `restart/audit/pass-3-runtime/PASS-3.md:84`, `restart/audit/pass-3-runtime/PASS-3.md:545` | PASS | Legacy package names remain only in deletion/archaeology context; current crate names are `path`, `path-core`, and `path-ts`. |

No new F/G/H regression was found in the amended PASS-3 text.

## §4 Gate rerun

Required commands run:

```text
git status --short
```

Result: clean before report creation.

```text
git show --stat --oneline 32c3dbf0
```

Result: `32c3dbf0 docs(restart/pass-3): wave-5.1 narrow amendment — runtime examples and diagnostic provenance`; one file changed, `restart/audit/pass-3-runtime/PASS-3.md`, with 83 insertions and 8 deletions.

```text
rg -n "path!|@pratt|@simd|OpenFrame|LayoutFacts|LayoutSink|passes::layout|pointer!|select!|LookbehindWidth|BBNF-LOOKBEHIND-WIDTH|BBNF1004|@host fn|waves-v4|wave-4|Wave 4|WASM|incremental|recover|DAP|debug|diagnostic|incubat|rare|bbnf_ir::|should reuse this identity" restart/audit/pass-3-runtime/PASS-3.md
```

Result: expected positive evidence for `pointer!`, `select!`, lookbehind aliases, `@host fn`, WASM, incremental recovery, DAP/debug, diagnostics, and rare escape route. No `path!`, `@pratt`, `@simd`, `OpenFrame`, `waves-v4`, `wave-4`, `Wave 4`, `bbnf_ir::`, or `should reuse this identity` hits were present in the output.

```text
rg -n "restart/locks/14-LOCKS.md:207|waves-v4|wave-4|Wave 4|bbnf_ir::|should reuse this identity|path!|@pratt|@simd" restart/audit/pass-3-runtime/PASS-3.md
```

Result: zero matches; this is the expected stale-match result.

```text
git diff --check
```

Result before report creation: clean. Result after report creation: clean.

Additional focused checks run:

```text
rg -n "ParseStream|rewrite-mode|Unicode class algebra|@recover|bbnf-path|bbnf-path-ts|bbnf-test-fixtures|path!|@pratt|@simd|OpenFrame|bbnf_ir::|should reuse this identity|restart/locks/14-LOCKS.md:207|waves-v4|wave-4|Wave 4" restart/audit/pass-3-runtime/PASS-3.md
```

Result: matches are stale conflict, DISCARD, legacy evidence, or alias-migration context only. No positive settled-state drift.

```text
rg -n "Pointer/select worked path|ValueRef|json.path-schema.toml|BBNF-POINTER001|BBNF-POINTER002|BBNF-POINTER003|pointer!|select!" restart/audit/pass-3-runtime/PASS-3.md
```

Result: success path, failure path, generated schema, `ValueRef`, both macros, and all three pointer diagnostics are present.

```text
rg -n "Incremental recovery worked path|@error\(recover|dirty range|ReparsePlan|fallback reason|BBNF-RECOVERY001|BBNF_LSP_DEBUG|LSP user-facing output policy|Debug and DAP must reuse" restart/audit/pass-3-runtime/PASS-3.md
```

Result: recovery syntax, `ReparsePlan`, dirty range, fallback reason, recovery diagnostic, debug channel, LSP silence policy, and mandatory DAP/debug identity are present.

```text
rg -n "WASM host primitive route|BBNF-HOST003|H\.W3|exported function names|host-call shape|marshalling|scalar/SIMD parity|\{N\}|\{M\}|BBNF1004|BBNF-LOOKBEHIND-WIDTH|LookbehindWidth|ir::grammar_ir|ir::backend_ir|Rare host adapter escape-valve policy|deletion path|reviewer" restart/audit/pass-3-runtime/PASS-3.md
```

Result: WASM route, H.W3 ABI fields, no-measurement guard, lookbehind alias chain, current IR paths, and rare escape review fields are present.

## §5 Residue ledger

none

## §6 Final verdict

READY.

All assigned PASS-3-route bundles are closed in the amended target, and the amended text does not introduce new F/G/H LLM-pathology drift. The only remaining obligations visible in PASS-3 are receiver-gated implementation work in later tranches: generated metadata, runtime identity tests, H.W3 ABI evidence, cookbook/runtime diagnostic parity, registry deletion, and benchmark output. Those are ordinary receiving gates, not V5.1 amendment residue.

## §7 Closing posture

PASS-3 is fit to enter synthesis for this route.

The text now teaches the settled runtime surface: tape/direct identity, `ValueRef`, generated metadata, `pointer!`, `select!`, `@error(recover = ...)`, debug/DAP snapshot identity, and host primitive routing. Stale forms are either absent or explicitly fenced as stale inputs, deletion archaeology, DISCARD items, or receiver-gated negative checks.
