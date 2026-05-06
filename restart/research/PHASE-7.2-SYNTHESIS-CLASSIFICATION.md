# Phase 7.2 SYNTHESIS Classification — MASTER-PLAN + MIGRATION cascade

This is the classification ledger for the Phase 7.2 SYNTHESIS trio fold against MASTER-PLAN and MIGRATION. Phase 7.1 landed locks (`adbaaaa0`) plus ARCHITECTURE §7.5 Backend trait + §13.1 lint manifest plus PASS-1 grammar amendments (`9cb92284`). The amendments cascade into MASTER-PLAN and MIGRATION at the sites tabulated below.

ARCHITECTURE.md and `restart/locks/14-LOCKS.md` were Phase-7.1 owned. PASS-1.md, PASS-2.md, PASS-3.md are sister Phase 7.2 fold agents. This classification covers MASTER-PLAN + MIGRATION cascade only.

## §A — `pointer!` → `path!` rename ledger

| Site | File:line | Verbatim today | Surgery |
|---|---|---|---|
| A1.M1 | `MASTER-PLAN.md:168` | "G \| Path, Value, Visitor \| 5 \| `pointer!`, `select!`, ..." | rename to `path!`, `select!` |
| A1.M2 | `MASTER-PLAN.md:221` | "`pointer!` and `select!` validate against the generated yaml path schema." | rename `pointer!` → `path!` |
| A1.M3 | `MASTER-PLAN.md:436` | "README path API. \| `pointer!`, `select!`, visitor mutation" | rename to `path!`, `select!` |
| A1.M4 | `MASTER-PLAN.md:445` | "G.W1 \| Rust `pointer!` and `select!`. \| Compile-time path diagnostics work." | rename `pointer!` → `path!` |
| A1.M5 | `MASTER-PLAN.md:784` | "PASS-3 API docs ... `pointer!`, `select!`, visitor, language-server omit committed string diagnostics." | rename to `path!`, `select!` |
| A1.M6 | `MASTER-PLAN.md:799` (cookbook row) | `pointer!`, `select!` cookbook row; `pointer!(Json => "/...")` canonical spelling; cookbook anchor `cookbook/path-pointer.md`; codes `BBNF-POINTER-UNKNOWN-SEGMENT`, `BBNF-POINTER-GRAMMAR-MISMATCH` | full rewrite: `path!` + `select!`; `path!(Json => "/...")`; cookbook anchor `cookbook/path-dsl.md`; codes `BBNF-PATH-UNKNOWN-SEGMENT`, `BBNF-PATH-GRAMMAR-MISMATCH` |
| A2.M1 | MIGRATION.md (no direct `pointer!` macro hits) | — | per audit #3 §4.4: rename ledger receives macro change at A.W1; no MIGRATION rewrite needed beyond §B parse-that-regex cascade. |

`pointer!` macro count in MASTER-PLAN today: **6 macro citations + 1 cookbook anchor + 2 diagnostic codes**. After fold: zero `pointer!` positive surface; archaeology only via the codes' `BBNF-PATH*` rename trail.

## §B — parse-that-regex cascade (`bbnf-regex` → `parse-that-regex`; `regex-automata` oracle removal)

Per Lock 11 amendment (Phase 7.1, line 54): `parse-that` is the canonical name for the published parser combinator + regex family; the legacy `bbnf-regex` crate renames to `parse-that-regex` and publishes as such.

Per audit #4 + #6: `regex-automata` is retired entirely; parse-that-regex owns the parity-test corpus internally.

| Site | File:line | Verbatim today | Surgery |
|---|---|---|---|
| B1.M1 | `MASTER-PLAN.md:477` | "...verifier-before-tape emission, and `regex-automata` oracle parity for regex fixtures." | rewrite: "...verifier-before-tape emission, and parse-that-regex internal cross-engine parity for regex fixtures." |
| B1.M2 | `MASTER-PLAN.md:499` | `cargo test -p parse-that regex_automata_oracle` | rewrite: `cargo test -p parse-that-regex cross_engine_parity` |
| B1.M3 | `MASTER-PLAN.md:777` | "`parse-that/regex` reimplements regex engines without grammar-owned delta or parity evidence... compare grammar HIR/verifier integration against `regex-automata` oracle cases for..." | rewrite: `parse-that-regex` (the crate); internal cross-engine parity (NFA vs lazy DFA vs full DFA vs VM) replaces oracle citation |
| B2.M1 | `MIGRATION.md:506` | "...PASS contracts, and the `regex-automata` oracle lane rather than..." | rewrite: "PASS contracts, and parse-that-regex internal cross-engine parity rather than..." |
| B2.M2 | `MIGRATION.md:594` | "`crates/parse-that` \| Regex/Unicode substrate below BBNF, with grammar-owned HIR/verifier integration and `regex-automata` as oracle/reference until bespoke parity is proven." | rewrite: route regex sub-crate to `parse-that-regex`; oracle-citation drops; cross-engine parity is internal. |
| B2.M3 | `MIGRATION.md:665-686` (tranche table H) | (today) "regex oracle parity" | rewrite to "regex internal cross-engine parity" via `parse-that-regex` |

Sites in MASTER-PLAN that mention `parse-that/regex` (sub-path inside the parse-that crate today) survive renamed to `parse-that-regex` (sibling sub-crate per Lock 11). The canonical surface name is `parse-that-regex` post-fold.

## §C — TS + WASM defer cascade (Q5 + Q6 + Phase-7.1 Lock 5/8)

Per Lock 5 amendment: TS + WASM backends defer post-V1; V1 ships Rust impl only via `Backend` trait (ARCH §7.5).

Per Lock 8 amendment: V1 SOTA close gates measure Rust line only; WASM SOTA defers post-V1; no measurement-pending WASM anchor lands in V1.

Per Lock 11 amendment: `path-ts` defers post-V1 alongside the principled TS-native parse+runtime fork; J.W3 publishes the stable surface `bbnf`, `bbnf-cli`, `bbnf-language-server`, `path`, `path-core`, `parse-that-regex`; sister crates (`egraph`, `egraph-derive`, `csp-solver`, `parse-that`) publish at J.W3 only after the 2-tranche stability gate.

| Site | File:line | Verbatim today | Surgery |
|---|---|---|---|
| C1.M1 | `MASTER-PLAN.md:222` (yaml H row) | "H.W3 evaluates yaml host primitives for WASM only if metadata enables the WASM lowerer. \| WASM host primitive ABI matrix records exported names, host-call shape, marshalling rule, primitive coverage, and scalar/SIMD parity; latency and size remain H.W3 measurements." | rewrite: WASM defers post-V1 via `WasmBackend: Backend` impl per ARCH §7.5; H.W3 measures Rust line only; the WASM ABI matrix routes to V2 alongside Lock 11 V2 publication carry. |
| C1.M2 | `MASTER-PLAN.md:169` (tranche H title) | "H \| Pratt, SIMD, WASM \| 6 \| Auto-detected Pratt/SIMD and WASM V1 pass early SOTA gates." | rewrite: "Auto-detected Pratt/SIMD pass early SOTA gates; WASM defers post-V1 via `WasmBackend: Backend` per ARCH §7.5." |
| C1.M3 | `MASTER-PLAN.md:189` (tranche calendar H row) | "H \| 8 \| Lock 10, PASS-2 SIMD/WASM, SOTA corpus. \| J. \| Pratt, SIMD, WASM V1, early perf." | rewrite: drop WASM V1 from H ownership; WASM lower-and-bench programme awaits V2 `WasmBackend` impl. |
| C1.M4 | `MASTER-PLAN.md:204` (forbidden-output) | "H \| Auto Pratt/SIMD and WASM V1. \| `@pratt` or `@simd` grammar directives." | rewrite: drop "and WASM V1" from H required outputs. |
| C1.M5 | `MASTER-PLAN.md:459-503` (tranche H section) | H.W3 WASM V1 wave with `{N}`/`{M}` placeholders, ABI matrix, etc. | rewrite: H.W3 retires; WASM lower-and-bench is V2; H wave count drops 6 → 5. ABI matrix moves to V2 alongside `WasmBackend: Backend`. |
| C2.M1 | `MASTER-PLAN.md:556` (J.W3 publication row) | stable surface includes `path-ts`; sister crates list includes `parse-that` (separate from regex sub-crate) | rewrite: stable surface is `bbnf`, `bbnf-cli`, `bbnf-language-server`, `bbnf-bench`, `path`, `path-core`, `parse-that-regex`; `path-ts` defers post-V1; sister crates `egraph`, `egraph-derive`, `csp-solver`, `parse-that` publish at J.W3 only after 2-tranche stability gate. |
| C2.M2 | `MASTER-PLAN.md:131-136` (SOTA close rows) | rows include WASM-affected lightning-css gates | retain Rust-line gates (M1 Pro native Rust release); retire any WASM-affected row. |
| C2.M3 | `MASTER-PLAN.md:138-150` (benchmark schema) | retain Rust-line schema | unchanged at this section; bench WASM rows defer post-V1. |
| C2.M4 | `MASTER-PLAN.md:691` (LOC trajectory H.W3 row) | `WASM-attributed LOC reported separately` | rewrite: H.W3 row retires; WASM LOC accounting is V2 concern. |
| C2.M5 | `MASTER-PLAN.md:780` (TS production carry row) | "TS production \| G/I/J \| TS path emitter or schema produces TS without grammar names in source." | rewrite: TS production routes to V2 carry; V1 carry retires the row or rewrites receiver to "V2 amendment". |
| C2.M6 | `MASTER-PLAN.md:787` (`path-ts` schema row) | "`path-ts` schema \| G \| TS schema does not derive from the same `path-core` semantics..." | rewrite: defer post-V1; route receiver to V2 amendment. |
| C2.M7 | `MASTER-PLAN.md:790` (`path-ts` package publication timing) | "...J.W3 dry-run records `path-ts` only after J.W0 parity matrix passes..." | rewrite: `path-ts` defers post-V1; row retires or routes to V2 amendment. |
| C2.M8 | `MASTER-PLAN.md:788` (WASM ABI carry row) | "WASM ABI \| H/J \| WASM exported ABI not specified for V1 binding... H.W3 records exported function names..." | rewrite: WASM ABI defers post-V1 alongside `WasmBackend: Backend` V2 impl. |
| C2.M9 | `MASTER-PLAN.md:782` (BD parity carry row) | "BD parity \| F/J \| BD-equivalent parity matrix not run for Rust/VM/WASM V1 backends." | rewrite: drop "WASM V1" from V1 parity scope; V1 parity is Rust/VM only; WASM parity defers post-V1. |
| C3.M1 | `MIGRATION.md:69` (analysis section) | TS/WASM emergence in BD prose | retain inheritance prose; flag BD inheritance as TS/WASM defers post-V1. |
| C3.M2 | `MIGRATION.md:497` (PASS-2 SIMD WASM row) | "PASS-2 requires SIMD coverage across scalar, NEON, AVX2, AVX512, and WASM SIMD paths" | retain SIMD coverage; flag WASM SIMD as V2 once `WasmBackend` lands. |
| C3.M3 | `MIGRATION.md:659` (BD inheritance row) | "BD \| Fixture package, cross-backend matrix, publication order. \| Premature TypeScript production if not backed by current lowerer contract." | rewrite: BD inheritance receives TS/WASM as principled V2 fork, not V1 carry. |
| C3.M4 | `MIGRATION.md:680` (tranche-level migration row H) | "H \| Pratt, verifier-bound exact/prefilter SIMD, regex oracle parity, WASM V1, SOTA early gates." | rewrite: drop "WASM V1" from H scope; H carries Pratt + SIMD + parse-that-regex internal cross-engine parity + Rust-line SOTA early gates. |

## §D — Tier 4 architectural prerequisites (audit #8; ARCH-side largely landed Phase 7.1)

| Site | File:line | Surgery |
|---|---|---|
| D1.M1 | `MASTER-PLAN.md:316` (C.W4 row) | retain "rewrite budget policy" bullet; cross-reference ARCH §10.1 (rewrite-budget categories now landed in Phase 7.1). C.W4 *consumes* the policy and verifies, no longer authors. |
| D2.M1 | `MASTER-PLAN.md:248` + §13 lint discipline | A.W4 `cargo xtask lint-grammar-generalization` consumes the lint manifest in ARCH §13.1 (Phase 7.1 landed); no MASTER-PLAN authoring. |
| D3.M1 | `MASTER-PLAN.md:771` (declaration-crate carry row) | row references the 8-field review form template; cross-reference the template anchor in ARCH §13 (Phase 7.1 owned). |
| D4.M1 | `MASTER-PLAN.md:798-806` (cookbook §25) | per-page contract template lifted to ARCH or `restart/templates/` (Phase 7.1 owned); §25 cookbook rows cross-reference. |

D items are mostly cascade-only because Phase 7.1 landed the ARCHITECTURE-side authoring. MASTER-PLAN consumes via cross-reference; no new content lands.

## §E — Lock cascade per audit #7

| Site | File:line | Surgery |
|---|---|---|
| E1 | various tranche references citing "TS+WASM at BD+" or BD-prefixed slots | rewrite: A-J tranche scheme; "BD+" retires per Lock 5 amendment. **Verification: rg `BD\+|BA\.W|BC\.W|BB\.W` MASTER-PLAN.md MIGRATION.md returns archaeology rows only (BA-BD inheritance map citations).** Active references retire. |
| E2.M1 | `MASTER-PLAN.md:556` (J.W3 publication row, see C2.M1) | per Lock 11 amendment: stable surface = `bbnf`, `bbnf-cli`, `bbnf-language-server`, `bbnf-bench`, `path`, `path-core`, `parse-that-regex`; sister crates `egraph`, `egraph-derive`, `csp-solver`, `parse-that` publish at J.W3 only after 2-tranche stability gate. |
| E2.M2 | `MIGRATION.md:69` "BD \| Fixture package" | rewrite: BD inheritance row continues; BD-prefixed inheritance map lives at `restart/inheritance/INDEX.md` and remains archaeology only. |
| E3 | `MASTER-PLAN.md:244` (A.W0) + `MASTER-PLAN.md:603` (commit shape) + `MIGRATION.md:519-521` (archive procedure) + `MIGRATION.md:627-642` (commit chain) | retain `pre-restart-2026-05-04` tag citation per Lock 12 amendment; verify `BA.W0` → `A.W0` if any drift; **today the active MASTER-PLAN already uses `A.W0`** — drift check returns clean. |

## §F — Function-value cascade (audit #2 + #6; D wave growth + C wave expansion)

Per V1-FOLD §5: D wave count grows 5 → 6 to absorb function-value lowering (D.W6 = function-typed `@host fn` parameter lowering + closure environment frame lowering + match/tuple lowering).

Per V1-FOLD §5: C-tranche absorbs DK13 + GADT substrate + row poly + schema miner + CHR + Backend trait surface; wave count unchanged but wave content grows.

| Site | File:line | Surgery |
|---|---|---|
| F1.M1 | `MASTER-PLAN.md:165` (tranche table D row) | grow stub waves 5 → 6; new D.W5 absorbs function values + lambdas + match + tuple grammar surface (per Lock 10 amendment). |
| F1.M2 | `MASTER-PLAN.md:330-361` (Tranche D section) | retain D.W0-D.W4; add D.W5 — function-typed `@host fn` parameter + closure environment + match/tuple expression lowering. |
| F1.M3 | `MASTER-PLAN.md:185` (calendar D row) | unchanged; calendar slot is "4"; wave count growth absorbs into the same slot. |
| F2.M1 | `MASTER-PLAN.md:164` (tranche table C row) | C wave count is 6 today; per V1-FOLD §5 the wave content grows (DK13 + GADT substrate + row poly + schema miner + CHR + Backend surface) but the wave count is unchanged. |
| F2.M2 | `MASTER-PLAN.md:308-328` (Tranche C section) | C.W1 absorbs DK13 (rank-1 → DK13 algorithmic completeness per Lock 4 amendment); C.W2 absorbs schema miner sibling; C.W4 absorbs Backend trait obligations cross-reference. |

## §G — Cookbook §25 + carry ledger §24 (path! + format())

| Site | File:line | Surgery |
|---|---|---|
| G1.M1 | `MASTER-PLAN.md:799` (cookbook §25 row 1, see also A1.M6) | rewrite cookbook row: target user same; mental model invokes `path!(Json, ["a", "b", 0])` and `select!(Json, "...")` macros; artefact is `cookbook/path-dsl.md`; diagnostics are `BBNF-PATH-UNKNOWN-SEGMENT`, `BBNF-PATH-GRAMMAR-MISMATCH`. Cite Phase 7.1 grammar amendments via cross-reference to PASS-1 §6 + ARCH §8. |
| G2.M1 | `MASTER-PLAN.md:798-806` (cookbook §25 fresh row) | add new cookbook row: friction = "public `format()` method on generated runtimes"; user mental model = "format reads `@layout` + `@pretty` metadata produced by the grammar; dispatch is metadata-driven, not authored at the call site"; artefact = `cookbook/format.md`; diagnostic = none (formatting is total over admitted documents). |

## §H — Out-of-scope (sister-agent owned)

| Surface | Owner |
|---|---|
| `restart/ARCHITECTURE.md` | Phase 7.1 owned (already landed) |
| `restart/locks/14-LOCKS.md` | Phase 7.1 owned (already landed) |
| `restart/audit/pass-1-substrate/PASS-1.md` | Phase 7.2 PASS-1-fold sister agent |
| `restart/audit/pass-2-codegen/PASS-2.md` | Phase 7.2 PASS-2-fold sister agent |
| `restart/audit/pass-3-runtime/PASS-3.md` | Phase 7.2 PASS-3-fold sister agent |

## §I — Verification commands

```sh
rg -n 'pointer!' restart/MASTER-PLAN.md restart/MIGRATION.md
rg -n 'path!' restart/MASTER-PLAN.md restart/MIGRATION.md
rg -n 'BBNF-POINTER' restart/MASTER-PLAN.md restart/MIGRATION.md
rg -n 'parse-that-regex' restart/MASTER-PLAN.md restart/MIGRATION.md
rg -n 'bbnf-regex|regex-automata' restart/MASTER-PLAN.md restart/MIGRATION.md
rg -n 'BA\.W|BD\.W|BC\.W' restart/MASTER-PLAN.md restart/MIGRATION.md
rg -n 'D\.W6' restart/MASTER-PLAN.md
rg -n 'path-ts' restart/MASTER-PLAN.md
rg -n 'WasmBackend|TsBackend' restart/MASTER-PLAN.md
```

Expected post-fold:
- `pointer!` → zero positive surface in MASTER-PLAN + MIGRATION
- `path!` → 6+ positive citations
- `BBNF-POINTER` → zero (renamed to `BBNF-PATH`)
- `parse-that-regex` → 5+ positive citations (replaces `bbnf-regex` + `regex-automata` oracle)
- `bbnf-regex|regex-automata` → zero (or archaeology only)
- `BA.W|BD.W|BC.W` → archaeology only (BA-BD inheritance map references)
- `D.W5` → positive (new function-surface wave landed; wave count grew 5 → 6)
- `path-ts` → only as deferred post-V1
- `WasmBackend|TsBackend` → only as deferred V2 references via ARCH §7.5 cross-citation
