# research/41 — Novel-feature surface census (SK-V21 post-freeze greenfield)

Date: 2026-07-06. HEAD: `05302109d` (no code since SK-V20 close). Read-only pass. Governing plan:
`restart/skinny/tranches/sk-v21/SPEC.md` (FROZEN 2026-07-05), §11 wave manifest
TW0→TW-S→TW1a→TW1b→TW1c→TW2→TW4.1→{TW3∥TW4.2}→TW5→TW6.

Method: enumerate each differentiating/novel surface, its code state at HEAD (file:line), whether
the frozen SPEC owns it (named wave/§) or leaves it unowned, and a
KEEP-AND-SHOWCASE / KEEP-DORMANT / PRUNE verdict. **The load-bearing finding is the UNOWNED set**
(§3): the entire debug/VM/LSP/DAP constellation survives at HEAD yet appears in zero wave.

---

## §1 — The eight surfaces, disposed

### (1) Core novelty — grammar-derived block-wide SIMD structural classification + typed value plane over ONE tape
- **Code state**: LIVE and flagship. `crates/simd-scan/src/neon.rs` — `vshrn_n_u16 #4` movemask to a
  64-bit per-chunk lane, `scan_structural`, singleton/digraph structural classification
  (`neon.rs:14,46,154`); alphabet-driven kernel selection `crates/simd-scan/src/alphabet.rs`
  (`KernelShape::select` ladder 1..=8 / 9..=16 / _). The ONE tape = the AoS `Vec<TapeRec>` write
  path (TW1b's hardening target). Typed value plane = the `->`-projection descent
  (`general_descent`, Sheets emits 31 projections).
- **SPEC ownership**: OWNED throughout — §2 (perf thesis, AU floor met), §4 (TW1b AoS tape), §5
  (TW4.1/4.2 kernel plane + KernelCostTable), §9 (the narrow novelty claim: "first GENERATOR to
  emit grammar-derived block-wide SIMD structural classification with a typed value plane" —
  verified still empty as of July 2026, SPEC.md:508-510).
- **Verdict: KEEP-AND-SHOWCASE.** This IS the differentiator. Every showcase headline binds here.

### (2) Language surface
- **`->` arrow projections** — LIVE; carried/materialized for all grammars via `general_descent`
  (P7's "BREAK 1" refuted, SPEC.md:471-473). Owned: §6 (`typed_materialization_invariant`, TW5),
  §9. **KEEP-AND-SHOWCASE.**
- **`?w` whitespace policy** — declared in grammars; at HEAD the emitter hardcodes skip-whitespace
  (WALL A, `emit.rs:126-135`). SPEC OWNS the fix: §9 / TW6 threads `WsPolicy` (`WsClass::inline()`)
  through `lower_program`. **KEEP-AND-SHOWCASE** (the TOML/newline-significant enabler).
- **`@import` grammar modules** — LIVE; consumed in `crates/core/src/pipeline/directives.rs`,
  `pipeline/compile/mod.rs`; used by bbnf.bbnf self-host (expressions.bbnf split). SPEC has NO
  explicit row — foundational-but-unnamed. **KEEP-DORMANT** (works; unowned; flag in §3).
- **`@host` / closures / GrammarCall / multi-param fns** — code present:
  `crates/core/src/pipeline/compile/closure_partition.rs` (212 LOC), `grammar/host.rs`, TS emitter
  grammar path. Research: ZERO production `@host fn` consumers; lowering drops `FnCall` args
  (unifier param edge unreachable). SPEC OWNS via §6/§13 N13: `@host` surface + HostSigTable +
  falsifier grammar land IN TW5; the **wider closure/GrammarCall surface is TW6 land-or-delete**.
  **KEEP-DORMANT (@host → TW5 activation) / PRUNE-unless-consumer (closures/GrammarCall → TW6).**
- **Self-hosting**: `grammar/bbnf/bbnf.bbnf` LIVE — round-trips to AST, self-host parity test
  (`crates/core/tests/bbnf_parity.rs:21-35`), fleet grammar #1, in the 9-grammar regen manifest.
  **KEEP-AND-SHOWCASE.** `grammar/misc/regex.bbnf` EXISTS but is NOT in the workspace manifest
  grammars (bbnf/json/css_l4/css_pretty/google_sheets/ebnf/bnf/csv/math). **UNOWNED / KEEP-DORMANT.**

### (3) Debug infrastructure — @debug, source maps, DebugBreak, interpreter hooks, compiled trace
- **Code state: SURVIVES at HEAD — the interpreter was NOT deleted.** The bytecode VM lives in
  `crates/ir/src/vm/` (1772 LOC): `interpreter/mod.rs:71 pub struct Interpreter`,
  `bytecode.rs:198 source_map: Vec<SourceMapEntry>`, `bytecode.rs:203 SourceMapEntry`,
  `vm/debug.rs` (DebugBreak), `vm/compiler/`. `@debug` directive is real:
  `crates/analysis/src/directives/debug.rs` (extraction, validation, semantic tokens). Consumed by
  the LSP debugger: `crates/lsp/src/dap/adapter.rs:150 Interpreter::new(...)`. Tests:
  `crates/ir/tests/vm/debug.rs` (375 LOC), `crates/core/tests/debug.rs`.
- **Nuance**: SPEC §2's "no interpreter" refers to the HOT JSON parse path (AW fn-per-rule RD
  prototype). The bytecode VM is a SEPARATE interpretation engine powering pretty/debug/LSP — it is
  not on the hot parse path and was never deleted.
- **SPEC ownership: UNOWNED.** Zero SPEC mention of `@debug`, DAP, source maps, or the bytecode VM
  (grep of SPEC.md: "interpreter" appears only as "no interpreter" and "bounded regex interpreter" —
  a different WALL-B matcher). No TW wave touches this constellation.
- **Verdict: KEEP-DORMANT.** Functional, orthogonal to the TW manifest, unowned. Showcase-adjacent
  (a grammar-derived source-mapped debugger is a genuine novelty) but the frozen plan neither
  showcases nor prunes it — a census gap.

### (4) gorgeous/pprint + fused parse+format pipeline (the 87x claim)
- **Code state**: `crates/gorgeous/` 604 LOC (bbnf/bnf/css/ebnf/google_sheets/json + vm.rs). The
  mainline pretty path (`gorgeous/src/vm.rs`) is a **runtime interpreter over the bytecode VM's
  `Value` tree** (`use bbnf_ir::interpreter::Value`) consulting `@pretty` directives — NOT a fused
  single-pass at HEAD. Functions labeled "fused parse+format pipeline" exist
  (`css.rs:10`, `json.rs:7`) but wrap the VM-interpreted path.
- **The 87x claim is NOT carried in the SPEC** (memory `fused-prettify`; zero SPEC occurrence).
- **SPEC ownership**: gorgeous appears ONLY in the Lock-12 archive decision — "ser/gorgeous Lock-12
  archive decision executed" (W0.6, SPEC.md:457,664). Disposition leans ARCHIVE, not showcase.
- **Oracle status**: gorgeous is the pretty/format oracle surface for the fleet.
- **Verdict: KEEP-DORMANT (archive-leaning per Lock-12).** The 87x fused claim is unsubstantiated
  at HEAD — the fused single-pass exists only as labeled wrappers over the VM path.

### (5) The LSP crate
- **Code state**: `crates/lsp/` LIVE — `server/` + `dap/` subtrees, `main.rs`, ~1772 LOC total.
  DAP adapter drives the bytecode VM (`dap/adapter.rs:150`). Diagnostics/hover/completion live in
  the sibling `crates/analysis` crate (`features/hover/directive.rs`, `directives/debug.rs`).
- **SPEC ownership: UNOWNED.** Zero SPEC mention of "lsp". No wave.
- **Consumers**: the DAP adapter is the sole runtime consumer of the VM `Interpreter` outside tests.
- **Verdict: KEEP-DORMANT.** Real, unowned, orthogonal to the manifest.

### (6) bbnf-path / direct-to-struct / serde bridge
- **bbnf-path**: `crates/bbnf-path/src/{lib,path_macro,registry}.rs` — the JSON-pointer-shaped path
  alphabet. SPEC OWNS: N17 rides the one-alphabet path plane at TW3; §4 collapses the four
  JSON-pointer implementations to one alphabet. **KEEP-AND-SHOWCASE** (one path alphabet, TW3).
- **direct-to-struct**: `json_sink_direct/` — the fused eager sink; also the WART-C name-coupling
  site (`json_sink_direct/mod.rs:98`) folded into `general_descent` at TW6. SPEC OWNS: §4 (fused
  SINK is the TW3 default eager lane), §9 (WART-C fold). **KEEP-AND-SHOWCASE.**
- **serde bridge**: SPEC OWNS §4 — "the serde `Deserialize` bridge rides the cursor" (TW3); facet
  runtime reflection explicitly rejected. **KEEP** (TW3 lane on the lazy cursor).
- **bbnf-path-ts**: `crates/bbnf-path-ts/` (compile/schema/template_tag/fixture) — the TS path
  runtime. DEFERRED-LEDGER §5: "TS path runtime (`bbnf-path-ts` full) rides the WASM decision; stays
  deferred." **KEEP-DORMANT (deferred).**

### (7) TS / WASM backend seam (Lock 5, post-V1)
- **Code state**: TS backend EXISTS and is WIRED — `crates/core/src/backend/ts/` (1719 LOC across
  code/alt/dispatch/projection/ws/repeat + emitter/{grammar,binary,value,leaves,mod}); reached via
  `CompileTarget::Ts` (`pipeline/compile/target.rs:58-74 TsEmitter`). WASM: `wasm/` present but
  **excluded from the workspace** (`Cargo.toml exclude = ["wasm"]`).
- **SPEC ownership: DEFERRED (owned as deferred).** DEFERRED-LEDGER §5: "TS/WASM backends — Lock 5
  (post-V1) — Stays deferred." The Emitter seam is PRESERVED by design: TW2's real `Emitter` trait
  (§7/§11) + the backend-agnostic type manifest (§6, "the backend-agnostic type manifest is the
  Rust/TS emission interface"; types are `TypeDesc::Named`, each backend resolves natively).
- **Verdict: KEEP-DORMANT.** The seam is deliberately kept; the backend is dormant/deferred. Note a
  reconcile gap: `CompileTarget::Ts` is an ACTIVE wired path, yet the ledger calls TS "deferred" —
  the frozen SPEC does not reconcile the live target-enum arm with the deferral.

### (8) value.js / parse-that TS constellation (PT-E1 diagnostics, research/17)
- **State**: parse-that RELEASED from the bbnf keep — the LR/packrat GO/NO-GO is **NO-GO, dated
  2026-07-05** (§7.1; bbnf eliminates left-recursion at compile time, `passes/lr.rs`). parse-that's
  TS 1.0.0 cut is TypeScript-only; the Rust regex tree bbnf consumes is unaffected (N7, smallbox
  count=0). PT-E1 (finish `ParseDiagnostic.expected`, per-parse diagnostics — research/17 §3) is a
  HIGH-value opportunity but is a **parse-that/value.js obligation, with no bbnf bearing**.
- **SPEC ownership**: only the outbound NO-GO letter — TW0 W0.10 records it (§7.1, §11.1 W0.10).
- **Verdict: PRUNE from the bbnf keep** (out of scope; parse-that retires the WDM tier, retains
  packrat memoization for value.js). PT-E1 is an outbound coordination opportunity, not a bbnf
  feature — no bbnf wave owns or should own it.

---

## §2 — Showcase-tier summary

| Surface | Code @HEAD | SPEC owner | Verdict |
|---|---|---|---|
| Grammar-derived block-SIMD + typed value plane / ONE tape | LIVE (neon.rs, simd-scan) | §2/§4/§5/§9 | **KEEP-AND-SHOWCASE** |
| `->` projections | LIVE (general_descent) | §6/§9 | **KEEP-AND-SHOWCASE** |
| `?w` whitespace policy | declared; fix TW6 | §9 TW6 | **KEEP-AND-SHOWCASE** |
| Self-host bbnf.bbnf | LIVE (parity test) | fleet #1 | **KEEP-AND-SHOWCASE** |
| bbnf-path one-alphabet + direct-to-struct sink | LIVE | N17/§4 TW3 | **KEEP-AND-SHOWCASE** |
| serde bridge | TW3 target | §4 | KEEP (TW3) |
| `@host` surface | present, 0 consumers | §6 N13 TW5 | KEEP-DORMANT→activate |
| closures/GrammarCall/multi-param | present, 0 consumers | §13 N13 TW6 | PRUNE-unless-consumer |
| `@import` modules | LIVE | unnamed | KEEP-DORMANT (unowned) |
| regex.bbnf | present, off-manifest | none | KEEP-DORMANT (unowned) |
| Debug infra (@debug/source map/DebugBreak/VM) | LIVE (ir/vm 1772 LOC) | none | KEEP-DORMANT (UNOWNED) |
| gorgeous/pprint + "fused" 87x | 604 LOC, VM-interpreted | Lock-12 archive | KEEP-DORMANT (archive-leaning) |
| LSP crate + DAP | LIVE (server/dap) | none | KEEP-DORMANT (UNOWNED) |
| TS backend seam | LIVE+wired (1719 LOC) | Lock 5 deferred | KEEP-DORMANT |
| WASM | present, workspace-excluded | Lock 5 deferred | KEEP-DORMANT |
| bbnf-path-ts | present | deferred-audit-5 | KEEP-DORMANT |
| value.js/parse-that PT-E1 | outbound | §7.1 NO-GO letter | PRUNE from keep |

## §3 — UNOWNED at freeze (the load-bearing gap)

Items with real, functional code at HEAD that NO frozen-SPEC wave names:
1. **The bytecode VM / interpreter** (`crates/ir/src/vm/`, 1772 LOC) — the debug/pretty/LSP engine.
   SPEC says "no interpreter" of the HOT path only; this engine survives untouched, unowned.
2. **Debug infrastructure** — `@debug` directive, `SourceMapEntry`, DebugBreak, compiled trace. Zero
   wave. A grammar-derived source-mapped debugger is showcase-grade novelty the plan neither
   showcases nor prunes.
3. **The LSP crate** (`crates/lsp/`, server + DAP). Zero SPEC mention.
4. **gorgeous's 87x fused claim** — unsubstantiated at HEAD (VM-interpreted, not fused single-pass);
   SPEC only carries a Lock-12 archive decision, not a showcase or a measured re-derivation.
5. **`@import`** — foundational, live, no explicit SPEC row.
6. **`regex.bbnf`** — off the 9-grammar manifest, no owner.
7. **Reconcile gap**: `CompileTarget::Ts` is a live wired arm while the ledger labels TS "deferred";
   the two are not reconciled in the frozen document.

These are not blockers to the frozen manifest (all orthogonal to TW1-TW6), but any "showcase the
novel surface" effort must either adopt (1)-(4) into a wave or consciously archive them — the frozen
SPEC does neither.
