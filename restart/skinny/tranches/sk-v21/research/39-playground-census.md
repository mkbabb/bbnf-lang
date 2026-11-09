# 39 — Playground census (SK-V21 post-freeze greenfield)

Read-only pass at charter HEAD. Evidence is `file:line`. Prior disposition of the
uncommitted diff is `research/16-head-delta-reverify.md` tail (items reproduced/extended below).

## 1. What the playground IS

A **Vue 3 + Vite 6 + TypeScript SPA**, ~**10,247 LOC** across `.vue/.ts/.css` under
`playground/src` (`find … | xargs wc -l` → 10247 total). Stack (`playground/package.json:12-38`):
Vue 3.5, vue-router 4.6, Vite 6, Tailwind 4 (`@tailwindcss/vite`), Monaco editor 0.52,
reka-ui 2, `@mkbabb/glass-ui@3` / `keyframes.js@2.2` / `value.js@0.10`, Playwright e2e.

Entry chain: `index.html` → `src/main.ts` → `src/App.vue` → router (`src/router/index.ts`)
with three routes: **LandingPage**, **PlaygroundPage**, **DocsPage** (`src/views/`).

What it renders / does:
- **Landing** — marketing hero with typewriter + code-card fan, DemoCards, FeatureCards,
  LivePreviewStrip (`src/components/landing/*`, `useHeroSequence.ts`).
- **Playground** — split-pane live editor (`src/views/playground/`): two Monaco editors
  (grammar + input), live **parse → AST JSON tab + gorgeous-formatted output**, entry-rule
  selector, formatter settings (maxWidth/indent/tabs), parse/format telemetry
  (`usePipeline.ts:14-19,250-253`). Examples: bbnf, css, google-sheets, hello, json, math
  (`src/composables/examples/`).
- **Docs** — bench charts, flowcharts, runnable code (`src/components/docs/*`).
- **Debug** — WASM debug-snapshot stepping (`src/components/debug/`, `useDebugSession.ts`).

Net: it is a **live-parse + gorgeous-formatting demo with an LSP-backed grammar editor**, not
a mascot/animation showpiece.

## 2. How it CONNECTS to the repo

- **WASM (the only engine edge).** `src/composables/wasm/loader.ts:12` dynamic-imports
  `../../wasm/bbnf_wasm`; artifacts are **committed prebuilt** at `src/wasm/bbnf_wasm_bg.wasm`
  + `.js` + `.d.ts` (force-added despite `src/wasm/.gitignore` = `*`; `git ls-files` lists them).
- **Producer crate = top-level `wasm/`** (`wasm/Cargo.toml`, name `bbnf-wasm`), built by
  **wasm-pack** (`[package.metadata.wasm-pack.profile.release] wasm-opt=["-O3","--enable-simd"]`;
  `wasm/pkg-node/` present). **No wasm-pack step is wired into `vite.config.ts`/`package.json`**
  — the `pkg` output is hand-copied into `src/wasm/`.
- **`wasm/` is EXCLUDED from the workspace** (`/Cargo.toml:3` `exclude = ["wasm"]`; own
  `wasm/Cargo.lock`). Its deps are all **legacy `crates/` tree** (`wasm/Cargo.toml:12-16`):
  `bbnf-analysis`→`crates/analysis`, `bbnf`→`crates/core`, `bbnf-ir`→`crates/ir`,
  `gorgeous`, `pprint 0.3`, `parse_that 0.4`, `ls-types`.
- **WASM API surface** (`wasm/src/`): `analysis.rs`, `gorgeous.rs` (5 fns), `lsp.rs` (~18 fns),
  `vm.rs` (compile/parse/parseCheck grammar-handle bytecode VM). Consumed by
  `src/composables/wasm/index.ts` (324 LOC) + `types.ts` (189 LOC); driven by
  `usePipeline.ts:124-233` (analyzeGrammar → compileGrammar → parseCheck/parseWithGrammar →
  formatWithGorgeous/formatWithGrammar).
- **npm `@mkbabb/*`** — keyframes.js/value.js/glass-ui are animation+design only
  (`useScrollMorph.ts:2-3`, `useHeroSequence.ts:2-3`). **No parse-that TS package** is imported.

## 3. STALENESS

- **Last touched**: `9278fe9d0 2026-06-02` (glass-ui 3.0.0 / keyframes 2.2.0 dep bump); last
  real code `09a0baa46 2026-04-17`; **WASM binary rebuilt `9f0142e5c 2026-03-30`** ("SIMD128 +
  direct JsValue"). The shipped `.wasm` predates the entire restart/skinny campaign.
- **Points at deleted stack.** The wasm crate targets the **legacy `crates/` tree**
  (core/analysis/ir/gorgeous). SK-V21 §3 "stack abrogation" **retires the legacy root `crates/`
  tree with the cutover** (`SPEC.md:207-219`). **`skinny/` ships zero wasm bindings**
  (`grep wasm_bindgen skinny` → none; no `*wasm*` file under `skinny/`). The playground is thus
  architecturally **disconnected from the SK-V21 cutover target**.
- **Uncommitted diff** (extends 16-tail): `vite.config.ts` removes a **redundant** keyframes.js
  `resolve.alias` (package still resolves via its own `exports`; safe cleanup, unverified-green).
  `test-results/.last-run.json` = Playwright artifact `{"status":"passed"}`, 4K, untracked,
  **not gitignored → pure junk**.

## 4. GAP LIST — showcasing the frozen novel feature set

- **Grammar-derived SIMD parse**: not surfaced. Playground calls the **legacy bytecode VM**
  (`vm.rs`), not the skinny SIMD-codegen path. Would need a **new wasm crate over the skinny
  workspace** (none exists today).
- **Typed value plane**: AST is rendered as generic `JSON.stringify(result.value)`
  (`usePipeline.ts:276`) — no typed-value-plane view.
- **TOML / fleet grammars**: **absent as grammars**. "TOML" appears only as a hero marketing
  chip + markdown highlight (`HeroSection.vue:17`, `markdown.ts:80`). No TOML/fleet example;
  examples are bbnf/css/google-sheets/hello/json/math only.
- **gorgeous**: IS demoed (`formatWithGorgeous`) but via the **legacy** gorgeous crate.
- Umbrella gap: a **skinny-backed wasm binding + rebuilt artifact** is the prerequisite for all
  four; without it the playground can showcase none of the cutover's novelty.

## 5. SPEC ownership

**No TW0-TW6 wave owns the playground for feature/showcase work.** Every SPEC mention is
dispositional: **W0.10** records seams — "playground junk **ignored** (16 tail)"
(`SPEC.md:566`, `:668`), repeated "playground ignore; LAST" (`SPEC.md:788`). The playground is
explicitly **out of scope**; the frozen plan's only action is to ignore the uncommitted junk.

## bbnf-buddy (mascot)

**Not present in the live repo.** No `bbnf-buddy`/`mascot` component under `playground/src`
(grep → 0); the only "mascot" hits are historical audit docs under `restart-archive-2026-05-04/`.
The playground's brand surface is a **static wordmark** `BbnfLogo.vue` (text + `bbnf-icon.png`)
and `HeaderRibbon.vue` — **no procedural/animated SVG mascot exists**. Memory's "bbnf-buddy
procedural SVG mascot" never materialized in this tree.
