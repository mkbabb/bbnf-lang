# Frontend structure evidence — bbnf-lang against Glass

Snapshot: bbnf-lang `af15f63e0` plus campaign edits; glass-ui `66294838`.
Glass had unrelated dirty documentation/prototype work and was read-only.

## Measured facts

- bbnf playground has about 10,241 authored Vue/TypeScript/CSS lines. Its largest
  organs are `lib/languageProvider.ts` (502), `assets/styles/main.css` (407),
  `composables/usePipeline.ts` (396), `editors/MonacoEditor.vue` (336),
  `composables/wasm/index.ts` (324), and `docs/DocsSidebar.vue` (317).
- Current ownership is horizontal (`components`, `composables`, `lib`, `views`) even
  when files serve only landing, docs, or playground. `lib/` contains nine unrelated
  registries/providers/theme/worker/data modules.
- The examples cycle is structural: each example imports `Example` from parent
  `useExamples`, while `useExamples` imports the examples barrel.
- `PlaygroundControls.vue` is a small pass-through composition; `useScrollMorph.ts`
  has no established consumer; `EntryRuleSelector.vue` is a real product capability
  awaiting compiler-owned entry metadata.
- Glass is large, but its useful organizing idiom is local ownership: a component
  directory contains its public components/index and adds `composables`, `styles`,
  `shaders`, or `constants` only when the feature genuinely has several organs.
  Simple components remain flat. Shared code is admitted only under an explicit
  primitive/interaction/value invariant, not because it is a helper.
- Glass also demonstrates the failure mode to avoid: very large general shared and
  composable/style taxonomies can become a second architecture. Bbnf needs a much
  smaller feature quotient, not a miniature Glass library.

## Ruling

Adopt Glass's **locality and graduated depth**, not its library-wide directory
count. Product routes own features; reusable UI stays in the packed Glass package.

```text
src/
  app/
    App.vue
    main.ts
    router.ts
    shell/
  features/
    landing/
    docs/
    playground/
      examples/
        model.ts
        catalog.ts
        json.ts
        css.ts
        ...
      editor/
      result/
      diagnostics/
      stages/
      walkthrough/
  platform/
    wasm/
  shared/
    # admitted only after two features share one proved contract
```

Within a module, child names strip the module name: `examples/model.ts`,
`platform/wasm/loader.ts`, `features/docs/sidebar.vue`. File depth grows only when
one invariant owns multiple substantial organs; a lone file does not receive a
ceremonial directory. Tests mirror this tree outside `src`.

## Initial dispositions

| Current organ | Target | Reason |
|---|---|---|
| `views/LandingPage.vue`, `components/landing/*`, landing-only hero data/motion | `features/landing` | one route-owned narrative |
| `views/DocsPage.vue`, `components/docs/*`, docs registry/markdown/search data | `features/docs` | one docs rendering/navigation invariant |
| `views/playground/*`, layout controls, editors, debug, walkthrough, examples, pipeline/query/split | `features/playground` | one interactive compiler product |
| `composables/wasm/*`, generated JS/types/binary load boundary | `platform/wasm` | artifact/coordinate/session invariant |
| `composables/examples/*` + `useExamples.ts` | `features/playground/examples/model.ts` plus catalog/data files | break type/provider cycle |
| `lib/*` | owning feature or platform | generic `lib` has no single invariant |
| custom BBNF logo | app/shared only if landing/docs/playground all consume its behavior | product identity, not generic UI |
| local HeaderRibbon | app shell pending packed Glass parity | behavior differs from Glass component |
| empty `utils.css`, unused `lib/utils.ts` | deleted | proved vacuous; build green after deletion |

## Move gates

The complete P2 graph must first resolve TypeScript/Vite/CSS/WASM/test edges. Each
move then lands with import rewrites, an external isomorphic test move, before/after
component screenshots for behavior-bearing components, and a re-extracted shard.
No compatibility barrel at the old path survives. Any new dependency edge reopens
both incident feature shards.
