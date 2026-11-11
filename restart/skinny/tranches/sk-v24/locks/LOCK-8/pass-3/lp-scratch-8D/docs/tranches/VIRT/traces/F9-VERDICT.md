# F-9 VERDICT — the words virtual-copy disposition

**Fork:** F-9 (§5.2 of `SPEC-PASS-3.md`) — is
`words/frontend/src/composables/virtual/` consumed **document-shaped** (→ delete
everywhere, words migrates to the native core VR-50) or **feed/grid-shaped** (→ the
windower SURVIVES at the glass-ui root, words is its first external consumer)?

**Verdict: BRANCH (b) — SURVIVE-AT-ROOT.** Every consumer is feed/grid-shaped. Sets
**VR-57 = *survive-at-root*** and pins **VR-71** (see §4 below — the semver corner is
sharper than the pass-2 leaning assumed).

**Method:** one clean read pass, no code changes. Exhaustive path-qualified importer
sweep (`.ts`/`.vue`/`.tsx`/`.js`) over `words/frontend/src`; every importer read in full
and classified against the §5.2 criteria. Machine evidence: `file:line` quotes
throughout.

---

## 1. The consumer table (file → import → classification → branch)

Exhaustive — these are ALL importers of the copy outside its own barrel
(`src/composables/virtual/index.ts`); no test file imports it:

| # | Consumer file | Imported symbol | What it windows | Shape | Branch |
|---|---------------|-----------------|-----------------|-------|--------|
| 1 | `components/custom/definition/components/content/DefinitionContentView.vue:162` | `useVirtualSectionWindow` | the definition **clusters** of ONE looked-up word (result cards) | **feed/grid** | (b) |
| 2 | `components/custom/definition/composables/flattenDefinitions.ts:1,4` | `type FlatSection` | type base for `FlatDefinitionCluster` (a definition cluster) | **feed/grid** (contract for #1) | (b) |
| 3 | `stores/search/modes/wordlist.ts:20,90` | `useWindowedStore` | the paginated **wordlist search-results feed** | **feed/grid** | (b) |
| 4 | `components/custom/definition/components/ThesaurusView.vue:195,219` | `useVirtualGrid` | the **synonym grid** (N-column `@tanstack/vue-virtual`) | **feed/grid** | (b) — stays words-local |

**Zero consumers are document-shaped.** No consumer of this copy renders a contiguous
prose document; not one participates in the VR-4..9 document semantics.

### Per-consumer classification evidence

**#1 `DefinitionContentView.vue` — the pivotal consumer (the section windower itself).**
This is the ONLY consumer of `useVirtualSectionWindow` — the twin whose shape decides the
fork. It windows the definition **clusters** of a single looked-up word, rendered as
result cards inside a `CardContent` (`DefinitionContentView.vue:2,18-60`):

- Spacer-div JS windowing, only a subset of clusters in the DOM:
  `<div :style="{ height: topSpacerPx + 'px' }" />` … `v-for="… in visibleItems"` …
  `<div :style="{ height: bottomSpacerPx + 'px' }" />` (`:15,18,63`). The other clusters
  are NOT in the DOM.
- **NO `content-visibility`, NO `contain-intrinsic-size`, NO `window.print`, NO
  `beforeprint`, NO `hidden=until-found`, NO find-in-page path, NO `:~:text=` /
  `location.hash` fragment nav, NO reader-mode probe** anywhere in
  `src/components/custom/definition` (grep-verified: the sole `scrollIntoView` hit is
  `TimeMachineTimeline.vue:97`, an unrelated horizontal dot-strip). This is the exact JS
  windowing anti-pattern the VIRT tranche retires for DOCUMENTS — but here the content is
  a **collection of result cards**, not a prose document. The document-semantics
  (find/print/reader/fragment) that would make it document-shaped simply do not apply and
  are not wired.
- Nav is provide/inject `ensureTargetWindow` into `ProgressiveSidebar` for cluster
  jumps (`:244`; `ProgressiveSidebar.vue:63,73,88`) — a windowed-collection jump primitive
  ("force item into the render window before scrolling"), not a document fragment/anchor.
- The `FlatDefinitionCluster extends FlatSection` node is `depth:0, parentId:null`
  flat — clusters, not a nested document tree
  (`flattenDefinitions.ts:4,17-27`).

→ **feed/grid-shaped.** A bounded collection of result cards windowed with spacers.

**#2 `flattenDefinitions.ts`** — imports only `type FlatSection` to type
`FlatDefinitionCluster` (`:1,4`). It is the data contract for #1; same shape (a cluster
collection). No independent shape.

**#3 `wordlist.ts`** — `useWindowedStore<WordListItem>({ maxResident: 500 })` (`:90`).
This is unambiguously a **paginated infinite-scroll results feed**: a sliding resident
window over the full wordlist with forward `appendIfCurrent` + backward `prepend` +
front/back eviction + a generation-counter stale-append race guard
(`:276-312`; `useWindowedStore.ts:42-91`). A feed store primitive; the VR-4..9 prose
semantics are meaningless for it.

**#4 `ThesaurusView.vue`** — `useVirtualGrid<SynonymData>` over a reactive N-column
synonym grid backed by `@tanstack/vue-virtual`'s `useWindowVirtualizer`
(`:219-224`; `useVirtualGrid.ts:2,59-65`). A grid windower — feed/grid by construction.

**Consequence for the §5.2 decision rule:** "**(b) ANY consumer feed/grid-shaped**". Here
**ALL** consumers are feed/grid-shaped. The document-native core (VR-50) is the wrong
migration target for this copy — none of these consumers wants find-in-page, print,
reader-mode, or fragment nav over a prose document. Branch (b) fires unambiguously.

---

## 2. THE STATE-OF-DISK CORRECTION (supersedes the pass-2 §0/§5.2 premise)

§5.2's framing assumed glass-ui `./virtual` = the *document-native core*, so branch (b)
would "generalize" that core for words. **That is not the disk truth.** The read found:

- **glass-ui 4.2.0 `./virtual` ALREADY SHIPS the windower family**, not a document core.
  `glass-ui/src/composables/virtual/index.ts:9-19` exports
  `useVirtualSectionWindow` + `clearSessionHeightCache` + `useWindowedStore` +
  `buildSectionLayout`/`findSectionOffset`/`resolveActiveSection`/`resolveSectionWindow`
  + the `FlatSection`/`SectionLayout`/`SectionWindowRange`/`ForcedSectionWindowRange`
  types. Confirmed live in `glass-ui/package.json:375-378` (`"./virtual"` → `dist/virtual.js`).
- **The windower was re-minted at tranche BC BECAUSE OF the words consumers.**
  `glass-ui/MIGRATION.md:604-625` (§3.2): "REMOVED in v1.0; **REVERSED-at-BC**… two
  binary consumers now overturn the no-consumer verdict — **the live words
  `DefinitionContentView` consumer** + the booked `BC.W-DOCK-SEARCH` results list". §3.3
  (`:627-638`): the `useWindowedStore` reversal is driven by "the live words
  `wordlist.ts` store". The grid: "`useVirtualGrid`… **STAYS words-local** — one consumer
  + a hard 3rd-party dep, the ≥2-consumer bar fails for glass-ui" (`:620-622`).

So branch (b) is not just correct — **it is ALREADY HALF-EXECUTED on disk.** The windower
already survives at the glass-ui root (`./virtual`), and words is already recorded as its
motivating consumer. What remains is the mechanical closure: words deletes its stale local
copy and imports the re-homed engine.

---

## 3. words fork divergence vs the glass-ui core (migration cost — enumerated)

The words copy is the **older transposed snapshot**; the glass-ui core is byte-faithful to
it **save three recorded refinements** (`MIGRATION.md:614-617`). Machine-verified diff
(`diff glass-ui/src/composables/virtual/<f> words/frontend/src/composables/virtual/<f>`):

| file | glass-ui LOC | changed lines vs words | nature of divergence |
|------|-------------|------------------------|----------------------|
| `useVirtualSectionWindow.ts` | 357 | 108 | ResizeObservers route through the house `useResizeObserver` leaf (VR-56) instead of raw `new ResizeObserver`; `clearSessionHeightCache` added; doc/lineage header |
| `virtualSectionLayout.ts` | 263 | 77 | `FlatSection extends SectionHierarchy` (from `sidebar/types`) vs the words copy's inlined `depth`/`parentId`/`rootId`/`rootIndex` fields; binary-search `findSectionOffset` (words linear-scans); doc header |
| `useWindowedStore.ts` | 106 | 25 | doc/lineage header only — logic byte-identical (the generation-counter race guard preserved verbatim) |

**The divergences are strict IMPROVEMENTS in glass-ui and shape-compatible with the words
usage — migration is net-deletion, not a rewrite.** Enumerated per words call-site:

1. **`DefinitionContentView.vue:162,223-241`** — change import
   `'@/composables/virtual'` → `'@mkbabb/glass-ui/virtual'`. The call
   (`useVirtualSectionWindow({ items, scrollContainer, contentEl, overscanBeforePx,
   overscanAfterPx })`, `:229-234`) is signature-unchanged. The `as any`/`as unknown as`
   casts at `:230-235` (worked around a cross-package `RefSymbol` mismatch) get REVISITED
   under VR-76 (the `ElementRef` structural-ref surface rule) — the migration is the
   moment those casts can go, glass-ui side. **Cost: 1 import line + a cast cleanup.**
2. **`flattenDefinitions.ts:1`** — change `type FlatSection` import to
   `'@mkbabb/glass-ui/virtual'`. The glass-ui `FlatSection extends SectionHierarchy` carries
   the SAME four hierarchy fields words sets (`depth/parentId/rootId/rootIndex`,
   `flattenDefinitions.ts:20-24`); structurally identical, so `FlatDefinitionCluster
   extends FlatSection` still compiles. words has NO local `SectionHierarchy`/`TreeIndexEntry`
   (grep-empty), so the type-reconcile is glass-ui-internal — zero words-side churn beyond
   the import path. **Cost: 1 import line.**
3. **`wordlist.ts:20`** — change `useWindowedStore` import to `'@mkbabb/glass-ui/virtual'`.
   Logic byte-identical; the `computed`-rewrap of the returned ShallowRefs (`:93-95`) is
   unaffected. **Cost: 1 import line.**
4. **`ThesaurusView.vue:195` (`useVirtualGrid`)** — **NO migration.** Stays words-local
   (the tanstack grid, one consumer + hard 3rd-party dep; `MIGRATION.md:620-622`). It
   remains in `words/frontend/src/composables/virtual/` as the ONLY surviving file there.
5. **words `package.json`** — glass-ui is pinned `^3.0.0` (`words/frontend/package.json`);
   consuming `./virtual` requires bumping to the F-9 cut (5.0.0, §4). words does NOT call
   `clearSessionHeightCache` (grep-empty) → the additive export costs words nothing.

**Total migration cost for words: 3 import-path edits + a version bump + one glass-ui-side
cast cleanup; then DELETE `useVirtualSectionWindow.ts`, `useWindowedStore.ts`,
`virtualSectionLayout.ts` from the words copy and trim the barrel to re-export only
`useVirtualGrid`.** Net LOC: deletes 726 lines (the three windower files), keeps 116
(the grid). This is the W5 "words: the F-9 branch executed" work item.

---

## 4. VR-71 semver — the value pins (mechanical rule applied to the disk truth)

VR-71 (`SPEC-PASS-3.md:447-455`): "if the shipped cut removes OR signature-breaks any
currently-published `./virtual` export relative to 4.2.0, the cut is MAJOR (5.0.0); only a
purely-additive surface is minor (4.3.0)."

Apply to the disk truth (§2 — `./virtual` today = the windower family):

- The VIRT W2 cut **adds** the document-native core (`useSectionViewport`,
  `resolveActivePosition`, the epoch gate, ElementRef contract, peer-meta) to `./virtual`.
- The windower family (`useVirtualSectionWindow`/`useWindowedStore`/`virtualSectionLayout`)
  is **KEPT** (branch (b) — words survives-at-root) and does NOT signature-break for its
  words consumers (§3: same call signatures).

→ **If the W2 cut lands the document core purely-additively alongside the retained
windower — with NO removal or signature-break of the currently-published windower exports —
the mechanical rule permits the 4.3.0 minor corner.** This is a REAL corner here (unlike
the pass-2 leaning, which assumed the windower would be REMOVED and forced 5.0.0). Two
gates decide which value lands, and they are W2-verifiable, not open forks:

1. **Peer-meta (VR-51)** flips 5 mandatory peers → optional in `package.json`. Making a
   mandatory peerDependency optional is additive-compatible (loosening a constraint), so it
   does NOT force major on its own.
2. **The VR-56 `useResizeObserver` re-home + any `FlatSection extends SectionHierarchy`
   reshaping** already shipped in the *current* 4.2.0 `./virtual` (§3) — they are NOT part
   of the W2 delta, so they do not force major either.

**Pinned value: 4.3.0 (minor) is achievable and is the pre-registered target — IF and
ONLY IF the W2 substrate lands additive over the retained windower exports with zero
removal/signature-break.** If W2 instead chooses to REMOVE the standalone windower (folding
it into the document core) or re-signature any published `./virtual` export, the rule fires
5.0.0. **This is a W2 IMPLEMENTATION CHOICE, now made explicit and mechanical — not a
lingering fork.** Recommendation carried to W2: land additive (retain the windower exports
byte-compatible, add the core beside them) → **4.3.0**; latex-paper is v0.3.0 minor either
way (VR-31).

> **Amendment to VR-71/VR-57 wording:** the pass-2/§5.2 leaning said "5.0.0" on the premise
> that branch (b) *generalizes/breaks* the published windower export. The disk truth is that
> the windower is ALREADY published unchanged and words consumes it unchanged, so **branch
> (b)'s natural landing is the 4.3.0 additive corner**, not 5.0.0. 5.0.0 is now the
> *contingent* value (only if W2 elects a removal/signature-break), not the default.

---

## 5. Gate consequences (both close on this verdict)

- **VR-57** flips `OPEN → SETTLED (survive-at-root)`.
- **VR-71** value pins: **4.3.0 (additive corner, the pre-registered target)**, with 5.0.0
  as the contingent value if W2 removes/signature-breaks a published `./virtual` export.
- **VR-72 twin-deletion gate** closes for the windower family under branch (b): the three
  windower files exist in **EXACTLY ONE repo** (glass-ui `src/composables/virtual/`) once
  words deletes its copy — the words copy is stale-and-deletes; latex-paper/fourier never
  held them. Path-qualified grep target for the W5/W6 dry-run:
  `src/composables/{virtual,sidebar}/` across {glass-ui, latex-paper, fourier, words}
  must show `useVirtualSectionWindow.ts`/`useWindowedStore.ts`/`virtualSectionLayout.ts`
  in glass-ui ONLY. `useVirtualGrid.ts` stays words-local (excluded from the twin gate —
  it is not a twin, it has no glass-ui counterpart).
- **Dismissal D-1 stands:** F-9 was a branch selector, not an architecture risk. The
  branch (b) machinery is already on disk (glass-ui BC re-mint); this pass only records the
  classification + the semver correction and books the words deletion into W5.

---

## Evidence pointers (all machine-verified this pass)

- Words importer sweep (exhaustive, 4 hits, 0 tests): grep over `words/frontend/src`.
- Consumer reads: `DefinitionContentView.vue` (full), `wordlist.ts` (full),
  `ThesaurusView.vue:185-224`, `flattenDefinitions.ts` (full),
  `ProgressiveSidebar.vue:55-114`, `constants/index.ts:7`.
- glass-ui state: `glass-ui/src/composables/virtual/index.ts:1-26`,
  `glass-ui/package.json:73-74,375-378`, `glass-ui/MIGRATION.md:604-638`.
- Divergence diffs: `diff glass-ui/…/<f> words/…/<f>` — 108/77/25 changed lines
  (SectionWindow/layout/WindowedStore).
